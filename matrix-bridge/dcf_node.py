"""DCF text node — minimal UDP endpoint that carries chat messages as DeModFrame
traffic, with optional SuperPack batching.

This is the laptop-side mesh endpoint the Matrix bridge and the MCP agent server
both talk to.  It is deliberately dependency-free (stdlib socket/threading) so it
runs anywhere Python does.  Confidentiality is provided by the underlay VPN
(WireGuard/Tailscale) — DCF itself is encryption-free by design for export
compliance, so the frames on the wire are plaintext DeModFrames inside the
encrypted tunnel.

Wire path:
  send_text(text, channel)  -> dcf_text.packetize() -> [17B frames]
                            -> (optional) superpack.pack() pairs -> UDP datagrams
  recv loop                 -> superpack.unpack() if 32B else raw 17B
                            -> dcf_text.TextReassembler -> on_message(src, channel, text)
"""
import os
import socket
import sys
import threading
import time
from queue import Queue, Empty

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "python", "MCP"))
import dcf_text
import superpack


def _now_ts_us():
    return int(time.time() * 1_000_000) & 0xFFFFFF


class DcfTextNode:
    """A UDP DeModFrame text endpoint.  Thread-safe send; background receiver."""

    def __init__(self, node_id, bind_host="0.0.0.0", port=7777,
                 channel=None, use_superpack=True, accept_all_channels=False):
        self.node_id = node_id & 0xFFFF
        self.channel = dcf_text.channel_id(channel)
        self.use_superpack = use_superpack
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.sock.bind((bind_host, port))
        self.port = self.sock.getsockname()[1]
        self._peers = {}                      # name -> (host, port)
        self._packet_id = 0
        self._pid_lock = threading.Lock()
        accept = None if accept_all_channels else self.channel
        self._rx = dcf_text.TextReassembler(accept_dst=accept)
        self._running = False
        self._rx_thread = None
        self.inbox = Queue()                  # ("message", src, dst, text, flags, ts)
        self.on_message = None                # optional callback(src, dst, text, flags)

    # ── peers ────────────────────────────────────────────────────────────────
    def add_peer(self, name, host, port):
        self._peers[name] = (host, int(port))

    def list_peers(self):
        return dict(self._peers)

    def _next_packet_id(self):
        with self._pid_lock:
            pid = self._packet_id
            self._packet_id = (self._packet_id + 1) % (dcf_text.MAX_PACKET_ID + 1)
            return pid

    # ── send ─────────────────────────────────────────────────────────────────
    def send_text(self, text, channel=None, flags=0, to=None):
        """Fragment `text` into DeModFrame DATA frames and unicast to peers
        (or a single peer `to`=name).  Returns the number of datagrams sent."""
        dst = self.channel if channel is None else dcf_text.channel_id(channel)
        pid = self._next_packet_id()
        frames = dcf_text.packetize(text, pid, _now_ts_us(),
                                    src=self.node_id, dst=dst, flags=flags)
        datagrams = self._batch(frames)
        targets = ([self._peers[to]] if to else list(self._peers.values()))
        for host, port in targets:
            for dg in datagrams:
                self.sock.sendto(dg, (host, port))
        return len(datagrams) * max(1, len(targets))

    def _batch(self, frames):
        """Pair consecutive frames into 32-byte SuperPacks when enabled; a lone
        trailing frame is sent as a plain 17-byte datagram."""
        if not self.use_superpack:
            return list(frames)
        out = []
        i = 0
        while i + 1 < len(frames):
            out.append(superpack.pack(frames[i], frames[i + 1]))
            i += 2
        if i < len(frames):
            out.append(frames[i])
        return out

    # ── receive ──────────────────────────────────────────────────────────────
    def _feed(self, datagram):
        """Turn one UDP datagram into 0..2 frames and push them through reassembly."""
        if superpack.is_superpack(datagram):
            try:
                a, b = superpack.unpack(datagram)
            except ValueError:
                return
            frames = (a, b)
        else:
            frames = (datagram,)
        for f in frames:
            for ev in self._rx.push(f):
                _, _pid, ts, src, dst, text, flags = ev
                self.inbox.put(("message", src, dst, text, flags, ts))
                if self.on_message:
                    try:
                        self.on_message(src, dst, text, flags)
                    except Exception:
                        pass

    def _rx_loop(self):
        self.sock.settimeout(0.5)
        while self._running:
            try:
                data, _addr = self.sock.recvfrom(2048)
            except socket.timeout:
                continue
            except OSError:
                break
            self._feed(data)

    def start(self):
        if self._running:
            return
        self._running = True
        self._rx_thread = threading.Thread(target=self._rx_loop, daemon=True)
        self._rx_thread.start()

    def stop(self):
        self._running = False
        if self._rx_thread:
            self._rx_thread.join(timeout=1.0)
        self.sock.close()

    def poll(self, timeout=0.0):
        """Pop one inbound message tuple or return None."""
        try:
            return self.inbox.get(timeout=timeout) if timeout else self.inbox.get_nowait()
        except Empty:
            return None


def _selftest():
    """Loopback: two nodes on localhost exchange a message both raw and packed."""
    a = DcfTextNode(node_id=1, port=0, channel="lab", use_superpack=True)
    b = DcfTextNode(node_id=2, port=0, channel="lab", use_superpack=True)
    a.add_peer("b", "127.0.0.1", b.port)
    b.add_peer("a", "127.0.0.1", a.port)
    b.start()
    msg = "hello over DeModFrame + SuperPack \U0001f680 " * 4
    a.send_text(msg, to="b")
    got = None
    deadline = time.time() + 3
    while time.time() < deadline:
        ev = b.poll(timeout=0.2)
        if ev:
            got = ev
            break
    a.stop(); b.stop()
    assert got is not None, "no message received"
    assert got[3] == msg, "payload mismatch"
    assert got[1] == 1, "src mismatch"
    print("dcf_node selftest: CERTIFIED (loopback over UDP, SuperPack on)")


if __name__ == "__main__":
    _selftest()
