# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF transport layer — carry the universal unit (the 17-byte DeModFrame) over any
medium behind one interface, so a node can speak UDP, audio (PipeWire/JACK/WAV), SDR
(.cf32/SoapySDR) or a file spool, and a Bridge can relay frames between them
(see ``dcf.bridge`` and Documentation/DCF_FIELD_USE.md).

The pieces that differ wildly between media are **bandwidth and baud** — UDP is ~Mbps,
SDR ~kbps, a handheld-acoustic link ~300 baud (~37 B/s). So every transport owns a
**bounded outbound queue with a sender thread that drains at the link's own pace**: the
buffer decouples a fast source from a slow sink, ``send()`` never blocks the caller, and
on overflow a drop policy sheds bulk while keeping priority (control/mesh) frames. That
buffer is the heart of making heterogeneous shapes work.
"""
import os
import socket
import sys
import threading
import time
from abc import ABC, abstractmethod
from collections import deque

# MCP is a sibling from source (../MCP) and nested under dcf once packaged (./MCP).
for _mcp in (os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP"),
             os.path.join(os.path.dirname(os.path.abspath(__file__)), "MCP")):
    if os.path.isdir(_mcp):
        sys.path.insert(0, _mcp)
import wirelab_core as wire  # noqa: E402

from .proto import ProtoMessage, MSG_FRAME

FRAME_LEN = 17


def frame_key(frame):
    """Dedup/identity key for loop-free forwarding: (src, dst, seq, crc) of a frame."""
    try:
        d = wire.decode(bytes(frame))
        return (d["src"], d["dst"], d["seq"], d["crc"])
    except Exception:
        return (bytes(frame),)


# ── the per-transport buffer (rate decoupling) ────────────────────────────────
class OutboundQueue:
    """Bounded two-tier FIFO. Priority items (control/mesh) are drained first and are the
    last to be dropped; on overflow bulk is shed oldest-first. Pure + unit-testable."""

    def __init__(self, maxlen=256):
        self.maxlen = maxlen
        self._hi = deque()
        self._lo = deque()
        self._lock = threading.Lock()
        self.dropped = 0

    def put(self, item, priority=False):
        with self._lock:
            (self._hi if priority else self._lo).append(item)
            while len(self._hi) + len(self._lo) > self.maxlen:
                # shed bulk first; only drop priority if nothing else remains
                (self._lo if self._lo else self._hi).popleft()
                self.dropped += 1

    def get(self):
        """Pop the next item (priority first), or None if empty. Non-blocking."""
        with self._lock:
            if self._hi:
                return self._hi.popleft()
            if self._lo:
                return self._lo.popleft()
            return None

    def __len__(self):
        with self._lock:
            return len(self._hi) + len(self._lo)


# ── base transport ────────────────────────────────────────────────────────────
class Transport(ABC):
    """One medium. Subclasses implement `_transmit` (send one frame) and, if they receive,
    push frames by calling `self._deliver(frame, meta)`."""

    def __init__(self, name, rate_bps=10_000_000, queue_max=256, pace=False):
        self.name = name
        self.rate_bps = rate_bps
        self.pace = pace                    # simulate link time (file/loopback transports)
        self._q = OutboundQueue(queue_max)
        self._on_frame = None
        self._running = False
        self._sender = None
        self.sent = 0
        self.recv = 0

    # public API ----------------------------------------------------------------
    def start(self, on_frame):
        self._on_frame = on_frame
        self._running = True
        self._sender = threading.Thread(target=self._sender_loop, name=f"tx-{self.name}",
                                        daemon=True)
        self._sender.start()
        self._start_recv()

    def send(self, frame, dest=None, priority=False):
        """Enqueue a frame for transmission. Never blocks; sheds by policy if the link is
        saturated. Returns False if the queue dropped something to make room."""
        before = self._q.dropped
        self._q.put((bytes(frame), dest), priority=priority)
        return self._q.dropped == before

    def stop(self):
        self._running = False
        self._stop_recv()
        if self._sender:
            self._sender.join(timeout=2.0)

    @property
    def backlog(self):
        return len(self._q)

    @property
    def dropped(self):
        return self._q.dropped

    # internals -----------------------------------------------------------------
    def _deliver(self, frame, meta=None):
        self.recv += 1
        if self._on_frame:
            self._on_frame(bytes(frame), {"transport": self.name, **(meta or {})})

    def _sender_loop(self):
        while self._running:
            item = self._q.get()
            if item is None:
                time.sleep(0.002)
                continue
            frame, dest = item
            t0 = time.monotonic()
            try:
                self._transmit(frame, dest)
                self.sent += 1
            except Exception as e:  # pragma: no cover - link errors are per-medium
                sys.stderr.write(f"[transport {self.name}] tx error: {e}\n")
            if self.pace and self.rate_bps > 0:
                want = len(frame) * 8 / self.rate_bps
                slack = want - (time.monotonic() - t0)
                if slack > 0:
                    time.sleep(slack)

    @abstractmethod
    def _transmit(self, frame, dest):
        ...

    def _start_recv(self):
        ...

    def _stop_recv(self):
        ...


# ── loopback / in-process medium (for tests + bridging in one process) ────────
class LoopbackMedium:
    """A shared in-memory medium: every LoopbackTransport attached delivers transmitted
    frames to all *other* attached transports. Models a broadcast wire."""

    def __init__(self):
        self.ports = []

    def attach(self, t):
        self.ports.append(t)

    def broadcast(self, sender, frame):
        for p in self.ports:
            if p is not sender:
                p._deliver(frame, {"via": "loopback"})


class LoopbackTransport(Transport):
    def __init__(self, name, medium, **kw):
        super().__init__(name, **kw)
        self._medium = medium
        medium.attach(self)

    def _transmit(self, frame, dest):
        self._medium.broadcast(self, frame)


# ── file spool (store-and-forward / offline bridging) ─────────────────────────
class FileTransport(Transport):
    """Append frames to a .dcf spool (tx) and/or tail one (rx)."""

    def __init__(self, name, out_path=None, in_path=None, poll=0.2, **kw):
        super().__init__(name, **kw)
        self._out = out_path
        self._in = in_path
        self._poll = poll
        self._rx_running = False
        self._rx_thread = None

    def _transmit(self, frame, dest):
        if self._out:
            with open(self._out, "ab") as f:
                f.write(frame)

    def _start_recv(self):
        if not self._in:
            return
        self._rx_running = True
        self._rx_thread = threading.Thread(target=self._tail, name=f"rx-{self.name}",
                                          daemon=True)
        self._rx_thread.start()

    def _tail(self):
        pos = 0
        while self._rx_running:
            try:
                with open(self._in, "rb") as f:
                    f.seek(pos)
                    data = f.read()
                    pos = f.tell()
                for i in range(0, len(data) - FRAME_LEN + 1, FRAME_LEN):
                    self._deliver(data[i:i + FRAME_LEN])
            except FileNotFoundError:
                pass
            time.sleep(self._poll)

    def _stop_recv(self):
        self._rx_running = False


# ── UDP (frames wrapped in the ProtoMessage envelope, MSG_FRAME) ──────────────
class UdpTransport(Transport):
    """Carry frames as ProtoMessage(MSG_FRAME) datagrams to a set of peers."""

    def __init__(self, name="udp", bind=("0.0.0.0", 0), peers=(), rate_bps=10_000_000, **kw):
        super().__init__(name, rate_bps=rate_bps, **kw)
        self._bind = bind
        self._peers = [tuple(p) for p in peers]          # [(host, port), ...]
        self._sock = None
        self._seq = 0
        self._rx_running = False
        self._rx_thread = None

    @property
    def port(self):
        return self._sock.getsockname()[1] if self._sock else None

    def add_peer(self, host, port):
        self._peers.append((host, int(port)))

    def _ensure_sock(self):
        if self._sock is None:
            self._sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            self._sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            self._sock.bind(self._bind)
            self._sock.settimeout(0.3)

    def _transmit(self, frame, dest):
        self._ensure_sock()
        self._seq = (self._seq + 1) & 0xFFFFFFFF
        msg = ProtoMessage(MSG_FRAME, self._seq, frame).serialize()
        targets = [dest] if dest else self._peers
        for host, port in targets:
            self._sock.sendto(msg, (host, int(port)))

    def _start_recv(self):
        self._ensure_sock()
        self._rx_running = True
        self._rx_thread = threading.Thread(target=self._rx_loop, name=f"rx-{self.name}",
                                          daemon=True)
        self._rx_thread.start()

    def _rx_loop(self):
        while self._rx_running:
            try:
                data, addr = self._sock.recvfrom(65536)
            except socket.timeout:
                continue
            except OSError:
                break
            try:
                msg = ProtoMessage.deserialize(data)
            except ValueError:
                continue
            if msg.msg_type == MSG_FRAME and len(msg.payload) == FRAME_LEN:
                self._deliver(msg.payload, {"addr": addr})

    def _stop_recv(self):
        self._rx_running = False
        if self._sock:
            try:
                self._sock.close()
            except OSError:
                pass


# ── the modems (audio + SDR), imported tolerant of source/packaged layout ─────
try:
    from dcf.modem import acoustic as _acoustic, iq as _iqmod
except Exception:  # pragma: no cover - source layout
    sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "modem"))
    import acoustic as _acoustic
    import iq as _iqmod


class _DirMedium(Transport):
    """Base for the file-medium modems: tx writes one encoded file per frame into
    `out_dir`; rx tails `in_dir` for new files and decodes them. Two nodes share a pair of
    dirs (A.out == B.in) to form a link — the same model as piping WAV/.cf32 through
    pw-play/pw-record or hackrf_transfer. `out_dir` may instead be a command sink later."""

    EXT = ""

    def __init__(self, name, out_dir=None, in_dir=None, poll=0.1, rate_bps=4000, **kw):
        super().__init__(name, rate_bps=rate_bps, **kw)
        self._out, self._in, self._poll = out_dir, in_dir, poll
        self._n = 0
        self._seen = set()
        self._rx_running = False
        self._rx_thread = None
        for d in (out_dir, in_dir):
            if d:
                os.makedirs(d, exist_ok=True)

    def _transmit(self, frame, dest):
        if not self._out:
            return
        self._n += 1
        tmp = os.path.join(self._out, f".{self.name}-{self._n}{self.EXT}.tmp")
        final = os.path.join(self._out, f"{self.name}-{self._n:08d}{self.EXT}")
        self._encode_file(frame, tmp)
        os.replace(tmp, final)            # atomic publish so the reader never sees a partial

    def _start_recv(self):
        if not self._in:
            return
        self._rx_running = True
        self._rx_thread = threading.Thread(target=self._tail, name=f"rx-{self.name}", daemon=True)
        self._rx_thread.start()

    def _tail(self):
        while self._rx_running:
            try:
                files = sorted(f for f in os.listdir(self._in)
                               if f.endswith(self.EXT) and not f.startswith("."))
            except FileNotFoundError:
                files = []
            for f in files:
                if f in self._seen:
                    continue
                self._seen.add(f)
                path = os.path.join(self._in, f)
                try:
                    frame = self._decode_file(path)
                except Exception:
                    frame = None
                if frame is not None and len(frame) == FRAME_LEN:
                    self._deliver(frame, {"file": f})
            time.sleep(self._poll)

    def _stop_recv(self):
        self._rx_running = False

    def _encode_file(self, frame, path): ...
    def _decode_file(self, path): ...


class AudioTransport(_DirMedium):
    """Carry frames as voice-band AFSK WAVs (PipeWire/JACK/ALSA via pw-play/pw-record, or
    a shared dir for loopback). Reuses python/modem/acoustic.py."""

    EXT = ".wav"

    def __init__(self, name="audio", profile="handheld", fec=False, **kw):
        super().__init__(name, **kw)
        self._profile, self._fec = profile, fec

    def _encode_file(self, frame, path):
        audio, _, _ = _acoustic.frame_to_audio(frame, profile=self._profile, fec=self._fec)
        _acoustic.write_wav(path, audio)

    def _decode_file(self, path):
        audio, fs = _acoustic.read_wav(path)
        r = _acoustic.decode_audio(audio, profile=self._profile, fs=fs, fec=self._fec)
        return r[0] if r else None


class SdrTransport(_DirMedium):
    """Carry frames as complex-baseband IQ (.cf32 → SoapySDR / GNU Radio / a shared dir).
    Reuses python/modem/iq.py."""

    EXT = ".cf32"

    def __init__(self, name="sdr", mod="gfsk", sps=8, **kw):
        super().__init__(name, **kw)
        self._mod, self._sps = mod, sps

    def _encode_file(self, frame, path):
        sig = _iqmod.frame_to_iq(frame, mod=self._mod, sps=self._sps)
        _iqmod.write_cf32(path, sig)

    def _decode_file(self, path):
        sig = _iqmod.read_cf32(path)
        r = _iqmod.iq_to_frame(sig, mod=self._mod, sps=self._sps)
        return r[0] if r else None
