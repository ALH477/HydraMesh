# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Snake raw-L2 Ethernet transport (cat5e) — Python reference + CI test double.

Carries a batch of opaque 17-byte DeModFrames over raw Ethernet (AF_PACKET, a custom
EtherType per plane, no IP/UDP) — a transport *beneath* the wire quantum, so the 246-vector
wire certificate is untouched.  Frames are batched as 32-byte SuperPacks into one Ethernet
payload to cut the datagram count.  Byte-identical to hydramodem/dcf-tools/snake_l2.h.

Ethernet payload layout:  [n_frames u16 BE][ SuperPack * ceil(n/2) ]
  Each SuperPack packs two DeModFrames; an odd trailing frame is paired with a canonical
  zero DATA filler frame that the receiver discards using n_frames.

- ``L2EthTransport`` uses a real AF_PACKET socket (needs CAP_NET_RAW; raises on EPERM).
- ``L2LoopbackTransport`` is a privilege-free double over ``LoopbackMedium`` that exercises
  the SuperPack batching/unbatching in CI without CAP_NET_RAW.
"""
import os
import socket
import sys

for _mcp in (os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP"),
             os.path.join(os.path.dirname(os.path.abspath(__file__)), "MCP")):
    if os.path.isdir(_mcp):
        sys.path.insert(0, _mcp)
import wirelab_core as wire       # noqa: E402  (certified DeModFrame codec)
import superpack                  # noqa: E402  (certified SuperPack container)

from .transport import Transport, LoopbackTransport

# Custom EtherTypes (IEEE local/experimental range; no clash with real AVB 0x22F0).
ETHERTYPE_RECORD = 0x88B5         # wire A: quanta record plane
ETHERTYPE_CUE = 0x88B6            # wire B: PCM cue plane

L2_HDR = 2                        # the n_frames u16
SUPER_LEN = 32
FRAME_LEN = 17
BROADCAST_MAC = b"\xff\xff\xff\xff\xff\xff"

# The canonical zero filler frame (a valid DATA DeModFrame with all application fields 0);
# byte-identical to the C snake_l2.h filler, so a batch decodes the same in both languages.
_FILLER = wire.encode(0, 0, 0, 0, b"\x00\x00\x00\x00", 0)


def l2_capacity(mtu):
    """Number of DeModFrames that fit one Ethernet payload of the given MTU."""
    if mtu < L2_HDR:
        return 0
    return ((mtu - L2_HDR) // SUPER_LEN) * 2


def batch(frames):
    """Batch a list of 17-byte DeModFrames into one Ethernet payload (bytes)."""
    n = len(frames)
    if n > 0xFFFF:
        raise ValueError("too many frames for one batch")
    out = bytearray(n.to_bytes(2, "big"))
    for i in range(0, n, 2):
        a = frames[i]
        b = frames[i + 1] if i + 1 < n else _FILLER
        out += superpack.pack(bytes(a), bytes(b))
    return bytes(out)


def unbatch(buf):
    """Split an Ethernet payload back into a list of 17-byte DeModFrames (bit-exact)."""
    if len(buf) < L2_HDR:
        raise ValueError("short batch")
    n = int.from_bytes(buf[:2], "big")
    npairs = (n + 1) // 2
    if len(buf) < L2_HDR + npairs * SUPER_LEN:
        raise ValueError("truncated batch")
    frames = []
    off = L2_HDR
    for _ in range(npairs):
        a, b = superpack.unpack(buf[off:off + SUPER_LEN])
        off += SUPER_LEN
        frames.append(a)
        if len(frames) < n:
            frames.append(b)
    return frames


# ── real AF_PACKET transport (needs CAP_NET_RAW) ──────────────────────────────
class L2EthTransport(Transport):
    """Raw-L2 Ethernet transport over one interface + EtherType.  Each ``send`` ships one
    frame as a 1-frame batch; ``send_batch`` ships many frames in one Ethernet payload."""

    def __init__(self, name, ifname, ethertype=ETHERTYPE_RECORD, mtu=1500,
                 dst_mac=BROADCAST_MAC, **kw):
        super().__init__(name, **kw)
        self._ifname = ifname
        self._ethertype = ethertype
        self._mtu = mtu
        self._dst = bytes(dst_mac)
        self._sock = None
        self._rx_running = False
        self._rx_thread = None

    def _open(self):
        s = socket.socket(socket.AF_PACKET, socket.SOCK_DGRAM, socket.htons(self._ethertype))
        s.bind((self._ifname, self._ethertype))     # raises PermissionError without CAP_NET_RAW
        return s

    def _transmit(self, frame, dest):
        if self._sock is None:
            self._sock = self._open()
        self._sock.sendto(batch([frame]), (self._ifname, self._ethertype, 0, 0,
                                            dest or self._dst))

    def send_batch(self, frames, dest=None):
        if self._sock is None:
            self._sock = self._open()
        self._sock.sendto(batch(frames), (self._ifname, self._ethertype, 0, 0,
                                          dest or self._dst))

    def _start_recv(self):
        import threading
        self._sock = self._sock or self._open()
        self._rx_running = True
        self._rx_thread = threading.Thread(target=self._rx_loop, name=f"rx-{self.name}",
                                           daemon=True)
        self._rx_thread.start()

    def _rx_loop(self):
        self._sock.settimeout(0.3)
        while self._rx_running:
            try:
                buf, _ = self._sock.recvfrom(self._mtu)
            except socket.timeout:
                continue
            except OSError:
                break
            try:
                for f in unbatch(buf):
                    self._deliver(f, {"via": "l2eth"})
            except ValueError:
                continue

    def _stop_recv(self):
        self._rx_running = False
        if self._rx_thread:
            self._rx_thread.join(timeout=1.0)
        if self._sock:
            self._sock.close()
            self._sock = None


# ── privilege-free double (CI) ────────────────────────────────────────────────
class L2LoopbackTransport(LoopbackTransport):
    """A LoopbackTransport that round-trips every frame through the SuperPack batch codec,
    so CI exercises batch/unbatch without CAP_NET_RAW.  Models the raw-L2 wire in-process."""

    def _transmit(self, frame, dest):
        # batch a single frame, then unbatch on the way onto the shared medium — proves the
        # SuperPack container is transparent to the frame bytes.
        (recovered,) = unbatch(batch([bytes(frame)]))
        self._medium.broadcast(self, recovered)
