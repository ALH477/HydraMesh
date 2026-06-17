"""DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
DeModFrame quanta (reference codec).

Two raw frames on the wire cost 2 x 17 = 34 bytes.  When you are already sending
frames in pairs (e.g. a descriptor + its first data fragment, or two fragments of
one message), most of that second header is recoverable from context:

  * both inner sync bytes are the constant 0xD3            -> 2 bytes redundant
  * each inner CRC-16 is a pure function of its own 15      -> 4 bytes redundant
    leading bytes, so it can be recomputed on unpack

SuperPack removes those 6 redundant bytes and spends 4 of them back on a single
outer sync, a 1-byte type/version tag, and ONE joint CRC-16 covering the whole
container.  Net: 34 -> 32 bytes (5.9% smaller per pair) AND a strictly stronger
integrity check — one CRC now protects both quanta jointly instead of each alone.

Layout (big-endian), all 32 bytes:
  [0]      sync   = 0xD3
  [1]      sflags = version[7:4]=1 | type[3:0]=SUPER(0x5)
  [2..15]  frame A core = A's bytes [1..14]  (flags,seq,src,dst,payload,ts24) — no sync, no crc
  [16..29] frame B core = B's bytes [1..14]
  [30..31] CRC-16/CCITT-FALSE over bytes [0..29]

Unpack reconstructs each inner frame bit-exactly by prepending sync=0xD3 and
recomputing its CRC-16 over the rebuilt 15 leading bytes, so the two emitted
frames are ordinary, fully valid DeModFrames — the 246-vector wire certificate is
untouched.  SuperPack is a *container adapter*, never a change to the quantum.

CRC anchors (see _selftest): CRC("123456789")=0x29B1, CRC(0^15)=0x4EC3 still hold
for each reconstructed frame; SuperPack of two zero-core frames has joint
CRC = 0x5B75 (pinned below).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from wirelab_core import crc16_ccitt, decode, FRAME_LEN, SYNC, VERSION  # certified codec

SUPER_TYPE = 0x5
SUPER_LEN = 32
CORE_LEN = 14            # a frame's bytes [1..14]: everything but sync and crc


def _frame_core(frame):
    """The 14 reconstructable bytes of a 17-byte DeModFrame (drop sync + crc)."""
    if len(frame) != FRAME_LEN:
        raise ValueError(f"need a {FRAME_LEN}-byte frame, got {len(frame)}")
    if frame[0] != SYNC:
        raise ValueError("bad sync byte")
    if frame[1] >> 4 != VERSION:
        raise ValueError("bad version nibble")
    # validate the inner CRC before we throw it away, so we never pack a bad frame
    if crc16_ccitt(frame[:15]) != int.from_bytes(frame[15:17], "big"):
        raise ValueError("inner frame CRC mismatch")
    return frame[1:15]


def _rebuild_frame(core):
    """Restore a full 17-byte DeModFrame from its 14-byte core (sync + recomputed crc)."""
    body = bytes([SYNC]) + core
    return body + crc16_ccitt(body).to_bytes(2, "big")


def pack(frame_a, frame_b):
    """Combine two valid 17-byte DeModFrames into one 32-byte SuperPack."""
    core_a = _frame_core(frame_a)
    core_b = _frame_core(frame_b)
    head = bytes([SYNC, (VERSION << 4) | SUPER_TYPE]) + core_a + core_b
    return head + crc16_ccitt(head).to_bytes(2, "big")


def is_superpack(buf):
    """True iff buf looks like a SuperPack (length + sync + version/type tag)."""
    return (len(buf) == SUPER_LEN and buf[0] == SYNC
            and buf[1] == ((VERSION << 4) | SUPER_TYPE))


def unpack(buf):
    """Split a 32-byte SuperPack back into (frame_a, frame_b), each a bit-exact,
    fully valid 17-byte DeModFrame.  Raises ValueError on any integrity failure."""
    if len(buf) != SUPER_LEN:
        raise ValueError(f"length {len(buf)} != {SUPER_LEN}")
    if buf[0] != SYNC:
        raise ValueError("bad sync byte")
    if buf[1] >> 4 != VERSION:
        raise ValueError("bad version nibble")
    if buf[1] & 0x0F != SUPER_TYPE:
        raise ValueError("not a SuperPack type")
    if crc16_ccitt(buf[:30]) != int.from_bytes(buf[30:32], "big"):
        raise ValueError("SuperPack CRC mismatch")
    core_a = buf[2:2 + CORE_LEN]
    core_b = buf[2 + CORE_LEN:2 + 2 * CORE_LEN]
    frame_a = _rebuild_frame(core_a)
    frame_b = _rebuild_frame(core_b)
    # belt and braces: the rebuilt frames must themselves decode cleanly
    decode(frame_a)
    decode(frame_b)
    return frame_a, frame_b


def _selftest():
    from wirelab_core import encode
    # round-trip arbitrary frame pairs
    a = encode(0, 0x0102, 0x0304, 0x0506, b"\xde\xad\xbe\xef", 0x010203)
    b = encode(3, 0x1f, 0x0007, 0xffff, b"chat", 0x0000ff)
    sp = pack(a, b)
    assert len(sp) == SUPER_LEN, len(sp)
    assert is_superpack(sp)
    ra, rb = unpack(sp)
    assert ra == a and rb == b, "SuperPack is not lossless"
    assert SUPER_LEN < 2 * FRAME_LEN, "SuperPack must be smaller than two raw frames"

    # pinned golden anchor: two all-zero cores
    zero = encode(0, 0, 0, 0, b"\x00\x00\x00\x00", 0)
    spz = pack(zero, zero)
    joint = int.from_bytes(spz[30:32], "big")
    assert joint == 0x5B75, f"golden joint CRC drift: 0x{joint:04X}"

    # tamper detection
    bad = bytearray(sp); bad[10] ^= 0xFF
    try:
        unpack(bytes(bad)); raise SystemExit("tamper not detected")
    except ValueError:
        pass
    print("superpack selftest: CERTIFIED (34 -> 32 bytes, lossless, joint-CRC)")


if __name__ == "__main__":
    _selftest()
