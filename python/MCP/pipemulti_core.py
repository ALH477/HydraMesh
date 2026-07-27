# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Pipe Multi-Control core — parallel Pipe control bus over one quantum.

Multi-Control is a control-plane adapter over the 17-byte DeModFrame: it packs
up to three steady-state DCF-Pipe commands into a *single* 4-byte payload, so a
node can steer up to three concurrent lossless pipes per quantum on links where
quantum bandwidth is the scarce resource (acoustic / HydraModem profiles).

It is NOT a new wire format and does NOT replace classic Pipe control messages:
OPEN, large NACK/SACK, ABORT-with-reason, and final DONE still ride the original
14/8/9/5/4-byte single-session formats (pipelab_core).  Multi-Control only
carries the high-frequency steady-state ops (credit top-up, lightweight acks,
nack/abort hints).  The pipe_vectors.json certificate is untouched.

Byte-aligned layout (exactly 4 bytes, big-endian, MSB-first):
  Byte 0: [magic:2 | count:2 | flags:4]   = 0xC0 | (count<<4) | flags
          magic = 0b11 in bits 7-6  ->  byte0 >= 0xC0  (clean discriminator)
          count = 1..3 (number of populated command slots)
          flags = reserved 0 in v0.1
  Byte 1: cmd[0] = (local_idx<<6) | (opcode<<3) | (param_lsb<<2)   [bits 7-2 used, 1-0 pad]
  Byte 2: cmd[1]
  Byte 3: cmd[2]
  Higher cmd bytes MUST be zero when count < 3.

Each command slot:
  bits 7-6 : local_idx   (0..3)  Active Set member (caller-managed mapping)
  bits 5-3 : opcode      (000..110; 111 reserved -> rejected)
  bit  2   : param_lsb   (1-bit parameter; interpretation is opcode-dependent)

The 1-bit param is packed raw by the codec.  Resolving it against per-pipe
context (last_credit, two ack bases, two missing seqs) is a runtime layer and is
NOT byte-certified (like HydraPack's delta context or audio's jitter buffer).

Byte-certified across C/Rust/Python by Documentation/pipemulti_vectors.json.
The 246-vector wire certificate and pipe_vectors.json are both untouched.
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from pipelab_core import (  # for the classic-msg constants used by is_classic_pipe
    PIPE_OPEN, PIPE_CREDIT, PIPE_SACK, PIPE_NACK, PIPE_DONE, PIPE_ABORT,
    pack_open, pack_credit, pack_sack, pack_nack, pack_done, pack_abort,
)

# ── Constants ────────────────────────────────────────────────────────────────
MC_VERSION = 1
MC_PAYLOAD_LEN = 4                 # exactly one DeModFrame quantum payload
MC_MAGIC_MASK = 0xC0              # top 2 bits of byte 0
MC_MAGIC = 0xC0                   # 0b11 << 6
MC_MAX_COUNT = 3                  # one byte-aligned slot per cmd byte (3 cmd bytes)
MC_CMD_MAX = 3
MC_HEADER_BYTE = 0                # byte 0 is the header

# ── Opcodes ──────────────────────────────────────────────────────────────────
OP_NOP = 0            # 000  no operation (padding)
OP_CREDIT_DELTA = 1   # 001  add n to the pipe's credit budget
OP_ACK_CUMUL = 2      # 010  cumulative ACK up to a recent base
OP_ACK_SELECTIVE = 3  # 011  selective ACK of one additional chunk
OP_NACK_ONE = 4       # 100  NACK a single missing chunk
OP_DONE_HINT = 5      # 101  receiver believes transfer is complete
OP_ABORT_HINT = 6     # 110  request orderly abort
OP_RESERVED = 7       # 111  must NOT be sent in v0.1 (rejected on pack & unpack)

_ALL_OPS = (OP_NOP, OP_CREDIT_DELTA, OP_ACK_CUMUL, OP_ACK_SELECTIVE,
            OP_NACK_ONE, OP_DONE_HINT, OP_ABORT_HINT)
_OPCODE_NAMES = {
    OP_NOP: "NOP", OP_CREDIT_DELTA: "CREDIT_DELTA", OP_ACK_CUMUL: "ACK_CUMUL",
    OP_ACK_SELECTIVE: "ACK_SELECTIVE", OP_NACK_ONE: "NACK_ONE",
    OP_DONE_HINT: "DONE_HINT", OP_ABORT_HINT: "ABORT_HINT",
}


# ── Command slot ────────────────────────────────────────────────────────────
class Cmd:
    """One 6-bit (byte-padded) command slot.

    local_idx : Active Set member (0..3)
    opcode    : one of OP_*
    param_lsb : 1-bit parameter (raw; runtime resolves against per-pipe context)
    """

    __slots__ = ("local_idx", "opcode", "param_lsb")

    def __init__(self, local_idx, opcode, param_lsb=0):
        if not (0 <= local_idx <= 3):
            raise ValueError("local_idx must be 0..3")
        if not (0 <= opcode <= 6):
            raise ValueError(f"opcode {opcode} reserved/illegal (only 000..110 allowed)")
        if not (0 <= param_lsb <= 1):
            raise ValueError("param_lsb must be 0 or 1")
        self.local_idx = local_idx
        self.opcode = opcode
        self.param_lsb = param_lsb

    def __eq__(self, o):
        return (isinstance(o, Cmd) and self.local_idx == o.local_idx
                and self.opcode == o.opcode and self.param_lsb == o.param_lsb)

    def __repr__(self):
        return (f"Cmd(local_idx={self.local_idx}, "
                f"opcode={_OPCODE_NAMES.get(self.opcode, self.opcode)}, "
                f"param_lsb={self.param_lsb})")

    def to_dict(self):
        return {"local_idx": self.local_idx, "opcode": self.opcode,
                "opcode_name": _OPCODE_NAMES.get(self.opcode, "?"),
                "param_lsb": self.param_lsb}


def _encode_cmd(c):
    """Pack one Cmd into a byte (bits 7-2 used, 1-0 zero-pad)."""
    return ((c.local_idx & 0x03) << 6) | ((c.opcode & 0x07) << 3) | ((c.param_lsb & 0x01) << 2)


def _decode_cmd(b):
    """Inverse of _encode_cmd. Raises on reserved opcode."""
    local_idx = (b >> 6) & 0x03
    opcode = (b >> 3) & 0x07
    param_lsb = (b >> 2) & 0x01
    if opcode == OP_RESERVED:
        raise ValueError("reserved opcode 111 must not appear in v0.1")
    if b & 0x03:
        raise ValueError(f"cmd byte 0x{b:02X} has nonzero pad bits (1-0)")
    return Cmd(local_idx, opcode, param_lsb)


# ── Discriminators ──────────────────────────────────────────────────────────
def is_multicontrol(buf):
    """True iff *buf* (>=1 byte) carries a Multi-Control payload (magic = 11b)."""
    return len(buf) >= 1 and (buf[0] & MC_MAGIC_MASK) == MC_MAGIC


def is_classic_pipe(buf):
    """True iff *buf* looks like a classic Pipe control message (first byte 0..5)."""
    return len(buf) >= 1 and PIPE_OPEN <= buf[0] <= PIPE_ABORT


# ── Pack / unpack ────────────────────────────────────────────────────────────
def pack_multicontrol(cmds, flags=0):
    """Pack a list of Cmds (1..3) into exactly 4 bytes.

    count = len(cmds); higher cmd bytes are zeroed.  Raises on illegal count,
    flags, or any reserved opcode / bad pad bits inside a Cmd.
    """
    n = len(cmds)
    if not (1 <= n <= MC_MAX_COUNT):
        raise ValueError(f"count must be 1..{MC_MAX_COUNT}, got {n}")
    if not (0 <= flags <= 0x0F):
        raise ValueError("flags must fit 4 bits (0..15)")
    out = bytearray(MC_PAYLOAD_LEN)
    out[0] = MC_MAGIC | ((n & 0x03) << 4) | (flags & 0x0F)
    for i, c in enumerate(cmds):
        out[1 + i] = _encode_cmd(c)
    return bytes(out)


def unpack_multicontrol(buf):
    """Inverse of pack_multicontrol.

    Returns ``(count, flags, cmds)`` where *cmds* is a list of *count* Cmd objects.
    Raises on bad magic, reserved flags, reserved opcode, nonzero pad bits, or
    nonzero higher cmd bytes when count < 3.
    """
    if len(buf) < MC_PAYLOAD_LEN:
        raise ValueError(f"need {MC_PAYLOAD_LEN} bytes, got {len(buf)}")
    b0 = buf[0]
    if (b0 & MC_MAGIC_MASK) != MC_MAGIC:
        raise ValueError(f"bad magic (top 2 bits != 11b): 0x{b0:02X}")
    count = (b0 >> 4) & 0x03
    flags = b0 & 0x0F
    if not (1 <= count <= MC_MAX_COUNT):
        raise ValueError(f"count must be 1..{MC_MAX_COUNT}, got {count}")
    if flags != 0:
        raise ValueError("reserved flags must be 0 in v0.1")
    cmds = []
    for i in range(count):
        cmds.append(_decode_cmd(buf[1 + i]))
    # higher cmd bytes MUST be zero
    for i in range(count, MC_MAX_COUNT):
        if buf[1 + i] != 0:
            raise ValueError(f"unused cmd byte {1 + i} must be 0, got 0x{buf[1+i]:02X}")
    return (count, flags, cmds)


# ── Self-test ────────────────────────────────────────────────────────────────
def _selftest():
    # Anchor: minimal count=1 NOP -> bytes [0xD0, 0, 0, 0]
    a = pack_multicontrol([Cmd(0, OP_NOP)])
    assert a == bytes([0xD0, 0, 0, 0]), a.hex()
    assert is_multicontrol(a)
    assert not is_classic_pipe(a)
    c, fl, cmds = unpack_multicontrol(a)
    assert c == 1 and fl == 0 and cmds == [Cmd(0, OP_NOP)]
    assert a[0] >= 0xC0  # magic pin

    # Opcode sweep: all 7 legal opcodes with count=1, local_idx=1, param=1
    for op in _ALL_OPS:
        v = pack_multicontrol([Cmd(1, op, 1)])
        assert is_multicontrol(v)
        _, _, cs = unpack_multicontrol(v)
        assert cs[0] == Cmd(1, op, 1)
    # Reserved opcode rejected on pack
    try:
        Cmd(0, OP_RESERVED); assert False
    except ValueError:
        pass
    # Reserved opcode rejected on unpack (crafted byte)
    bad = bytearray([0xD0, 0b001_111_00, 0, 0])  # opcode=111
    try:
        unpack_multicontrol(bytes(bad)); assert False
    except ValueError:
        pass

    # count sweep 1..3 with mixed opcodes
    trio = [Cmd(0, OP_CREDIT_DELTA, 1), Cmd(2, OP_ACK_CUMUL, 0), Cmd(3, OP_ABORT_HINT, 1)]
    for n in (1, 2, 3):
        v = pack_multicontrol(trio[:n])
        assert len(v) == 4
        c, fl, cs = unpack_multicontrol(v)
        assert c == n and cs == trio[:n]
        # higher cmd bytes zeroed
        for i in range(n, 3):
            assert v[1 + i] == 0

    # zero-fill law: count<3 -> higher bytes zero (reject if nonzero on unpack)
    good2 = pack_multicontrol(trio[:2])
    assert good2[3] == 0
    bad2 = bytearray(good2); bad2[3] = 0x01
    try:
        unpack_multicontrol(bytes(bad2)); assert False
    except ValueError:
        pass

    # pad-bits law: nonzero pad bits in cmd byte rejected on unpack
    bad_pad = bytearray([0xD0, 0b000_000_01, 0, 0])  # pad bit 0 set
    try:
        unpack_multicontrol(bytes(bad_pad)); assert False
    except ValueError:
        pass

    # param LSB bit position: CREDIT_DELTA param=0 vs 1 differ exactly in bit 2
    p0 = pack_multicontrol([Cmd(0, OP_CREDIT_DELTA, 0)])
    p1 = pack_multicontrol([Cmd(0, OP_CREDIT_DELTA, 1)])
    assert p0[1] == ((0 << 6) | (OP_CREDIT_DELTA << 3) | (0 << 2))
    assert p1[1] == ((0 << 6) | (OP_CREDIT_DELTA << 3) | (1 << 2))
    assert (p0[1] ^ p1[1]) == 0x04

    # Discriminator vs classic Pipe + audio descriptor bytes
    classic_samples = [pack_open(1, 1000, 256, 0x12345678)[:1],
                       pack_credit(1, 100)[:1],
                       pack_sack(1, 0, b"")[:1],
                       pack_nack(1, [0])[:1],
                       pack_done(1)[:1],
                       pack_abort(1, 0)[:1]]
    for s in classic_samples:
        assert not is_multicontrol(s), s.hex()
        assert is_classic_pipe(s)
    audio_desc = bytes([124, 0xFF, 0xFF, 0xFF])  # audio CTRL frag-0 first byte = payload_len
    assert not is_multicontrol(audio_desc)
    assert is_multicontrol(bytes([0xC0]))      # top 2 bits set
    assert is_multicontrol(bytes([0xFF]))

    # round-trip identity on a representative cluster
    cluster = [Cmd(0, OP_ACK_SELECTIVE, 0), Cmd(1, OP_NACK_ONE, 1), Cmd(2, OP_DONE_HINT, 0)]
    packed = pack_multicontrol(cluster)
    assert unpack_multicontrol(packed)[2] == cluster

    print("pipemulti_core selftest: CERTIFIED")


if __name__ == "__main__":
    _selftest()