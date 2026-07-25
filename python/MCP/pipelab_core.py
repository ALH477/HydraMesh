# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Pipe — the certified control-plane codec for lossless bulk transfer.

DCF frames are the *control plane* for a high-throughput lossless transfer: the
17-byte wire quantum carries a small set of control messages (OPEN / CREDIT /
SACK / NACK / DONE / ABORT) that steer a separate, dumb UDP *data plane* of
numbered chunks. The intelligence — flow control, loss recovery, completion —
lives in these control messages; the data lane just streams `[session, seq,
payload]` datagrams as fast as credit allows. Loss is healed forward by the
certified DCF-FEC layer (feclab_core) and, past the FEC budget, by NACK-driven
retransmit; the transfer completes only when the whole-object checksum verifies.

This module is the byte-exact *contract* (like meshlab_core): pure pack/unpack
functions with no I/O, pinned across C (codec/demod_pipe.h), Rust
(codec/src/pipe.rs), and Python by Documentation/pipe_vectors.json. The runtime
that drives them lives in python/dcf/pipe/. The wire certificate is untouched —
these are payloads carried over ordinary frames, not a new frame format.

All ids are small non-negative integers; multi-byte fields are big-endian.
"""

PIPE_VERSION = 1

# Control message types (first payload byte).
PIPE_OPEN = 0    # sender -> receiver: begin a transfer
PIPE_CREDIT = 1  # receiver -> sender: permit N more chunks (receiver-driven flow control)
PIPE_SACK = 2    # receiver -> sender: cumulative ack base + selective bitmap
PIPE_NACK = 3    # receiver -> sender: explicit missing-chunk list (FEC-budget fallback)
PIPE_DONE = 4    # receiver -> sender: whole object verified, transfer complete
PIPE_ABORT = 5   # either side: tear the session down

# ABORT reasons (opaque to the codec; carried for diagnostics).
ABORT_CHECKSUM = 0   # whole-object checksum failed after full receipt
ABORT_TIMEOUT = 1
ABORT_POLICY = 2
ABORT_PEER = 3


def fnv1a32(data):
    """FNV-1a 32-bit hash — the whole-object checksum carried in OPEN.

    Deterministic and trivially portable (no tables), so C/Rust/Python agree
    byte-for-byte. This detects a completed transfer's integrity; it is not a
    security MAC (the wire is plaintext by design; see DCF_SECURITY_EXPOSURE.md).
    """
    h = 0x811C9DC5
    for b in data:
        h ^= b & 0xFF
        h = (h * 0x01000193) & 0xFFFFFFFF
    return h


def _be16(v):
    return bytes([(v >> 8) & 0xFF, v & 0xFF])


def _be32(v):
    return bytes([(v >> 24) & 0xFF, (v >> 16) & 0xFF, (v >> 8) & 0xFF, v & 0xFF])


def _rd16(buf, off):
    return (buf[off] << 8) | buf[off + 1]


def _rd32(buf, off):
    return (buf[off] << 24) | (buf[off + 1] << 16) | (buf[off + 2] << 8) | buf[off + 3]


# ── OPEN ──────────────────────────────────────────────────────────────────────
def pack_open(session_id, total_len, chunk_size, obj_checksum):
    """Begin a transfer: total byte length, chunk size, whole-object FNV-1a checksum."""
    return bytes([PIPE_OPEN, PIPE_VERSION]) + _be16(session_id) + \
        _be32(total_len) + _be16(chunk_size) + _be32(obj_checksum)


def unpack_open(buf):
    if len(buf) < 14 or buf[0] != PIPE_OPEN or buf[1] != PIPE_VERSION:
        raise ValueError("not an OPEN")
    return (_rd16(buf, 2), _rd32(buf, 4), _rd16(buf, 8), _rd32(buf, 10))


# ── CREDIT ────────────────────────────────────────────────────────────────────
def pack_credit(session_id, credit):
    """Receiver grants the sender permission for `credit` more chunks."""
    return bytes([PIPE_CREDIT, PIPE_VERSION]) + _be16(session_id) + _be32(credit)


def unpack_credit(buf):
    if len(buf) < 8 or buf[0] != PIPE_CREDIT or buf[1] != PIPE_VERSION:
        raise ValueError("not a CREDIT")
    return (_rd16(buf, 2), _rd32(buf, 4))


# ── SACK ──────────────────────────────────────────────────────────────────────
def pack_sack(session_id, base, bitmap):
    """Cumulative ack: all chunks < base received; `bitmap` bytes mark chunks
    base, base+1, ... (LSB-first within each byte) that arrived above base."""
    if len(bitmap) > 255:
        raise ValueError("sack bitmap too long")
    return bytes([PIPE_SACK, PIPE_VERSION]) + _be16(session_id) + _be32(base) + \
        bytes([len(bitmap)]) + bytes(bitmap)


def unpack_sack(buf):
    if len(buf) < 9 or buf[0] != PIPE_SACK or buf[1] != PIPE_VERSION:
        raise ValueError("not a SACK")
    nbytes = buf[8]
    if len(buf) < 9 + nbytes:
        raise ValueError("truncated SACK")
    return (_rd16(buf, 2), _rd32(buf, 4), bytes(buf[9:9 + nbytes]))


def sack_has(bitmap, base, seq):
    """True if `seq` (>= base) is marked present in a SACK bitmap."""
    if seq < base:
        return True  # below the cumulative base = already acked
    off = seq - base
    byte, bit = divmod(off, 8)
    return byte < len(bitmap) and bool(bitmap[byte] & (1 << bit))


# ── NACK ──────────────────────────────────────────────────────────────────────
def pack_nack(session_id, missing):
    """Explicit missing-chunk list (the FEC-budget fallback). `missing` is a
    list of chunk sequence numbers, up to 255."""
    if len(missing) > 255:
        raise ValueError("too many NACK entries")
    out = bytearray([PIPE_NACK, PIPE_VERSION]) + _be16(session_id) + bytes([len(missing)])
    for seq in missing:
        out += _be32(seq)
    return bytes(out)


def unpack_nack(buf):
    if len(buf) < 5 or buf[0] != PIPE_NACK or buf[1] != PIPE_VERSION:
        raise ValueError("not a NACK")
    n = buf[4]
    if len(buf) < 5 + 4 * n:
        raise ValueError("truncated NACK")
    return (_rd16(buf, 2), [_rd32(buf, 5 + 4 * i) for i in range(n)])


# ── DONE ──────────────────────────────────────────────────────────────────────
def pack_done(session_id):
    return bytes([PIPE_DONE, PIPE_VERSION]) + _be16(session_id)


def unpack_done(buf):
    if len(buf) < 4 or buf[0] != PIPE_DONE or buf[1] != PIPE_VERSION:
        raise ValueError("not a DONE")
    return _rd16(buf, 2)


# ── ABORT ─────────────────────────────────────────────────────────────────────
def pack_abort(session_id, reason):
    return bytes([PIPE_ABORT, PIPE_VERSION]) + _be16(session_id) + bytes([reason & 0xFF])


def unpack_abort(buf):
    if len(buf) < 5 or buf[0] != PIPE_ABORT or buf[1] != PIPE_VERSION:
        raise ValueError("not an ABORT")
    return (_rd16(buf, 2), buf[4])


def pipe_msg_type(buf):
    return buf[0] if buf else -1


# ── Data-plane chunk header ───────────────────────────────────────────────────
# The dumb UDP lane's datagram: [session_id(2) | chunk_seq(4) | payload...].
# Not a DeModFrame — a raw datagram — but its header is certified too so every
# language frames chunks identically.
CHUNK_HDR_LEN = 6


def pack_chunk(session_id, chunk_seq, payload):
    return _be16(session_id) + _be32(chunk_seq) + bytes(payload)


def unpack_chunk(buf):
    if len(buf) < CHUNK_HDR_LEN:
        raise ValueError("short chunk")
    return (_rd16(buf, 0), _rd32(buf, 2), bytes(buf[CHUNK_HDR_LEN:]))


def num_chunks(total_len, chunk_size):
    """Number of data chunks a `total_len`-byte object splits into."""
    if chunk_size <= 0:
        raise ValueError("chunk_size must be positive")
    return (total_len + chunk_size - 1) // chunk_size


if __name__ == "__main__":
    # Smoke laws (the full generator lives in gen_pipe_vectors.py).
    assert unpack_open(pack_open(7, 100000, 1400, 0xDEADBEEF)) == (7, 100000, 1400, 0xDEADBEEF)
    assert unpack_credit(pack_credit(7, 32)) == (7, 32)
    sid, base, bm = unpack_sack(pack_sack(7, 10, bytes([0b00000101])))
    assert (sid, base) == (7, 10) and sack_has(bm, 10, 10) and sack_has(bm, 10, 12) and not sack_has(bm, 10, 11)
    assert unpack_nack(pack_nack(7, [3, 9, 4000000])) == (7, [3, 9, 4000000])
    assert unpack_done(pack_done(7)) == 7
    assert unpack_abort(pack_abort(7, ABORT_CHECKSUM)) == (7, ABORT_CHECKSUM)
    assert unpack_chunk(pack_chunk(7, 42, b"hello")) == (7, 42, b"hello")
    assert fnv1a32(b"") == 0x811C9DC5
    assert fnv1a32(b"hello") == 0x4F9F2CAB
    assert num_chunks(100000, 1400) == 72
    print("ALL PIPE SMOKE LAWS HOLD")
