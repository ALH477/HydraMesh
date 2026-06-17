"""Wire-level tests for the Matrix<->DCF bridge adapters.

Covers the text-over-DeModFrame adapter, the 32-byte SuperPack container, and the
UDP node loopback.  These pin the bytes the bridge puts on the wire; the inner
frames stay ordinary DeModFrames so the repo's 246-vector wire certificate is not
affected.
"""
import os
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, ".."))
sys.path.insert(0, os.path.join(HERE, "..", "..", "python", "MCP"))

import dcf_text
import superpack
from dcf_node import DcfTextNode
from wirelab_core import decode, encode


# ── text adapter ──────────────────────────────────────────────────────────────
def test_text_roundtrip_unicode_and_caps():
    for msg in ["", "hi", "DeModFrame", "❤️\U0001f680 mesh" * 10, "z" * dcf_text.MAX_PAYLOAD]:
        frames = dcf_text.packetize(msg, 1, 42, src=7, dst=9)
        for f in frames:                               # every frame is a valid quantum
            d = decode(f)
            assert len(f) == 17 and d["frame_type"] == dcf_text.FDATA
        r = dcf_text.TextReassembler()
        out = []
        for f in frames:
            out += r.push(f)
        assert len(out) == 1
        assert out[0][5] == msg and out[0][3] == 7 and out[0][4] == 9


def test_text_out_of_order_and_dupes():
    # Duplicate + reordered fragments arriving BEFORE completion reassemble once.
    # (Dedup of a fully retransmitted message reusing the same 6-bit packet_id is
    # the application layer's job, not the reassembler's.)
    msg = "abcdefghijklmnop"                            # 16 B -> desc + 4 data frags
    f = dcf_text.packetize(msg, 5, 1, src=1, dst=2)
    assert len(f) == 5
    order = [f[2], f[1], f[1], f[0], f[2], f[3], f[4]]   # dupes/reorder, f[4] completes
    r = dcf_text.TextReassembler()
    out = []
    for fr in order:
        out += r.push(fr)
    assert len(out) == 1 and out[0][5] == msg


def test_text_channel_filter():
    lab = dcf_text.channel_id("lab")
    frames = dcf_text.packetize("secret", 1, 1, src=1, dst=lab)
    # a reassembler bound to a different channel ignores it (broadcast still ok)
    r = dcf_text.TextReassembler(accept_dst=dcf_text.channel_id("other"))
    out = []
    for f in frames:
        out += r.push(f)
    assert out == []


def test_text_too_big():
    import pytest
    with pytest.raises(ValueError):
        dcf_text.packetize("x" * (dcf_text.MAX_PAYLOAD + 1), 0, 0, src=0, dst=0)


# ── SuperPack ─────────────────────────────────────────────────────────────────
def test_superpack_lossless_and_size():
    a = encode(0, 0x0102, 0x0304, 0x0506, b"\xde\xad\xbe\xef", 0x010203)
    b = encode(3, 0x1f, 0x0007, 0xffff, b"chat", 0x0000ff)
    sp = superpack.pack(a, b)
    assert len(sp) == 32 < 2 * 17
    assert superpack.is_superpack(sp)
    ra, rb = superpack.unpack(sp)
    assert ra == a and rb == b
    # reconstructed frames are themselves valid quanta
    decode(ra); decode(rb)


def test_superpack_golden_joint_crc():
    zero = encode(0, 0, 0, 0, b"\x00\x00\x00\x00", 0)
    sp = superpack.pack(zero, zero)
    assert int.from_bytes(sp[30:32], "big") == 0x5B75


def test_superpack_tamper_detected():
    import pytest
    a = encode(0, 1, 2, 3, b"abcd", 4)
    b = encode(0, 5, 6, 7, b"efgh", 8)
    sp = bytearray(superpack.pack(a, b))
    sp[12] ^= 0xFF
    with pytest.raises(ValueError):
        superpack.unpack(bytes(sp))


def test_superpack_rejects_bad_inner_frame():
    import pytest
    a = bytearray(encode(0, 1, 2, 3, b"abcd", 4))
    a[16] ^= 0xFF                                       # corrupt inner CRC
    b = encode(0, 5, 6, 7, b"efgh", 8)
    with pytest.raises(ValueError):
        superpack.pack(bytes(a), b)


# ── node loopback ─────────────────────────────────────────────────────────────
def test_node_loopback_superpack():
    a = DcfTextNode(node_id=1, port=0, channel="lab", use_superpack=True)
    b = DcfTextNode(node_id=2, port=0, channel="lab", use_superpack=True)
    a.add_peer("b", "127.0.0.1", b.port)
    b.start()
    try:
        msg = "round trip through real UDP \U0001f680 " * 4
        a.send_text(msg, to="b")
        got = None
        deadline = time.time() + 3
        while time.time() < deadline and got is None:
            got = b.poll(timeout=0.2)
        assert got is not None and got[3] == msg and got[1] == 1
    finally:
        a.stop(); b.stop()
