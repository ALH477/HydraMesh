# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the quanta ⇄ DCF-Snake bridge (dcf.quanta): the pure, version-independent QSS
packet splitter (sync-scan + CRC resync) and the end-to-end map QSS packet → DCF-Snake
message → wire → reassemble.  Needs no quanta binary — QSS packets are synthesised with the
certified CRC.  See Documentation/DCF_SNAKE_SPEC.md."""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

from wirelab_core import crc16_ccitt
import snakelab_core as snake
from dcf import quanta


def make_header(magic=quanta.QSS2_MAGIC):
    h = bytearray(quanta.QSS_HDR_BYTES)
    h[0:4] = magic.to_bytes(4, "big")
    # fields 4..31 arbitrary (here: a fake sample_rate) then CRC over first 32
    h[4:8] = (48000).to_bytes(4, "big")
    h[32:34] = crc16_ccitt(bytes(h[:32])).to_bytes(2, "big")
    return bytes(h)


def make_packet(hop_index, body):
    """A synthetic QSS packet: sync 0xA55A | hop_index u32 | body | crc16(over hop..body)."""
    inner = hop_index.to_bytes(4, "big") + bytes(body)
    crc = crc16_ccitt(inner)
    return b"\xa5\x5a" + inner + crc.to_bytes(2, "big")


class TestQssSplitter(unittest.TestCase):
    def test_split_join_roundtrip(self):
        hdr = make_header()
        pkts = [make_packet(i, bytes((i * 7 + j) & 0xFF for j in range(10 + i * 3)))
                for i in range(5)]
        stream = quanta.join_qss_packets(hdr, pkts)
        got_hdr, got_pkts = quanta.split_qss_packets(stream)
        self.assertEqual(got_hdr, hdr)
        self.assertEqual(got_pkts, pkts)

    def test_resync_past_false_sync_word_in_body(self):
        # a body that itself contains the 0xA55A byte pair must NOT split the packet — the CRC
        # resync steps past the false sync.
        hdr = make_header()
        p0 = make_packet(1, b"\x00\xa5\x5a\x01\x02\x03")   # false sync inside the body
        p1 = make_packet(2, b"\xde\xad\xbe\xef")
        stream = quanta.join_qss_packets(hdr, [p0, p1])
        _, got = quanta.split_qss_packets(stream)
        self.assertEqual(got, [p0, p1], "CRC resync ignores the body-internal 0xA55A")

    def test_truncated_final_packet_dropped(self):
        hdr = make_header()
        good = make_packet(1, b"abcd")
        truncated = make_packet(2, b"efgh")[:-3]   # chop the CRC + a body byte
        _, got = quanta.split_qss_packets(hdr + b"" + good + truncated)
        # join uses header+packets; build the stream manually here
        stream = hdr + good + truncated
        _, got = quanta.split_qss_packets(stream)
        self.assertEqual(got, [good])

    def test_bad_header_rejected(self):
        with self.assertRaises(ValueError):
            quanta.split_qss_packets(b"\x00" * quanta.QSS_HDR_BYTES)  # bad magic

    def test_qss_packet_to_snake_message_and_back(self):
        # the full bridge: each QSS packet → one DCF-Snake message → wire → reassemble.
        hdr = make_header()
        pkts = [make_packet(i, bytes((i * 13 + j) & 0xFF for j in range(50 + i * 40)))
                for i in range(4)]
        stream = quanta.join_qss_packets(hdr, pkts)
        _, split = quanta.split_qss_packets(stream)
        self.assertEqual(split, pkts)

        # send each packet as a DCF-Snake message on a rolling stream_id; reassemble; rejoin.
        r = snake.SnakeReassembler()
        recovered = []
        for i, pkt in enumerate(split):
            frames = snake.packetize(pkt, i % (snake.MAX_STREAM_ID + 1), 1000 * i,
                                     0x00A1, 0xFFFF, snake.MODE_LIVE, snake.FLAG_ANCHOR)
            for f in frames:
                for ev in r.push(f):
                    recovered.append(ev[6])   # payload bytes
        self.assertEqual(recovered, pkts)
        self.assertEqual(quanta.join_qss_packets(hdr, recovered), stream)


class TestQuantaAvailability(unittest.TestCase):
    def test_available_is_boolean_and_skips_gracefully(self):
        # In CI the binary is absent → False; the wrappers must raise a clear RuntimeError.
        self.assertIn(quanta.quanta_available(), (True, False))
        if not quanta.quanta_available():
            with self.assertRaises(RuntimeError):
                quanta.encode_wav("/nonexistent.wav")


if __name__ == "__main__":
    unittest.main()
