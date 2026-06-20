# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the DCF FEC adapter (Reed-Solomon + interleaver) — the correction value
that makes DCF usable over lossy media. Run: pytest python/tests/test_fec.py."""
import os
import random
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

import feclab_core as fec


class TestRS(unittest.TestCase):
    def test_systematic_and_clean(self):
        frame = bytes(range(17))
        code = fec.rs_encode(frame, 16)
        self.assertEqual(len(code), 33)
        self.assertEqual(code[:17], frame)              # systematic
        self.assertEqual(fec.rs_decode(code, 16), (frame, 0))

    def test_corrects_up_to_t(self):
        random.seed(7)
        frame = bytes((i * 37 + 11) & 0xFF for i in range(17))
        code = fec.rs_encode(frame, 16)
        for _ in range(300):
            bad = bytearray(code)
            for p in random.sample(range(33), 8):       # exactly t = 8
                bad[p] ^= random.randint(1, 255)
            msg, n = fec.rs_decode(bytes(bad), 16)
            self.assertEqual(msg, frame)

    def test_beyond_t_is_caught(self):
        random.seed(9)
        frame = bytes(17)
        code = fec.rs_encode(frame, 16)
        caught = 0
        for _ in range(300):
            bad = bytearray(code)
            for p in random.sample(range(33), 9):       # t + 1
                bad[p] ^= random.randint(1, 255)
            try:
                msg, _ = fec.rs_decode(bytes(bad), 16)
                if msg != frame:
                    caught += 1                          # mis-correct (frame CRC would catch)
            except fec.FecError:
                caught += 1
        self.assertEqual(caught, 300)                    # never silently accepted

    def test_interleaver_spreads_burst(self):
        np = 16
        cws = [fec.rs_encode(bytes([i] * 17), np) for i in range(8)]  # depth 8
        stream = bytearray(fec.interleave(cws))
        # an 8-byte burst (== depth) -> at most 1 byte per codeword
        for k in range(8):
            stream[40 + k] ^= 0xFF
        recv = fec.deinterleave(bytes(stream), 8, 33)
        for i, c in enumerate(recv):
            self.assertEqual(fec.rs_decode(c, np)[0], bytes([i] * 17))


if __name__ == "__main__":
    unittest.main()
