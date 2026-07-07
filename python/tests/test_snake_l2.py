# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the DCF-Snake raw-L2 Ethernet transport (dcf.l2eth): the SuperPack batching
codec (byte-identical to hydramodem/dcf-tools/snake_l2.h) and the privilege-free loopback
double.  No CAP_NET_RAW needed — the batching is pure.  See Documentation/DCF_SNAKE_SPEC.md."""
import os
import sys
import time
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

import wirelab_core as wire
import snakelab_core as snake
import monitorlab_core as cue
from dcf import l2eth
from dcf import transport as T


class TestSnakeL2Batch(unittest.TestCase):
    def test_batch_unbatch_roundtrip_even_and_odd(self):
        for n in range(1, 10):
            frames = [wire.encode(3, 0x1000 + i, 7, 42, bytes([i, 0xAB, 0, 0]), 0x010203)
                      for i in range(n)]
            blob = l2eth.batch(frames)
            self.assertEqual(len(blob), l2eth.L2_HDR + ((n + 1) // 2) * l2eth.SUPER_LEN)
            self.assertEqual(int.from_bytes(blob[:2], "big"), n)   # header = frame count
            out = l2eth.unbatch(blob)
            self.assertEqual(out, frames, f"n={n} byte-exact round-trip")

    def test_filler_is_zero_data_frame(self):
        # the odd-frame filler is the canonical zero DATA frame (all app fields 0); it is a
        # valid DeModFrame and is byte-identical to the C snake_l2.h filler.
        d = wire.decode(l2eth._FILLER)
        self.assertEqual((d["frame_type"], d["seq"], d["src"], d["dst"], d["payload"], d["ts_us"]),
                         (0, 0, 0, 0, "00000000", 0))

    def test_capacity(self):
        self.assertEqual(l2eth.l2_capacity(1500), 92)
        self.assertEqual(l2eth.l2_capacity(9000), 562)

    def test_truncated_batch_rejected(self):
        blob = l2eth.batch([wire.encode(3, 1, 1, 2, b"abcd", 0)])
        with self.assertRaises(ValueError):
            l2eth.unbatch(blob[:-1])

    def test_snake_message_through_l2(self):
        # a whole DCF-Snake QSS message survives packetize → batch → unbatch → reassemble
        qss = bytes((i * 37) & 0xFF for i in range(500))
        frames = snake.packetize(qss, 5, 0x00ABCD, 0x00A1, 0xFFFF,
                                 snake.MODE_LIVE, snake.FLAG_ANCHOR)
        out = l2eth.unbatch(l2eth.batch(frames))
        self.assertEqual(out, frames)
        r = snake.SnakeReassembler()
        got = []
        for f in out:
            got += r.push(f)
        self.assertEqual(len(got), 1)
        self.assertEqual(got[0][6], qss)   # payload bytes

    def test_cue_block_through_l2(self):
        pcm = bytes(range(96))   # 48 samples mono S16 = a 1 ms block
        frames = cue.packetize(pcm, 7, 1000, 0x00A1, 0xFFFF, 48, cue.FMT_S16, 1,
                               cue.FLAG_CUE_RETURN)
        out = l2eth.unbatch(l2eth.batch(frames))
        self.assertEqual(out, frames)
        r = cue.CueReassembler()
        got = []
        for f in out:
            got += r.push(f)
        self.assertEqual(len(got), 1)
        self.assertEqual(got[0][9], pcm)   # pcm bytes


class TestSnakeL2Loopback(unittest.TestCase):
    def test_privilege_free_double_delivers_through_batch(self):
        medium = T.LoopbackMedium()
        tx = l2eth.L2LoopbackTransport("tx", medium)
        rx = l2eth.L2LoopbackTransport("rx", medium)
        received = []
        rx.start(lambda frame, meta: received.append(frame))
        tx.start(lambda frame, meta: None)
        frame = wire.encode(3, 0x2A, 0x00A1, 0xFFFF, b"cue!", 0x010203)
        tx.send(frame)
        deadline = time.monotonic() + 2.0
        while not received and time.monotonic() < deadline:
            time.sleep(0.01)
        tx.stop()
        rx.stop()
        self.assertEqual(received, [frame], "frame survives the SuperPack batch codec loopback")


if __name__ == "__main__":
    unittest.main()
