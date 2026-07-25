# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Pipe tests: control codec round-trips + lossless transfer over a lossy link."""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))
import pipelab_core as pc  # noqa: E402
from dcf.pipe import run_transfer, profile as pipe_profile, PipeSender  # noqa: E402


class TestPipeCodec(unittest.TestCase):
    def test_control_roundtrips(self):
        self.assertEqual(pc.unpack_open(pc.pack_open(7, 100000, 1400, 0xDEADBEEF)),
                         (7, 100000, 1400, 0xDEADBEEF))
        self.assertEqual(pc.unpack_credit(pc.pack_credit(7, 32)), (7, 32))
        self.assertEqual(pc.unpack_nack(pc.pack_nack(7, [3, 9, 4000000])), (7, [3, 9, 4000000]))
        self.assertEqual(pc.unpack_done(pc.pack_done(7)), 7)
        self.assertEqual(pc.unpack_abort(pc.pack_abort(7, pc.ABORT_TIMEOUT)), (7, pc.ABORT_TIMEOUT))
        self.assertEqual(pc.unpack_chunk(pc.pack_chunk(7, 42, b"hi")), (7, 42, b"hi"))

    def test_fnv_anchors(self):
        self.assertEqual(pc.fnv1a32(b""), 0x811C9DC5)
        self.assertEqual(pc.fnv1a32(b"hello"), 0x4F9F2CAB)

    def test_sack_bitmap(self):
        bm = bytes([0b00000101])
        self.assertTrue(pc.sack_has(bm, 10, 10))
        self.assertTrue(pc.sack_has(bm, 10, 12))
        self.assertFalse(pc.sack_has(bm, 10, 11))
        self.assertTrue(pc.sack_has(bm, 10, 9))  # below base


class TestPipeTransfer(unittest.TestCase):
    def setUp(self):
        self.payload = bytes((i * 37 + 11) & 0xFF for i in range(20000))

    def test_lossless_clean_link(self):
        got, stats = run_transfer(self.payload, chunk_size=512, drop=0.0, corrupt=0.0, seed=1)
        self.assertEqual(got, self.payload)
        self.assertEqual(stats["dropped"], 0)
        # a clean link needs no retransmits
        self.assertEqual(stats["retransmits"], 0)

    def test_corruption_healed_forward_by_fec(self):
        # byte corruption below the FEC budget must heal with NO retransmit
        got, stats = run_transfer(self.payload, chunk_size=512, drop=0.0, corrupt=0.30, seed=2)
        self.assertEqual(got, self.payload)
        self.assertGreater(stats["corrupted"], 0)
        self.assertEqual(stats["retransmits"], 0, "FEC should heal single-byte corruption in-place")

    def test_whole_chunk_drops_recovered_by_nack(self):
        got, stats = run_transfer(self.payload, chunk_size=512, drop=0.20, corrupt=0.0, seed=3)
        self.assertEqual(got, self.payload)
        self.assertGreater(stats["dropped"], 0)
        self.assertGreater(stats["retransmits"], 0, "dropped chunks must be retransmitted")

    def test_both_loss_modes(self):
        got, _ = run_transfer(self.payload, chunk_size=400, drop=0.15, corrupt=0.15, seed=4)
        self.assertEqual(got, self.payload)

    def test_small_object(self):
        got, _ = run_transfer(b"a tiny payload", chunk_size=8, drop=0.1, seed=5)
        self.assertEqual(got, b"a tiny payload")


class TestHydraModemProfile(unittest.TestCase):
    """The acoustic link (~8-12 app-bytes/s) is where airtime, not CPU, is the
    scarce resource: the profile must not spend bytes on redundant parity, and a
    lossy link must still converge byte-exact."""

    def test_profile_values(self):
        p = pipe_profile("hydramodem")
        self.assertEqual(p["nparity"], 0, "modem's own conv FEC already corrects")
        self.assertLessEqual(p["chunk_size"], 512, "small chunks: one loss must not cost minutes")
        self.assertLessEqual(p["credit_window"], 4, "no point authorizing far ahead on a slow link")

    def test_lossless_over_lossy_acoustic_link(self):
        payload = bytes((i * 13 + 5) & 0xFF for i in range(2000))
        got, stats = run_transfer(payload, profile_name="hydramodem", drop=0.2, seed=11)
        self.assertEqual(got, payload)
        self.assertGreater(stats["dropped"], 0)

    def test_airtime_overhead_is_minimal(self):
        """With nparity=0 the on-air bytes are the object plus only the 6-byte
        chunk header — no FEC parity tax on a link that already corrects."""
        payload = bytes(1024)
        p = pipe_profile("hydramodem")
        s = PipeSender(1, payload, p["chunk_size"], p["nparity"])
        on_air = sum(len(s._chunk_payload(i)) + 6 for i in range(s.n))
        overhead = (on_air - len(payload)) / len(payload)
        self.assertLess(overhead, 0.05, f"airtime overhead {overhead:.1%} too high for the modem")

    def test_lan_profile_does_use_fec(self):
        """Contrast: on a fast link, parity is cheap and worth it."""
        self.assertGreater(pipe_profile("lan")["nparity"], 0)


if __name__ == "__main__":
    unittest.main()
