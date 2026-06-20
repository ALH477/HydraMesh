# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the DCF bridge (dcf.bridge): loop-free flooding via dedup, multi-hop relay
across media (audio -> SDR), and egress routing toward an uplink. See
Documentation/DCF_FIELD_USE.md."""
import os
import sys
import tempfile
import time
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

try:
    import numpy  # noqa: F401
    from dcf import transport as T
    from dcf.bridge import Bridge, BROADCAST
    import wirelab_core as wire
    HAVE = True
except Exception:
    HAVE = False

if HAVE:
    class _Recording(T.Transport):
        """Records what it would transmit, without a real medium."""
        def __init__(self, name, **kw):
            super().__init__(name, **kw)
            self.tx = []
        def _transmit(self, frame, dest):
            self.tx.append(frame)

    def _frame(dst=0xFFFF, seq=1, typ=0):
        return wire.encode(typ, seq, 0x00A1, dst, b"\x01\x02\x03\x04", 0)


@unittest.skipUnless(HAVE, "numpy required for the bridge")
class TestBridgeFlood(unittest.TestCase):
    def _drain(self, t):  # run the sender once, synchronously-ish
        time.sleep(0.05)

    def test_flood_relays_to_others_only(self):
        a, b, c = _Recording("a"), _Recording("b"), _Recording("c")
        br = Bridge([a, b, c], route="flood")
        br.start()
        try:
            br._on_frame("a", _frame(), {"transport": "a"})  # arrived on a
            self._drain(a)
            self.assertEqual(len(b.tx), 1)         # relayed to b and c
            self.assertEqual(len(c.tx), 1)
            self.assertEqual(len(a.tx), 0)         # never back to the source link
        finally:
            br.stop()

    def test_dedup_blocks_loops(self):
        a, b = _Recording("a"), _Recording("b")
        br = Bridge([a, b], route="flood")
        br.start()
        try:
            f = _frame(seq=7)
            br._on_frame("a", f, {"transport": "a"})     # new
            br._on_frame("b", f, {"transport": "b"})     # same frame loops back
            self._drain(a)
            self.assertEqual(br.deduped, 1)              # second is dropped
            self.assertEqual(len(b.tx), 1)              # only the first relay happened
        finally:
            br.stop()

    def test_distinct_seqs_not_deduped(self):
        a, b = _Recording("a"), _Recording("b")
        br = Bridge([a, b]).start()
        try:
            br._on_frame("a", _frame(seq=1), {"transport": "a"})
            br._on_frame("a", _frame(seq=2), {"transport": "a"})
            self._drain(a)
            self.assertEqual(len(b.tx), 2)
            self.assertEqual(br.deduped, 0)
        finally:
            br.stop()


@unittest.skipUnless(HAVE, "numpy required for the bridge")
class TestBridgeEgress(unittest.TestCase):
    def test_unknown_dst_goes_to_uplink(self):
        leaf, uplink = _Recording("acoustic"), _Recording("udp")
        br = Bridge([leaf, uplink], route="egress", uplink="udp").start()
        try:
            br._on_frame("acoustic", _frame(dst=0x0042), {"transport": "acoustic"})
            time.sleep(0.05)
            self.assertEqual(len(uplink.tx), 1)    # off-grid dst -> egress to the uplink
            self.assertEqual(len(leaf.tx), 0)
        finally:
            br.stop()

    def test_learned_dst_goes_to_its_transport(self):
        rf, udp = _Recording("rf"), _Recording("udp")
        br = Bridge([rf, udp], route="egress", uplink="udp").start()
        try:
            # node 0x0042 is heard on rf -> reverse-path learned
            br._on_frame("rf", wire.encode(0, 1, 0x0042, 0x00A1, b"\x00\x00\x00\x00", 0),
                         {"transport": "rf"})
            time.sleep(0.05)
            udp.tx.clear(); rf.tx.clear()
            # now a frame TO 0x0042 should go out rf, not the uplink
            br._on_frame("udp", _frame(dst=0x0042, seq=9), {"transport": "udp"})
            time.sleep(0.05)
            self.assertEqual(len(rf.tx), 1)
            self.assertEqual(len(udp.tx), 0)
        finally:
            br.stop()


@unittest.skipUnless(HAVE, "numpy required for the bridge")
class TestBridgeMultiHopMedia(unittest.TestCase):
    def test_audio_then_sdr(self):
        d = tempfile.mkdtemp()
        link1, link2 = os.path.join(d, "l1"), os.path.join(d, "l2")
        arrived = []
        A = Bridge([T.AudioTransport("A.audio", out_dir=link1, profile="handheld")]).start()
        B = Bridge([T.AudioTransport("B.audio", in_dir=link1, profile="handheld"),
                    T.SdrTransport("B.sdr", out_dir=link2, mod="gfsk")]).start()
        C = Bridge([T.SdrTransport("C.sdr", in_dir=link2, mod="gfsk")],
                   on_frame=lambda f, m: arrived.append(f)).start()
        try:
            fr = _frame(seq=3)
            A.inject(fr)
            end = time.monotonic() + 5.0
            while time.monotonic() < end and not arrived:
                time.sleep(0.05)
            self.assertTrue(arrived and arrived[0] == fr, "frame crossed audio->sdr")
        finally:
            A.stop(); B.stop(); C.stop()


if __name__ == "__main__":
    unittest.main()
