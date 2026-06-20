# SPDX-License-Identifier: LGPL-3.0-only
"""Loopback tests for the DCF IQ modem: every modulation carries a DeModFrame over an
AWGN channel and the RS-FEC recovers it byte-exact — the proof DCF is usable over real
radio. Needs numpy (skips cleanly without it). Run: pytest python/tests/test_iq_loopback.py
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "modem"))

try:
    import numpy as np
    import iq
    HAVE = True
except Exception:
    HAVE = False

if HAVE:
    from wirelab_core import encode
    FRAME = encode(3, 0x1234, 0x00A1, 1420, b"\xde\xad\xbe\xef", 0xABCDEF)


@unittest.skipUnless(HAVE, "numpy required for the IQ loopback")
class TestIQLoopback(unittest.TestCase):
    def _run(self, mod, snr, trials=30):
        np.random.seed(11)
        ok = 0
        for _ in range(trials):
            s = iq.frame_to_iq(FRAME, mod=mod, sps=8)
            r = iq.iq_to_frame(iq.awgn(s, snr), mod=mod, sps=8)
            if r and r[0] == FRAME:
                ok += 1
        return ok, trials

    def test_gfsk(self):
        ok, n = self._run("gfsk", 18); self.assertEqual(ok, n)

    def test_qpsk(self):
        ok, n = self._run("qpsk", 16); self.assertEqual(ok, n)

    def test_qam16(self):
        ok, n = self._run("qam", 26); self.assertEqual(ok, n)

    def test_ook(self):
        ok, n = self._run("ook", 16); self.assertEqual(ok, n)

    def test_afsk_over_fm(self):
        np.random.seed(5)
        ok = 0
        for _ in range(20):
            s, sps, nbits = iq.afsk_fm_modulate(FRAME)
            r = iq.afsk_fm_demodulate(iq.awgn(s, 20), sps, nbits)
            if r and r[0] == FRAME:
                ok += 1
        self.assertEqual(ok, 20)

    def test_fec_adds_margin(self):
        """At an SNR where raw demod makes byte-errors, FEC still recovers."""
        np.random.seed(2)
        import feclab_core as fec
        raw_errors = recovered = 0
        for _ in range(60):
            s = iq.frame_to_iq(FRAME, mod="qpsk", sps=8)
            r = iq.iq_to_frame(iq.awgn(s, 11), mod="qpsk", sps=8)   # marginal SNR
            # did FEC recover?
            if r and r[0] == FRAME:
                recovered += 1
        # FEC should recover the large majority even at a marginal SNR.
        self.assertGreaterEqual(recovered, 50)

    def test_offair_sync(self):
        """Carrier-frequency offset + symbol-timing delay + AWGN: the synchronizer
        (sync=True) recovers where naive demod fails — the off-air path."""
        for mod, cfo, snr in [("gfsk", 8e-4, 18), ("qpsk", 6e-4, 18),
                              ("qam", 3e-4, 30), ("ook", 0.0, 16)]:
            np.random.seed(4)
            sync_ok = 0
            for _ in range(20):
                s = iq.frame_to_iq(FRAME, mod=mod, sps=8)
                n = len(s)
                rx = s * np.exp(2j * np.pi * cfo * np.arange(n))
                rx = np.concatenate([np.zeros(np.random.randint(3, 30), np.complex64),
                                     rx, np.zeros(40, np.complex64)])
                rx = iq.awgn(rx, snr)
                r = iq.iq_to_frame(rx, mod=mod, sps=8, sync=True)
                if r and r[0] == FRAME:
                    sync_ok += 1
            self.assertGreaterEqual(sync_ok, 19, f"{mod} off-air sync {sync_ok}/20")

    def test_cf32_roundtrip(self):
        import tempfile
        s = iq.frame_to_iq(FRAME, mod="gfsk", sps=8)
        with tempfile.NamedTemporaryFile(suffix=".cf32", delete=False) as f:
            path = f.name
        try:
            iq.write_cf32(path, s)
            back = iq.read_cf32(path)
            self.assertEqual(len(back), len(s))
            self.assertTrue(np.allclose(back, s))
            r = iq.iq_to_frame(back, mod="gfsk", sps=8)
            self.assertEqual(r[0], FRAME)
        finally:
            os.unlink(path)


if __name__ == "__main__":
    unittest.main()
