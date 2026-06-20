# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the acoustic modem (python/modem/acoustic.py) and the shared on-air framing
(acoustic_frame.py) that makes the numpy modem interoperable with the real-time Faust
modem (main.py). A DeModFrame crosses a *modelled* handheld-radio channel (300-3000 Hz
band-pass + AGC + noise) and is recovered. Needs numpy (skips cleanly without it). See
Documentation/DCF_FIELD_USE.md."""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "modem"))

try:
    import numpy as np
    import acoustic as A
    import acoustic_frame as af
    from wirelab_core import encode
    HAVE = True
except Exception:
    HAVE = False

if HAVE:
    FRAME = encode(3, 0x1234, 0x00A1, 1420, b"\xde\xad\xbe\xef", 0xABCDEF)


@unittest.skipUnless(HAVE, "numpy required for the acoustic modem")
class TestAcousticFraming(unittest.TestCase):
    """The shared bit framing — the contract both modems implement."""

    def test_roundtrip_both_payload_modes(self):
        for fec in (False, True):
            bits = af.encode_bits(FRAME, fec_mode=fec, preamble_bits=240)
            self.assertEqual(af.decode_bits(bits, fec_mode=fec), (FRAME, 0 if not fec else 0))

    def test_matches_legacy_faust_framing(self):
        # acoustic_frame (which main.py now delegates to) must reproduce the deployed
        # Faust modem's exact on-air bits: preamble(0,1,…) | 0x7E | frame | crc8 | postamble.
        for pre in (80, 240):
            for _ in range(50):
                fb = os.urandom(17)
                crc = af.crc8(fb)
                expect = ([i % 2 for i in range(pre)]
                          + af.to_bits(bytes([0x7E])) + af.to_bits(fb) + af.to_bits(bytes([crc]))
                          + [i % 2 for i in range(16)])
                self.assertEqual(af.encode_bits(fb, fec_mode=False, preamble_bits=pre), expect)

    def test_crc8_matches_faust_modem(self):
        # the poly-0x31, MSB-first, non-reflected CRC-8 the deployed Faust modem uses
        # (its docstring says "MAXIM" but it is not reflected; what matters is byte parity).
        self.assertEqual(af.crc8(b"123456789"), 0xA2)
        self.assertEqual(af.crc8(b"\x00" * 17), 0x00)


@unittest.skipUnless(HAVE, "numpy required for the acoustic modem")
class TestAcousticInterop(unittest.TestCase):
    """numpy <-> Faust interoperability: same bits + same tones/baud => same on-air signal."""

    def test_numpy_tx_carries_shared_bits(self):
        # The audio the numpy modem transmits decodes back to the frame via the SHARED
        # framing — i.e. exactly the bits a Faust receiver would slice and decode.
        for prof in ("handheld", "standard"):
            audio, sps, nbits = A.frame_to_audio(FRAME, profile=prof, fec=False)
            out, _ = A.audio_to_frame(audio, sps, nbits, profile=prof, fec=False)
            self.assertEqual(out, FRAME, prof)

    def test_profiles_are_shared(self):
        # main.py uses acoustic_frame.PROFILES, so the numpy modem sees identical tones/baud.
        self.assertIs(A.PROFILES, af.PROFILES)
        self.assertEqual(af.PROFILES["handheld"]["baud"], af.PROFILES["standard"]["baud"])


@unittest.skipUnless(HAVE, "numpy required for the acoustic modem")
class TestAcousticChannel(unittest.TestCase):
    def test_clean_loopback(self):
        for prof in ("handheld", "standard"):
            for fec in (False, True):
                self.assertEqual(A.loopback(FRAME, profile=prof, fec=fec, channel=False)[0],
                                 FRAME, f"{prof}/{fec}")

    def test_through_walkie_channel(self):
        for fec in (False, True):
            ok = sum(A.loopback(FRAME, profile="handheld", fec=fec, snr_db=12.0, seed=s)[0] == FRAME
                     for s in range(40))
            self.assertEqual(ok, 40, f"fec={fec}")

    def test_payload_independence(self):
        import random
        rng = random.Random(5)
        fixed = [b"\x00\x00\x00\x00", b"\xff\xff\xff\xff", b"PyPI", b"0000", b"\xaa\xaa\xaa\xaa"]
        for p in fixed + [bytes(rng.randrange(256) for _ in range(4)) for _ in range(30)]:
            fr = encode(3, rng.randrange(0x10000), rng.randrange(0x10000),
                        rng.randrange(0x10000), p, rng.randrange(0x1000000))
            self.assertEqual(A.loopback(fr, profile="handheld", snr_db=14.0, seed=1)[0], fr, p.hex())

    def test_bandpass_attenuates_out_of_band(self):
        fs = A.FS
        t = np.arange(int(0.05 * fs)) / fs
        for hz, passes in [(1500, True), (200, False), (3500, False)]:
            x = np.sin(2 * np.pi * hz * t).astype(np.float32)
            rms = np.sqrt(np.mean(A.walkie_channel(x, snr_db=120, agc=False, seed=0) ** 2))
            self.assertTrue(rms > 0.5 if passes else rms < 0.05, f"{hz} Hz")

    def test_out_of_band_tones_are_lost(self):
        def recovers(mark, space):
            a, sps, nb = A.frame_to_audio(FRAME, mark=mark, space=space)
            ch = A.walkie_channel(a, snr_db=40, seed=0)
            r = A.audio_to_frame(ch, sps, nb, mark=mark, space=space)
            return bool(r and r[0] == FRAME)
        self.assertTrue(recovers(1200, 1800))            # in-band: fine
        self.assertFalse(recovers(3500, 4000))           # both out of band: lost


if __name__ == "__main__":
    unittest.main()
