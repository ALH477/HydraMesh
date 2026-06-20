# SPDX-License-Identifier: LGPL-3.0-only
"""Unit tests for the DCF-Radio engine (no ffmpeg needed — the encoder is a no-op
`true` and the writer thread is driven manually via _tick()).

Covers: duration/name parsing, channel routing by dst, only_channel filter,
per-peer reassembly + tracking, and the gapless silence-fill that prevents dead air.
Run: pytest python/tests/test_radio.py   (or `make test`).
"""
import os
import sys
import time
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

# Encoder is a no-op for tests: `true` exits immediately; the engine tolerates it.
os.environ["FFMPEG"] = "true"

import audiolab_core as al
from dcf import radio
from dcf.radio import (parse_duration, parse_names, channel_label,
                       RadioStation, RadioServer, _Opts, SILENCE)


def audio_frames(src, dst, codec_id, payload, pid=0):
    """One DCF-Audio packet (descriptor + data frames) for (src,dst,codec)."""
    return al.packetize(codec_id, payload, pid, (pid * 20000) & 0xFFFFFF, src, dst, 0)


class TestParsing(unittest.TestCase):
    def test_parse_duration(self):
        self.assertEqual(parse_duration("6h"), 6 * 3600)
        self.assertEqual(parse_duration("30m"), 1800)
        self.assertEqual(parse_duration("90s"), 90)
        self.assertEqual(parse_duration("120"), 120)
        self.assertEqual(parse_duration(None), radio.DEFAULT_DVR)
        with self.assertRaises(ValueError):
            parse_duration("nope")

    def test_parse_names(self):
        self.assertEqual(parse_names("0x00a1=Hermes,66=Bob"), {0x00a1: "Hermes", 66: "Bob"})
        self.assertEqual(parse_names(""), {})

    def test_channel_label(self):
        self.assertEqual(channel_label(1420), "CH1420")
        self.assertEqual(channel_label(0xFFFF), "CH65535")


class TestRouting(unittest.TestCase):
    def test_feed_routes_by_dst_and_tracks_peers(self):
        import tempfile
        with tempfile.TemporaryDirectory() as d:
            srv = RadioServer(hls_dir=d, http_addr=None)
            try:
                for fr in audio_frames(src=1, dst=1420, codec_id=al.CODEC_PCM_DIAG,
                                       payload=bytes(120)):
                    srv.feed(fr)
                for fr in audio_frames(src=7, dst=2000, codec_id=al.CODEC_PCM_DIAG,
                                       payload=bytes(120)):
                    srv.feed(fr)
                self.assertIn(1420, srv._stations)
                self.assertIn(2000, srv._stations)
                self.assertIn(1, srv._stations[1420]._srcs)
                self.assertIn(7, srv._stations[2000]._srcs)
                info = {i["channel"]: i for i in srv.stations_info()}
                self.assertEqual(info[1420]["num_peers"], 1)
            finally:
                srv.close()

    def test_only_channel_filter(self):
        import tempfile
        with tempfile.TemporaryDirectory() as d:
            srv = RadioServer(hls_dir=d, http_addr=None, only_channel=1420)
            try:
                for fr in audio_frames(1, 2000, al.CODEC_PCM_DIAG, bytes(120)):
                    srv.feed(fr)              # different channel -> ignored
                self.assertEqual(srv._stations, {})
                for fr in audio_frames(1, 1420, al.CODEC_PCM_DIAG, bytes(120)):
                    srv.feed(fr)
                self.assertIn(1420, srv._stations)
            finally:
                srv.close()


class TestSilenceFill(unittest.TestCase):
    """The core 'no dead air' guarantee, driven deterministically."""

    def _station(self, d):
        return RadioStation(dst=1420, hls_dir=d, opts=_Opts(archive_dir=None),
                            start_thread=False)

    def test_real_then_silence(self):
        import tempfile
        with tempfile.TemporaryDirectory() as d:
            st = self._station(d)
            try:
                for fr in audio_frames(1, 1420, al.CODEC_PCM_DIAG, bytes(120)):
                    st.feed(fr, src=1)
                st._tick()                                   # drains the real block
                self.assertEqual(st.frames_real, 1)
                self.assertEqual(st.frames_silence, 0)
                self.assertEqual(st._srcs[1]["codec"], al.CODEC_PCM_DIAG)

                # simulate a talk-gap: peer quiet past GAP_FILL_AFTER
                st._srcs[1]["last_real"] = time.time() - 1.0
                st._tick()                                   # must inject silence
                self.assertEqual(st.frames_silence, 1)       # live edge kept moving
            finally:
                st.close()

    def test_silence_payloads_present_per_codec(self):
        self.assertEqual(len(SILENCE[al.CODEC_PCM_DIAG]), al.PCM_DIAG_BLOCK)
        self.assertEqual(SILENCE[al.CODEC_OPUS], bytes([0xF8, 0xFF, 0xFE]))
        self.assertEqual(len(SILENCE[al.CODEC_FAUST_PM]), 8)


if __name__ == "__main__":
    unittest.main()
