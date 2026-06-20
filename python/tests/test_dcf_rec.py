# SPDX-License-Identifier: LGPL-3.0-only
"""Smoke test for the dcf-rec CLI `info` path (pure-python frame scan; no ffmpeg).

Generates a small deterministic PCM-diag dump and asserts `dcf-rec info` reports the
peer, codec, and zero loss. Run: pytest python/tests/test_dcf_rec.py.
"""
import os
import subprocess
import sys
import tempfile
import unittest

_ROOT = os.path.join(os.path.dirname(__file__), "..", "..")
sys.path.insert(0, os.path.join(_ROOT, "python", "MCP"))

import audiolab_core as al

DCF_REC = os.path.join(_ROOT, "ffmpeg-dcf", "dcf-rec")


def make_dump(path, src=1, dst=0xFFFF, blocks=10):
    with open(path, "wb") as f:
        for pid in range(blocks):
            payload = bytes([0x80]) * al.PCM_DIAG_BLOCK     # silence, byte-deterministic
            for fr in al.packetize(al.CODEC_PCM_DIAG, payload, pid,
                                   (pid * 20000) & 0xFFFFFF, src, dst, 0):
                f.write(fr)


class TestDcfRecInfo(unittest.TestCase):
    def test_info_reports_peer_and_codec(self):
        with tempfile.TemporaryDirectory() as d:
            dump = os.path.join(d, "t.dcf")
            make_dump(dump, src=0x00a1, blocks=10)
            out = subprocess.run([sys.executable, DCF_REC, "info", dump, "--json"],
                                 capture_output=True, text=True)
            self.assertEqual(out.returncode, 0, out.stderr)
            import json
            data = json.loads(out.stdout)
            self.assertEqual(len(data), 1)
            st = data[0]
            self.assertEqual(st["src"], 0x00a1)
            self.assertEqual(st["codec"], "pcm_u8")
            self.assertEqual(st["packets"], 10)
            self.assertEqual(st["loss_pct"], 0.0)


if __name__ == "__main__":
    unittest.main()
