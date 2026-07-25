# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-SPA knock-client tests: header layout and token construction (spec §5)."""
import hashlib
import hmac
import struct
import unittest

from dcf.spa import knock


class TestSpaHeader(unittest.TestCase):
    def test_header_layout(self):
        hdr = knock.build_header(
            device_id=5, port=7100, timestamp_ms=0x0102030405060708,
            nonce=bytes(range(16)), flags=0,
        )
        self.assertEqual(len(hdr), knock.HDR_LEN)
        self.assertEqual(hdr[0], 0x53)  # magic 'S'
        self.assertEqual(hdr[1], 0x01)  # version
        # device_id big-endian at [2:4]
        self.assertEqual(struct.unpack(">H", hdr[2:4])[0], 5)
        # timestamp big-endian at [4:12]
        self.assertEqual(struct.unpack(">Q", hdr[4:12])[0], 0x0102030405060708)
        # nonce at [12:28]
        self.assertEqual(hdr[12:28], bytes(range(16)))
        # port at [28:30], flags at [30]
        self.assertEqual(struct.unpack(">H", hdr[28:30])[0], 7100)
        self.assertEqual(hdr[30], 0)

    def test_hmac_token_length_and_tag(self):
        key = bytes([7]) * 32
        tok = knock.token_hmac(key, 5, 7100, timestamp_ms=1000, nonce=bytes(16))
        self.assertEqual(len(tok), 63)  # 31 header + 32 tag
        hdr, tag = tok[:31], tok[31:]
        expected = hmac.new(key, hdr, hashlib.sha256).digest()
        self.assertEqual(tag, expected)  # tag is over the header only

    def test_nonce_must_be_16_bytes(self):
        with self.assertRaises(ValueError):
            knock.build_header(1, 0, nonce=b"short")

    def test_ed25519_optional(self):
        """Ed25519 mode requires PyNaCl; skip cleanly if it's absent."""
        try:
            import nacl.signing  # noqa: F401
        except ImportError:
            self.skipTest("pynacl not installed")
        sk_hex = "00" * 32
        tok = knock.token_ed25519(sk_hex, 5, 7100, timestamp_ms=1000, nonce=bytes(16))
        self.assertEqual(len(tok), 95)  # 31 header + 64 signature


if __name__ == "__main__":
    unittest.main()
