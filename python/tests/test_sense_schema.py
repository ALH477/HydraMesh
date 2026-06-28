# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Sense reading schema: one reading = one bare DeModFrame (src_id = node_id),
encode/decode round-trip and exact byte layout."""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

try:
    from dcf.sense import schema
    import wirelab_core as wire
    HAVE = True
except Exception:
    HAVE = False


@unittest.skipUnless(HAVE, "wirelab_core required")
class TestSenseSchema(unittest.TestCase):
    def test_roundtrip_all_types(self):
        for t, (name, unit, scale) in schema.SENSORS.items():
            frame = schema.encode_reading(0x1001, t, 12.34, dst=0xABCD, seq=7, ts_us=42)
            self.assertEqual(len(frame), 17)
            r = schema.decode_reading(frame)
            self.assertIsNotNone(r)
            self.assertEqual(r["node_id"], 0x1001)
            self.assertEqual(r["dst"], 0xABCD)
            self.assertEqual(r["seq"], 7)
            self.assertEqual(r["sensor_type"], t)
            self.assertEqual(r["sensor"], name)
            # value survives to the type's resolution (1/scale)
            self.assertAlmostEqual(r["value"], round(12.34 * scale) / scale, places=6)

    def test_exact_payload_layout(self):
        # temp 21.5 degC, scale 100 -> raw 2150 = 0x0866; type 0; flags 0
        frame = schema.encode_reading(0x00A1, 0, 21.5, dst=0xFFFF, seq=0, ts_us=0)
        d = wire.decode(frame)
        self.assertEqual(d["frame_type"], 0)            # DATA
        self.assertEqual(d["src"], 0x00A1)
        self.assertEqual(bytes.fromhex(d["payload"]), bytes([0x00, 0x08, 0x66, 0x00]))

    def test_negative_value(self):
        frame = schema.encode_reading(1, 0, -5.25)      # -525 = 0xFDF3 (two's complement)
        r = schema.decode_reading(frame)
        self.assertAlmostEqual(r["value"], -5.25, places=6)

    def test_clamp_sets_flag(self):
        frame = schema.encode_reading(1, 0, 10000.0)    # 10000*100 overflows i16
        r = schema.decode_reading(frame)
        self.assertTrue(r["flags"] & schema.FLAG_CLAMPED)
        self.assertEqual(r["raw"], 32767)

    def test_decode_rejects_non_frame(self):
        self.assertIsNone(schema.decode_reading(b"\x00" * 17))   # bad sync/CRC


if __name__ == "__main__":
    unittest.main()
