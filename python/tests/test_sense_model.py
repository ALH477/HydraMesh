# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Sense energy/throughput model — sanity invariants."""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

try:
    from dcf.sense import model
    HAVE = True
except Exception:
    HAVE = False


@unittest.skipUnless(HAVE, "dcf.sense.model required")
class TestModel(unittest.TestCase):
    def test_airtime_ordering(self):
        # none is fastest, rep3 slowest (more coding)
        self.assertLess(model.AIRTIME_S["none"], model.AIRTIME_S["conv"])
        self.assertLess(model.AIRTIME_S["conv"], model.AIRTIME_S["rep3"])

    def test_capacity_scales_with_interval(self):
        a = model.model(interval_s=30)
        b = model.model(interval_s=60)
        self.assertGreater(b["nodes_per_channel"], a["nodes_per_channel"])

    def test_fdma_multiplies_capacity(self):
        one = model.model(channels=1)
        eight = model.model(channels=8)
        self.assertEqual(eight["total_nodes"], 8 * one["nodes_per_channel"])

    def test_energy_positive_and_line_negligible(self):
        m = model.model(node_mw=70.0)
        self.assertGreater(m["energy_per_reading_mj"], 0)
        self.assertLess(m["line_mw"], 1.0)        # line drive is sub-milliwatt

    def test_duty_cycle_low(self):
        m = model.model(interval_s=60)
        self.assertLess(m["duty_cycle"], 0.02)    # <2% at a 60s report interval


if __name__ == "__main__":
    unittest.main()
