# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Sense config-driven network builder: one SenseConfig -> gateway + nodes; readings
flow end-to-end over the loopback medium, honouring per-node sensor filters and MAC mode."""
import os
import sys
import time
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

try:
    import numpy  # noqa: F401  (transport layer)
    from dcf.sense.config import SenseConfig
    from dcf.sense import network
    HAVE = True
except Exception:
    HAVE = False


def _read(node_id):
    return {"temp": 21.0 + (node_id & 0xF), "humidity": 60.0, "co2": 500}


@unittest.skipUnless(HAVE, "numpy + dcf.sense required")
class TestNetwork(unittest.TestCase):
    def _run(self, cfg_dict, cycles=2):
        cfg = SenseConfig.from_dict(cfg_dict)
        net = network.build_network(cfg, _read)        # default loopback factory
        got = []
        net.gateway.on_reading = got.append
        sent = net.run_cycles(cycles)
        t0 = time.monotonic()
        while net.gateway.decoded < sent and time.monotonic() - t0 < 3.0:
            time.sleep(0.02)
        net.stop()
        return sent, got

    def test_tdma_star_end_to_end(self):
        sent, got = self._run({
            "topology": "star",
            "mac": {"mode": "tdma", "num_slots": 3, "slot_dur": 1.0, "guard": 0.05},
            "nodes": [{"node_id": 0x1001, "sensors": ["temp", "humidity"]},
                      {"node_id": 0x1002, "sensors": ["temp", "co2"]},
                      {"node_id": 0x1003}],          # all sensors
        })
        # node1: 2 sensors, node2: 2, node3: 3  -> 7/cycle * 2 cycles = 14
        self.assertEqual(sent, 14)
        self.assertEqual(len(got), 14)

    def test_per_node_sensor_filter(self):
        _, got = self._run({
            "topology": "star",
            "mac": {"mode": "dedicated"},
            "nodes": [{"node_id": 0x2001, "sensors": ["co2"]}],
        }, cycles=1)
        self.assertTrue(all(r["sensor"] == "co2" for r in got))
        self.assertEqual(len(got), 1)

    def test_csma_mode_builds_and_runs(self):
        sent, got = self._run({
            "topology": "star", "mac": {"mode": "csma", "slot_dur": 0.2},
            "nodes": [{"node_id": 1, "sensors": ["temp"]},
                      {"node_id": 2, "sensors": ["temp"]}],
        }, cycles=1)
        self.assertEqual(sent, 2)
        self.assertEqual(len(got), 2)


if __name__ == "__main__":
    unittest.main()
