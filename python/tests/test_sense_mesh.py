# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Sense over the self-healing mesh: a reading is a DeModFrame, so the existing
dcf.bridge.Bridge relays it multi-hop across media. Here a sensor reading crosses a
2-hop relay (medium A -> bridge -> medium B) and decodes at the far gateway."""
import os
import sys
import time
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

try:
    import numpy  # noqa: F401
    from dcf import transport as T
    from dcf.bridge import Bridge
    from dcf.sense import schema
    from dcf.sense.gateway import Gateway
    HAVE = True
except Exception:
    HAVE = False


@unittest.skipUnless(HAVE, "numpy + dcf.sense required")
class TestSenseMesh(unittest.TestCase):
    def test_reading_crosses_two_hops(self):
        m1, m2 = T.LoopbackMedium(), T.LoopbackMedium()
        # relay node: bridges medium 1 -> medium 2 (flood)
        relay = Bridge([T.LoopbackTransport("relay.a", m1),
                        T.LoopbackTransport("relay.b", m2)]).start()
        # gateway on medium 2 decodes DCF-Sense readings
        gw = Gateway(T.LoopbackTransport("gw", m2)).start()
        # sensor node on medium 1
        src = T.LoopbackTransport("node", m1)
        src.start(lambda f, m: None)
        try:
            frame = schema.encode_reading(0x4001, schema.NAME_TO_ID["temp"], 24.5)
            src.send(frame)
            t0 = time.monotonic()
            while not gw.readings and time.monotonic() - t0 < 3.0:
                time.sleep(0.02)
            self.assertTrue(gw.readings, "reading crossed the 2-hop mesh")
            r = gw.readings[0]
            self.assertEqual(r["node_id"], 0x4001)
            self.assertEqual(r["sensor"], "temp")
            self.assertAlmostEqual(r["value"], 24.5, places=2)
        finally:
            src.stop(); gw.stop(); relay.stop()


if __name__ == "__main__":
    unittest.main()
