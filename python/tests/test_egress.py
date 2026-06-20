# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for uplink-oriented egress routing (meshlab_core.egress_routes): a mesh orients
every node toward the nearest/cheapest backhaul (Starlink/cellular/ham gateway) by RTT,
reusing the certified RTT Dijkstra via a virtual sink. No wire-byte changes; this is the
prototype behind Documentation/DCF_FIELD_USE.md's adaptive-mesh section.
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

import meshlab_core as mesh  # noqa: E402


class TestEgressRoutes(unittest.TestCase):
    def test_line_orients_to_single_uplink(self):
        # 0-1-2-3-4 line, each link 10ms; node 4 has the only uplink.
        edges = [(0, 1, 10), (1, 2, 10), (2, 3, 10), (3, 4, 10)]
        r = mesh.egress_routes(5, edges, [(4, 5)])
        self.assertEqual([e["next_hop"] for e in r], [1, 2, 3, 4, 4])  # all point at 4
        self.assertEqual([e["uplink"] for e in r], [4] * 5)
        self.assertEqual([e["egress_dist"] for e in r], [45, 35, 25, 15, 5])  # cost drops

    def test_uplink_egresses_locally(self):
        r = mesh.egress_routes(3, [(0, 1, 10), (1, 2, 10)], [(0, 3)])
        self.assertEqual(r[0]["next_hop"], 0)        # node 0 is the uplink -> egress here
        self.assertEqual(r[0]["egress_dist"], 3)     # just its backhaul cost
        self.assertEqual(r[0]["uplink"], 0)

    def test_cheaper_uplink_flips_orientation(self):
        # add a cheap uplink at node 0; near nodes should re-orient to it, far ones stay.
        edges = [(0, 1, 10), (1, 2, 10), (2, 3, 10), (3, 4, 10)]
        r = mesh.egress_routes(5, edges, [(4, 5), (0, 1)])
        self.assertEqual([e["uplink"] for e in r], [0, 0, 0, 4, 4])
        self.assertEqual(r[0]["next_hop"], 0)        # node 0 egresses locally
        self.assertEqual(r[1]["next_hop"], 0)        # node 1 -> node 0
        self.assertEqual(r[3]["next_hop"], 4)        # node 3 -> node 4 (the far uplink)

    def test_unreachable_nodes_get_no_route(self):
        # uplink sits on an isolated node; the others have no path to any uplink.
        r = mesh.egress_routes(5, [(0, 1, 10), (1, 2, 10), (2, 3, 10)], [(4, 5)])
        for v in range(4):
            self.assertEqual(r[v]["next_hop"], mesh.NO_HOP)
            self.assertEqual(r[v]["egress_dist"], mesh.INF)
            self.assertEqual(r[v]["uplink"], mesh.NO_HOP)
        self.assertEqual(r[4]["uplink"], 4)          # the uplink still egresses itself

    def test_no_uplinks_means_no_egress(self):
        r = mesh.egress_routes(3, [(0, 1, 10), (1, 2, 10)], [])
        self.assertTrue(all(e["next_hop"] == mesh.NO_HOP for e in r))

    def test_backhaul_cost_breaks_a_distance_tie(self):
        # two uplinks equidistant by mesh RTT; the cheaper backhaul wins.
        edges = [(0, 1, 10), (1, 2, 10)]            # node 1 is 10ms from both 0 and 2
        r = mesh.egress_routes(3, edges, [(0, 7), (2, 3)])
        self.assertEqual(r[1]["uplink"], 2)          # 10+3 < 10+7
        self.assertEqual(r[1]["next_hop"], 2)


if __name__ == "__main__":
    unittest.main()
