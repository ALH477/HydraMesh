#!/usr/bin/env python3
# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF uplink-oriented mesh demo — "the mesh points at the Starlink".

A heterogeneous field mesh usually has one node with backhaul (Starlink, cellular, a ham
gateway). DCF's RTT-weighted routing orients every node's egress toward the nearest /
cheapest uplink, with no new wire format: `meshlab_core.egress_routes` models the uplink
as a virtual sink and reuses the certified RTT Dijkstra (see Documentation/DCF_FIELD_USE.md).

Run: python3 python/modem/uplink_demo.py
"""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))
import meshlab_core as mesh  # noqa: E402


def show(title, n, edges, uplinks, names):
    routes = mesh.egress_routes(n, edges, uplinks)
    up = {u for (u, _c) in uplinks}
    print(f"\n{title}")
    print("  link RTTs:", ", ".join(f"{names[u]}-{names[v]}={w}ms" for (u, v, w) in edges))
    print("  uplinks  :", ", ".join(f"{names[u]} (backhaul {c}ms)" for (u, c) in uplinks))
    for v in range(n):
        e = routes[v]
        if e["next_hop"] == mesh.NO_HOP:
            print(f"    {names[v]:<10} -> (no path to any uplink)")
            continue
        hop, dist, u = e["next_hop"], e["egress_dist"], e["uplink"]
        tag = "  [UPLINK]" if v in up and hop == v else ""
        via = "egress here" if hop == v else f"via {names[hop]}"
        print(f"    {names[v]:<10} -> {via:<14} ({dist}ms to {names[u]} uplink){tag}")


def main():
    # SAR scenario: a base camp with Starlink; two field teams fan up a valley. The mesh
    # self-orients so every handheld's traffic egresses toward base by lowest total RTT.
    names = ["BaseCamp", "Relay-N", "TeamA", "Relay-S", "TeamB"]
    edges = [(0, 1, 12), (1, 2, 20), (0, 3, 15), (3, 4, 25), (1, 3, 30)]
    show("SAR: BaseCamp has Starlink — teams orient egress to base",
         5, edges, [(0, 5)], names)

    # Disaster aid: base's link drops; a TeamB member raises a cellular hotspot. The
    # orientation flips — nearby nodes now egress toward the new uplink automatically.
    show("Disaster: base uplink lost, TeamB raises cellular — mesh re-orients",
         5, edges, [(4, 8)], names)

    # Two uplinks coexist (Starlink at base + cellular at TeamB): each node picks its
    # cheapest egress; the mesh partitions itself around the two gateways.
    show("Dual uplink: Starlink@BaseCamp + cellular@TeamB — split egress",
         5, edges, [(0, 5), (4, 8)], names)


if __name__ == "__main__":
    main()
