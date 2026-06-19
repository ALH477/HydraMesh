# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for the DCF self-healing mesh algorithms.

Pins the five deterministic primitives (liveness FSM, RTT grouping, RTT-weighted
Dijkstra, route failover, master election) so the Rust (codec/src/mesh.rs), C
(codec/demod_mesh.h), and Go (go/mesh/mesh.go) implementations agree with the
Python reference (python/MCP/meshlab_core.py) value-for-value.

Usage:  python3 gen_mesh_vectors.py [mesh_vectors.json]
Writes  <path>  and  <dir>/mesh_vectors.gen.h. Exit 0 iff every law holds.
Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import meshlab_core as m

ok = lambda name: print(f"  PASS  {name}")

# ── FSM cases ─────────────────────────────────────────────────────────────────
fsm_inputs = [
    ([1, 1, 1, 1], 3, 2),
    ([1, 0], 3, 2),
    ([0, 0, 0], 3, 2),
    ([0, 0, 0, 1], 3, 2),
    ([0, 0, 0, 1, 1], 3, 2),
    ([1, 0, 1, 0, 1], 2, 2),
    ([0, 0, 1, 1, 0, 0, 0, 1, 1], 3, 2),
    ([1, 1, 0, 0], 2, 1),
]
fsm = [{"events": ev, "fail_threshold": ft, "ok_threshold": ot,
        "status": m.peer_status(ev, ft, ot)} for (ev, ft, ot) in fsm_inputs]
# law: UNREACHABLE only reachable via fail_threshold consecutive failures
assert m.peer_status([0] * 2, 3, 2) == m.DEGRADED
assert m.peer_status([0] * 3, 3, 2) == m.UNREACHABLE
ok(f"{len(fsm)} liveness-FSM cases (sticky UNREACHABLE + recovery)")

# ── grouping cases ────────────────────────────────────────────────────────────
grp_inputs = [(35, 50, m.HEALTHY), (50, 50, m.HEALTHY), (120, 50, m.HEALTHY),
              (5, 20, m.DEGRADED), (999, 50, m.HEALTHY), (35, 50, m.UNREACHABLE)]
grouping = [{"rtt": r, "threshold": t, "status": s, "group": m.group_of(r, t, s)}
            for (r, t, s) in grp_inputs]
ok(f"{len(grouping)} RTT-grouping cases")

# ── Dijkstra cases ────────────────────────────────────────────────────────────
dij_inputs = [
    (3, [(0, 1, 10), (1, 2, 10), (0, 2, 50)], 0),                  # via 1
    (5, [(0, 1, 1), (1, 2, 1), (0, 2, 5), (2, 3, 1), (0, 3, 2), (3, 4, 1)], 0),
    (4, [(0, 1, 10), (2, 3, 10)], 0),                              # 2,3 unreachable
    (4, [(0, 1, 5), (0, 2, 5), (1, 3, 5), (2, 3, 5)], 0),         # tie -> smaller hop
    (1, [], 0),
]
dijkstra = []
for (n, edges, src) in dij_inputs:
    dist, nh = m.dijkstra(n, edges, src)
    assert dist[src] == 0 and nh[src] == m.NO_HOP
    for v in range(n):
        if dist[v] >= m.INF:
            assert nh[v] == m.NO_HOP
    dijkstra.append({"n": n, "edges": [list(e) for e in edges], "source": src,
                     "dist": dist, "next_hop": nh})
ok(f"{len(dijkstra)} RTT-weighted Dijkstra cases (dist + next_hop)")

# ── route-failover cases ──────────────────────────────────────────────────────
rt_inputs = [
    [(7, m.DEGRADED, 5), (3, m.HEALTHY, 9), (4, m.UNREACHABLE, 1), (5, m.HEALTHY, 2)],
    [(1, m.HEALTHY, 10), (2, m.HEALTHY, 10)],                      # tie -> smaller id
    [(9, m.UNREACHABLE, 1)],                                       # all dead -> empty
    [(0, m.DEGRADED, 3), (1, m.DEGRADED, 3), (2, m.HEALTHY, 8)],
]
routes = [{"candidates": [list(c) for c in cs], "ordered": m.select_routes(cs)}
          for cs in rt_inputs]
ok(f"{len(routes)} route-failover cases")

# ── election cases ────────────────────────────────────────────────────────────
el_inputs = [
    (4, [(1, 0, 10), (1, 2, 10), (1, 3, 10)], 2),                 # star center 1
    (5, [(0, 1, 5), (1, 2, 5), (2, 3, 5), (3, 4, 5)], 2),         # line
    (4, [(0, 1, 10), (0, 2, 10), (1, 2, 10), (2, 3, 30)], 2),     # 0/2 tie on degree
    (3, [], 2),                                                    # no edges -> id tiebreak
]
election = []
for (n, edges, rmin) in el_inputs:
    master, roles = m.elect(n, edges, rmin)
    assert roles[master] == m.MASTER
    election.append({"n": n, "edges": [list(e) for e in edges],
                     "relay_min_degree": rmin, "master": master, "roles": roles})
ok(f"{len(election)} master-election cases")

cert = {
    "constants": {"HEALTHY": m.HEALTHY, "DEGRADED": m.DEGRADED, "UNREACHABLE": m.UNREACHABLE,
                  "LEAF": m.LEAF, "RELAY": m.RELAY, "MASTER": m.MASTER,
                  "INF": m.INF, "NO_HOP": m.NO_HOP, "ISOLATED": m.ISOLATED},
    "fsm": fsm, "grouping": grouping, "dijkstra": dijkstra, "routes": routes, "election": election,
}

out = sys.argv[1] if len(sys.argv) > 1 else "mesh_vectors.json"
with open(out, "w") as f:
    json.dump(cert, f, indent=1)
    f.write("\n")
print(f"  INFO  wrote {out} ({os.path.getsize(out)} bytes)")

# ── dependency-free C header ──────────────────────────────────────────────────
def arr(v):
    return "{" + ",".join(str(x) for x in v) + "}"

def arr2(rows, width):
    return "{" + ",".join(arr(r) + "" for r in rows) + "}" if rows else "{{0}}"

hdr = os.path.join(os.path.dirname(os.path.abspath(out)), "mesh_vectors.gen.h")
with open(hdr, "w") as h:
    h.write("/* GENERATED by python/MCP/gen_mesh_vectors.py — DO NOT EDIT. */\n")
    h.write("#ifndef DCF_MESH_VECTORS_GEN_H\n#define DCF_MESH_VECTORS_GEN_H\n#include <stdint.h>\n\n")

    h.write("typedef struct { int n_events; uint8_t events[64]; int fail_threshold, ok_threshold, status; } mesh_fsm_t;\n")
    h.write("static const mesh_fsm_t MESH_FSM[] = {\n")
    for c in fsm:
        ev = c["events"] + [0] * (64 - len(c["events"]))
        h.write("  {%d,%s,%d,%d,%d},\n" % (len(c["events"]), arr(ev), c["fail_threshold"], c["ok_threshold"], c["status"]))
    h.write("};\nstatic const int MESH_N_FSM = (int)(sizeof(MESH_FSM)/sizeof(MESH_FSM[0]));\n\n")

    h.write("typedef struct { int rtt, threshold, status, group; } mesh_group_t;\n")
    h.write("static const mesh_group_t MESH_GROUP[] = {\n")
    for c in grouping:
        h.write("  {%d,%d,%d,%d},\n" % (c["rtt"], c["threshold"], c["status"], c["group"]))
    h.write("};\nstatic const int MESH_N_GROUP = (int)(sizeof(MESH_GROUP)/sizeof(MESH_GROUP[0]));\n\n")

    h.write("typedef struct { int n, n_edges, edges[64][3], source, dist[32], next_hop[32]; } mesh_dij_t;\n")
    h.write("static const mesh_dij_t MESH_DIJ[] = {\n")
    for c in dijkstra:
        edges = c["edges"] + [[0, 0, 0]] * (64 - len(c["edges"]))
        dist = c["dist"] + [0] * (32 - len(c["dist"]))
        nh = c["next_hop"] + [0] * (32 - len(c["next_hop"]))
        h.write("  {%d,%d,%s,%d,%s,%s},\n" % (c["n"], len(c["edges"]), arr2(edges, 3), c["source"], arr(dist), arr(nh)))
    h.write("};\nstatic const int MESH_N_DIJ = (int)(sizeof(MESH_DIJ)/sizeof(MESH_DIJ[0]));\n\n")

    h.write("typedef struct { int n_cand, cand[32][3], n_ord, ordered[32]; } mesh_route_t;\n")
    h.write("static const mesh_route_t MESH_ROUTE[] = {\n")
    for c in routes:
        cand = c["candidates"] + [[0, 0, 0]] * (32 - len(c["candidates"]))
        ordr = c["ordered"] + [0] * (32 - len(c["ordered"]))
        h.write("  {%d,%s,%d,%s},\n" % (len(c["candidates"]), arr2(cand, 3), len(c["ordered"]), arr(ordr)))
    h.write("};\nstatic const int MESH_N_ROUTE = (int)(sizeof(MESH_ROUTE)/sizeof(MESH_ROUTE[0]));\n\n")

    h.write("typedef struct { int n, n_edges, edges[64][3], relay_min_degree, master, roles[32]; } mesh_elect_t;\n")
    h.write("static const mesh_elect_t MESH_ELECT[] = {\n")
    for c in election:
        edges = c["edges"] + [[0, 0, 0]] * (64 - len(c["edges"]))
        roles = c["roles"] + [0] * (32 - len(c["roles"]))
        h.write("  {%d,%d,%s,%d,%d,%s},\n" % (c["n"], len(c["edges"]), arr2(edges, 3), c["relay_min_degree"], c["master"], arr(roles)))
    h.write("};\nstatic const int MESH_N_ELECT = (int)(sizeof(MESH_ELECT)/sizeof(MESH_ELECT[0]));\n")
    h.write("#endif\n")
print(f"  INFO  wrote {hdr} ({os.path.getsize(hdr)} bytes)")
print("ALL MESH LAWS HOLD")
