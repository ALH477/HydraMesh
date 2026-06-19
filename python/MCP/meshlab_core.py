# SPDX-License-Identifier: LGPL-3.0-only
"""DCF self-healing mesh — the certified, deterministic algorithm layer.

These are pure integer functions (no I/O, no deps): the *contract* for the mesh
runtime, the same way the wire codec and adapters are. The node runtime (Go/Rust/C)
drives them from live PING/PONG + a control adapter; the runtime's timing is
integration-tested, but THESE functions are byte/value-exact across languages,
pinned by Documentation/mesh_vectors.json.

Five primitives (mirroring dcf_design_spec.markdown §5.3/§5.5 and dcf_redundancy.h):
  1. peer_status   — per-peer liveness FSM from a health-check sequence.
  2. group_of      — RTT-based grouping (clusters peers by RTT / threshold).
  3. dijkstra      — RTT-weighted shortest paths -> dist[] + next_hop[].
  4. select_routes — order candidate routes to a recipient (failover order).
  5. elect         — deterministic master election + role assignment (AUTO mode).

All node/peer ids are small non-negative integers (0..N-1) so every binding agrees.
"""

# Peer status (matches DCFPeerStatus intent).
HEALTHY = 0
DEGRADED = 1
UNREACHABLE = 2

# Roles (matches AUTO-mode roles).
LEAF = 0
RELAY = 1
MASTER = 2

INF = 0x3FFFFFFF  # "unreachable" distance (fits 31 bits; identical in C/Rust/Go)
NO_HOP = -1
ISOLATED = -1     # group id for an unreachable peer


# ── 1. liveness / health FSM ──────────────────────────────────────────────────
def peer_status(events, fail_threshold, ok_threshold):
    """Fold a sequence of health checks (1 = ok, 0 = timeout) into a status.

    Sticky UNREACHABLE: once fail_threshold consecutive failures hit, the peer is
    UNREACHABLE until ok_threshold consecutive successes recover it. Between, a
    recent failure is DEGRADED; a clean run is HEALTHY.
    """
    status = HEALTHY
    cf = 0  # consecutive failures
    cs = 0  # consecutive successes
    for e in events:
        if e:
            cs += 1
            cf = 0
        else:
            cf += 1
            cs = 0
        if cf >= fail_threshold:
            status = UNREACHABLE
        elif status == UNREACHABLE:
            if cs >= ok_threshold:
                status = HEALTHY
            # else: remain UNREACHABLE (hysteresis)
        elif cf >= 1:
            status = DEGRADED
        else:
            status = HEALTHY
    return status


# ── 2. RTT grouping ───────────────────────────────────────────────────────────
def group_of(rtt_ms, threshold_ms, status):
    """Group id for a peer: floor(rtt/threshold), or ISOLATED if unreachable."""
    if status == UNREACHABLE or rtt_ms >= INF:
        return ISOLATED
    return rtt_ms // threshold_ms


# ── 3. RTT-weighted Dijkstra ──────────────────────────────────────────────────
def dijkstra(n, edges, source):
    """Shortest paths from `source` over an RTT-weighted graph.

    `edges` is a list of (u, v, w) undirected links (w = RTT ms, integer > 0).
    Returns (dist, next_hop) arrays of length n: dist[v] is the min total RTT
    (INF if unreachable); next_hop[v] is the first node id on the path from
    `source` (source itself -> NO_HOP; unreachable -> NO_HOP). Deterministic
    tie-break: on an equal-cost relaxation, prefer the smaller predecessor id.
    """
    adj = [[] for _ in range(n)]
    for u, v, w in edges:
        adj[u].append((v, w))
        adj[v].append((u, w))
    dist = [INF] * n
    prev = [NO_HOP] * n
    done = [False] * n
    dist[source] = 0
    for _ in range(n):
        # settle the unfinished node with the smallest (dist, id)
        u = NO_HOP
        for i in range(n):
            if not done[i] and dist[i] < INF and (u == NO_HOP or dist[i] < dist[u]):
                u = i
        if u == NO_HOP:
            break
        done[u] = True
        for v, w in adj[u]:
            nd = dist[u] + w
            if nd < dist[v] or (nd == dist[v] and u < prev[v]):
                dist[v] = nd
                prev[v] = u
    next_hop = [NO_HOP] * n
    for v in range(n):
        if v == source or dist[v] >= INF:
            continue
        hop = v
        while prev[hop] != source:
            hop = prev[hop]
        next_hop[v] = hop
    return dist, next_hop


# ── 4. route selection / failover ─────────────────────────────────────────────
def select_routes(candidates):
    """Order candidate routes to a recipient into a failover list.

    `candidates` is a list of (route_id, status, rtt_ms). UNREACHABLE routes are
    dropped; the rest are ordered by (status asc, rtt asc, route_id asc) — i.e.
    healthy-before-degraded, then lowest RTT, then lowest id. Returns the ordered
    list of route_ids (the first is the optimal route).
    """
    usable = [(s, r, rid) for (rid, s, r) in candidates if s != UNREACHABLE]
    usable.sort()  # (status, rtt, route_id)
    return [rid for (_s, _r, rid) in usable]


# ── 5. master election / role assignment (AUTO mode) ──────────────────────────
def elect(n, edges, relay_min_degree=2):
    """Deterministic master election + role assignment over a healthy topology.

    `edges` is the list of (u, v, w) healthy links (same shape as dijkstra). The
    master is the node maximizing healthy degree, tie-broken by smallest average
    RTT (floored int), then smallest id. Nodes with degree >= relay_min_degree are
    RELAY; the rest LEAF; the master is MASTER. Returns (master, roles[]).
    """
    deg = [0] * n
    rtt_sum = [0] * n
    for u, v, w in edges:
        deg[u] += 1; rtt_sum[u] += w
        deg[v] += 1; rtt_sum[v] += w
    avg = [(rtt_sum[i] // deg[i]) if deg[i] else INF for i in range(n)]
    master = 0
    for i in range(1, n):
        better = (deg[i] > deg[master] or
                  (deg[i] == deg[master] and avg[i] < avg[master]))
        if better:
            master = i
    roles = [RELAY if deg[i] >= relay_min_degree else LEAF for i in range(n)]
    roles[master] = MASTER
    return master, roles


# ── DCF-Mesh control adapter (REPORT / ROLE) ──────────────────────────────────
# Carried as the payload of a ProtoMessage MsgMesh (=11). Big-endian, byte-exact.
#   REPORT (node->master): type(1)=0 | ver(1)=1 | node_id(2) | n_peers(1) |
#                          n_peers x [ peer_id(2) | status(1) | rtt_ms(2) ]
#   ROLE   (master->node): type(1)=1 | ver(1)=1 | node_id(2) | role(1) | master_id(2)
MESH_REPORT = 0
MESH_ROLE = 1
MESH_VERSION = 1


def pack_report(node_id, peers):
    """peers: list of (peer_id, status, rtt_ms) -> bytes."""
    out = bytearray([MESH_REPORT, MESH_VERSION, (node_id >> 8) & 0xFF, node_id & 0xFF, len(peers) & 0xFF])
    for pid, status, rtt in peers:
        out += bytes([(pid >> 8) & 0xFF, pid & 0xFF, status & 0xFF, (rtt >> 8) & 0xFF, rtt & 0xFF])
    return bytes(out)


def unpack_report(buf):
    if len(buf) < 5 or buf[0] != MESH_REPORT or buf[1] != MESH_VERSION:
        raise ValueError("not a REPORT")
    node_id = (buf[2] << 8) | buf[3]
    n = buf[4]
    if len(buf) < 5 + 5 * n:
        raise ValueError("truncated REPORT")
    peers = []
    off = 5
    for _ in range(n):
        peers.append(((buf[off] << 8) | buf[off + 1], buf[off + 2], (buf[off + 3] << 8) | buf[off + 4]))
        off += 5
    return node_id, peers


def pack_role(node_id, role, master_id):
    return bytes([MESH_ROLE, MESH_VERSION, (node_id >> 8) & 0xFF, node_id & 0xFF,
                  role & 0xFF, (master_id >> 8) & 0xFF, master_id & 0xFF])


def unpack_role(buf):
    if len(buf) < 7 or buf[0] != MESH_ROLE or buf[1] != MESH_VERSION:
        raise ValueError("not a ROLE")
    return ((buf[2] << 8) | buf[3], buf[4], (buf[5] << 8) | buf[6])


def mesh_msg_type(buf):
    return buf[0] if buf else -1


if __name__ == "__main__":
    # FSM
    assert peer_status([1, 1, 1], 3, 2) == HEALTHY
    assert peer_status([1, 0], 3, 2) == DEGRADED
    assert peer_status([0, 0, 0], 3, 2) == UNREACHABLE
    assert peer_status([0, 0, 0, 1], 3, 2) == UNREACHABLE          # not yet recovered
    assert peer_status([0, 0, 0, 1, 1], 3, 2) == HEALTHY           # recovered
    # grouping
    assert group_of(35, 50, HEALTHY) == 0 and group_of(120, 50, HEALTHY) == 2
    assert group_of(35, 50, UNREACHABLE) == ISOLATED
    # dijkstra: 0-1(10) 1-2(10) 0-2(50) -> 0..2 via 1
    dist, nh = dijkstra(3, [(0, 1, 10), (1, 2, 10), (0, 2, 50)], 0)
    assert dist == [0, 10, 20] and nh == [NO_HOP, 1, 1]
    # routes: prefer healthy low-rtt
    assert select_routes([(7, DEGRADED, 5), (3, HEALTHY, 9), (4, UNREACHABLE, 1),
                          (5, HEALTHY, 2)]) == [5, 3, 7]
    # election: star center 1 is master
    m, roles = elect(4, [(1, 0, 10), (1, 2, 10), (1, 3, 10)])
    assert m == 1 and roles == [LEAF, MASTER, LEAF, LEAF]
    # control adapter round-trips
    rb = pack_report(5, [(1, HEALTHY, 10), (2, DEGRADED, 30), (3, UNREACHABLE, 9999)])
    assert unpack_report(rb) == (5, [(1, 0, 10), (2, 1, 30), (3, 2, 9999)])
    assert mesh_msg_type(rb) == MESH_REPORT
    lb = pack_role(7, RELAY, 5)
    assert unpack_role(lb) == (7, 1, 5) and mesh_msg_type(lb) == MESH_ROLE
    print("meshlab selftest: CERTIFIED (FSM + grouping + Dijkstra + routes + election + control)")
