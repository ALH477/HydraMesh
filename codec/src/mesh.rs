// SPDX-License-Identifier: LGPL-3.0-only
//! DCF self-healing mesh algorithms — the certified, deterministic layer,
//! value-identical to `python/MCP/meshlab_core.py`, `codec/demod_mesh.h`, and
//! `go/mesh/mesh.go`. Pure integer functions over small-int node ids (0..n-1).
//!
//! The node runtime drives these from live PING/PONG + the DCF-Mesh control
//! adapter; the runtime's timing is integration-tested, but these functions are
//! the contract — pinned by `Documentation/mesh_vectors.json`.

pub const HEALTHY: i32 = 0;
pub const DEGRADED: i32 = 1;
pub const UNREACHABLE: i32 = 2;
pub const LEAF: i32 = 0;
pub const RELAY: i32 = 1;
pub const MASTER: i32 = 2;
pub const INF: i32 = 0x3FFF_FFFF;
pub const NO_HOP: i32 = -1;
pub const ISOLATED: i32 = -1;

/// Liveness FSM: events (1 = ok, 0 = timeout) → status. Sticky UNREACHABLE.
pub fn peer_status(events: &[u8], fail_threshold: i32, ok_threshold: i32) -> i32 {
    let (mut status, mut cf, mut cs) = (HEALTHY, 0i32, 0i32);
    for &e in events {
        if e != 0 { cs += 1; cf = 0; } else { cf += 1; cs = 0; }
        if cf >= fail_threshold {
            status = UNREACHABLE;
        } else if status == UNREACHABLE {
            if cs >= ok_threshold { status = HEALTHY; }
        } else if cf >= 1 {
            status = DEGRADED;
        } else {
            status = HEALTHY;
        }
    }
    status
}

/// RTT grouping: floor(rtt/threshold), or ISOLATED if unreachable.
pub fn group_of(rtt_ms: i32, threshold_ms: i32, status: i32) -> i32 {
    if status == UNREACHABLE || rtt_ms >= INF { ISOLATED } else { rtt_ms / threshold_ms }
}

/// RTT-weighted Dijkstra. `edges` are undirected `[u, v, w]`. Returns
/// `(dist, next_hop)` of length `n` (next_hop = first hop from source; NO_HOP for
/// source/unreachable). Deterministic tie-break: smaller predecessor id.
pub fn dijkstra(n: usize, edges: &[[i32; 3]], source: usize) -> (Vec<i32>, Vec<i32>) {
    let mut dist = vec![INF; n];
    let mut prev = vec![NO_HOP; n];
    let mut done = vec![false; n];
    dist[source] = 0;
    for _ in 0..n {
        let mut u: i32 = NO_HOP;
        for i in 0..n {
            if !done[i] && dist[i] < INF && (u == NO_HOP || dist[i] < dist[u as usize]) {
                u = i as i32;
            }
        }
        if u == NO_HOP { break; }
        let u = u as usize;
        done[u] = true;
        for e in edges {
            let (a, b, w) = (e[0], e[1], e[2]);
            let v = if a as usize == u { b } else if b as usize == u { a } else { continue };
            let v = v as usize;
            let nd = dist[u] + w;
            if nd < dist[v] || (nd == dist[v] && (u as i32) < prev[v]) {
                dist[v] = nd;
                prev[v] = u as i32;
            }
        }
    }
    let mut next_hop = vec![NO_HOP; n];
    for v in 0..n {
        if v == source || dist[v] >= INF { continue; }
        let mut hop = v;
        while prev[hop] != source as i32 { hop = prev[hop] as usize; }
        next_hop[v] = hop as i32;
    }
    (dist, next_hop)
}

/// Route failover ordering. `cand` are `[route_id, status, rtt]`. Drops
/// UNREACHABLE, orders by (status, rtt, route_id) ascending; returns route_ids.
pub fn select_routes(cand: &[[i32; 3]]) -> Vec<i32> {
    let mut usable: Vec<[i32; 3]> = cand.iter().copied().filter(|c| c[1] != UNREACHABLE).collect();
    usable.sort_by_key(|c| (c[1], c[2], c[0]));
    usable.iter().map(|c| c[0]).collect()
}

/// Master election + roles. `edges` are healthy `[u, v, w]` links. Returns
/// `(master, roles)`: max healthy degree, tie-break min avg RTT then min id.
pub fn elect(n: usize, edges: &[[i32; 3]], relay_min_degree: i32) -> (i32, Vec<i32>) {
    let mut deg = vec![0i32; n];
    let mut sum = vec![0i32; n];
    for e in edges {
        let (u, v, w) = (e[0] as usize, e[1] as usize, e[2]);
        deg[u] += 1; sum[u] += w; deg[v] += 1; sum[v] += w;
    }
    let avg: Vec<i32> = (0..n).map(|i| if deg[i] != 0 { sum[i] / deg[i] } else { INF }).collect();
    let mut master = 0usize;
    for i in 1..n {
        if deg[i] > deg[master] || (deg[i] == deg[master] && avg[i] < avg[master]) {
            master = i;
        }
    }
    let mut roles: Vec<i32> = (0..n).map(|i| if deg[i] >= relay_min_degree { RELAY } else { LEAF }).collect();
    roles[master] = MASTER;
    (master as i32, roles)
}

// ── DCF-Mesh control adapter (REPORT / ROLE) ──────────────────────────────────
// Carried as the payload of a ProtoMessage MsgMesh (=11). Big-endian, byte-exact.
pub const MESH_REPORT: u8 = 0;
pub const MESH_ROLE: u8 = 1;
pub const MESH_VERSION: u8 = 1;
pub const MSG_MESH: u8 = 11;

/// Serialise a REPORT. `peers` are `[peer_id, status, rtt]`.
pub fn pack_report(node_id: i32, peers: &[[i32; 3]]) -> Vec<u8> {
    let mut out = vec![MESH_REPORT, MESH_VERSION,
                       ((node_id >> 8) & 0xFF) as u8, (node_id & 0xFF) as u8,
                       (peers.len() & 0xFF) as u8];
    for p in peers {
        out.push(((p[0] >> 8) & 0xFF) as u8); out.push((p[0] & 0xFF) as u8);
        out.push((p[1] & 0xFF) as u8);
        out.push(((p[2] >> 8) & 0xFF) as u8); out.push((p[2] & 0xFF) as u8);
    }
    out
}

/// Parse a REPORT into `(node_id, peers)`, or `None` on bad input.
pub fn unpack_report(buf: &[u8]) -> Option<(i32, Vec<[i32; 3]>)> {
    if buf.len() < 5 || buf[0] != MESH_REPORT || buf[1] != MESH_VERSION {
        return None;
    }
    let node_id = ((buf[2] as i32) << 8) | buf[3] as i32;
    let n = buf[4] as usize;
    if buf.len() < 5 + 5 * n {
        return None;
    }
    let mut peers = Vec::with_capacity(n);
    let mut off = 5;
    for _ in 0..n {
        peers.push([
            ((buf[off] as i32) << 8) | buf[off + 1] as i32,
            buf[off + 2] as i32,
            ((buf[off + 3] as i32) << 8) | buf[off + 4] as i32,
        ]);
        off += 5;
    }
    Some((node_id, peers))
}

/// Serialise a ROLE (7 bytes).
pub fn pack_role(node_id: i32, role: i32, master_id: i32) -> Vec<u8> {
    vec![MESH_ROLE, MESH_VERSION,
         ((node_id >> 8) & 0xFF) as u8, (node_id & 0xFF) as u8, (role & 0xFF) as u8,
         ((master_id >> 8) & 0xFF) as u8, (master_id & 0xFF) as u8]
}

/// Parse a ROLE into `(node_id, role, master_id)`, or `None` on bad input.
pub fn unpack_role(buf: &[u8]) -> Option<(i32, i32, i32)> {
    if buf.len() < 7 || buf[0] != MESH_ROLE || buf[1] != MESH_VERSION {
        return None;
    }
    Some((((buf[2] as i32) << 8) | buf[3] as i32, buf[4] as i32,
          ((buf[5] as i32) << 8) | buf[6] as i32))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn smoke() {
        assert_eq!(peer_status(&[0, 0, 0], 3, 2), UNREACHABLE);
        let (dist, nh) = dijkstra(3, &[[0, 1, 10], [1, 2, 10], [0, 2, 50]], 0);
        assert_eq!(dist, vec![0, 10, 20]);
        assert_eq!(nh, vec![NO_HOP, 1, 1]);
        let (m, roles) = elect(4, &[[1, 0, 10], [1, 2, 10], [1, 3, 10]], 2);
        assert_eq!(m, 1);
        assert_eq!(roles, vec![LEAF, MASTER, LEAF, LEAF]);
    }
}
