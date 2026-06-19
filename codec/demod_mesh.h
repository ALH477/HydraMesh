/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * demod_mesh.h — DCF self-healing mesh algorithms (certified, deterministic),
 * byte/value-identical to python/MCP/meshlab_core.py and codec/src/mesh.rs and
 * go/mesh/mesh.go. Pure integer functions over small-int node ids (0..n-1); the
 * node runtime drives them from live PING/PONG + the DCF-Mesh control adapter.
 *
 *   dcf_mesh_peer_status  — liveness FSM (health-check sequence -> status)
 *   dcf_mesh_group_of     — RTT-based grouping
 *   dcf_mesh_dijkstra     — RTT-weighted shortest paths -> dist[] + next_hop[]
 *   dcf_mesh_select_routes— failover ordering of candidate routes
 *   dcf_mesh_elect        — master election + role assignment (AUTO mode)
 */
#ifndef DCF_DEMOD_MESH_H
#define DCF_DEMOD_MESH_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DCF_MESH_HEALTHY     0
#define DCF_MESH_DEGRADED    1
#define DCF_MESH_UNREACHABLE 2
#define DCF_MESH_LEAF        0
#define DCF_MESH_RELAY       1
#define DCF_MESH_MASTER      2
#define DCF_MESH_INF         0x3FFFFFFF
#define DCF_MESH_NO_HOP      (-1)
#define DCF_MESH_ISOLATED    (-1)
#define DCF_MESH_MAX_NODES   32

/* 1. liveness FSM: events[] (1=ok, 0=timeout) -> status. */
static inline int dcf_mesh_peer_status(const uint8_t *events, int n,
                                       int fail_threshold, int ok_threshold) {
    int status = DCF_MESH_HEALTHY, cf = 0, cs = 0;
    for (int i = 0; i < n; i++) {
        if (events[i]) { cs++; cf = 0; } else { cf++; cs = 0; }
        if (cf >= fail_threshold)            status = DCF_MESH_UNREACHABLE;
        else if (status == DCF_MESH_UNREACHABLE) { if (cs >= ok_threshold) status = DCF_MESH_HEALTHY; }
        else if (cf >= 1)                    status = DCF_MESH_DEGRADED;
        else                                 status = DCF_MESH_HEALTHY;
    }
    return status;
}

/* 2. RTT grouping: floor(rtt/threshold), or ISOLATED if unreachable. */
static inline int dcf_mesh_group_of(int rtt_ms, int threshold_ms, int status) {
    if (status == DCF_MESH_UNREACHABLE || rtt_ms >= DCF_MESH_INF) return DCF_MESH_ISOLATED;
    return rtt_ms / threshold_ms;
}

/* 3. RTT-weighted Dijkstra. edges[n_edges][3] = {u,v,w} undirected. Writes
 * dist[n] and next_hop[n] (first hop from source; NO_HOP for source/unreachable). */
static inline void dcf_mesh_dijkstra(int n, const int edges[][3], int n_edges,
                                     int source, int *dist, int *next_hop) {
    int prev[DCF_MESH_MAX_NODES];
    uint8_t done[DCF_MESH_MAX_NODES];
    for (int i = 0; i < n; i++) { dist[i] = DCF_MESH_INF; prev[i] = DCF_MESH_NO_HOP; done[i] = 0; }
    dist[source] = 0;
    for (int it = 0; it < n; it++) {
        int u = DCF_MESH_NO_HOP;
        for (int i = 0; i < n; i++)
            if (!done[i] && dist[i] < DCF_MESH_INF && (u == DCF_MESH_NO_HOP || dist[i] < dist[u])) u = i;
        if (u == DCF_MESH_NO_HOP) break;
        done[u] = 1;
        for (int e = 0; e < n_edges; e++) {
            int a = edges[e][0], b = edges[e][1], w = edges[e][2];
            int v;
            if (a == u) v = b; else if (b == u) v = a; else continue;
            int nd = dist[u] + w;
            if (nd < dist[v] || (nd == dist[v] && u < prev[v])) { dist[v] = nd; prev[v] = u; }
        }
    }
    for (int v = 0; v < n; v++) {
        next_hop[v] = DCF_MESH_NO_HOP;
        if (v == source || dist[v] >= DCF_MESH_INF) continue;
        int hop = v;
        while (prev[hop] != source) hop = prev[hop];
        next_hop[v] = hop;
    }
}

/* 4. route failover ordering. cand[n_cand][3] = {route_id, status, rtt}. Writes
 * the ordered route ids into out[] and returns the count (UNREACHABLE dropped). */
static inline int dcf_mesh_select_routes(const int cand[][3], int n_cand, int *out) {
    int idx[64], m = 0;
    for (int i = 0; i < n_cand; i++) if (cand[i][1] != DCF_MESH_UNREACHABLE) idx[m++] = i;
    /* insertion sort by (status, rtt, route_id) ascending */
    for (int i = 1; i < m; i++) {
        int j = i, t = idx[i];
        while (j > 0) {
            const int *a = cand[idx[j - 1]], *b = cand[t];
            int lt = (b[1] < a[1]) || (b[1] == a[1] && (b[2] < a[2] ||
                     (b[2] == a[2] && b[0] < a[0])));
            if (!lt) break;
            idx[j] = idx[j - 1]; j--;
        }
        idx[j] = t;
    }
    for (int i = 0; i < m; i++) out[i] = cand[idx[i]][0];
    return m;
}

/* 5. master election + roles. edges[n_edges][3] = {u,v,w} healthy links. Writes
 * roles[n] and returns the elected master id. */
static inline int dcf_mesh_elect(int n, const int edges[][3], int n_edges,
                                 int relay_min_degree, int *roles) {
    int deg[DCF_MESH_MAX_NODES], sum[DCF_MESH_MAX_NODES], avg[DCF_MESH_MAX_NODES];
    for (int i = 0; i < n; i++) { deg[i] = 0; sum[i] = 0; }
    for (int e = 0; e < n_edges; e++) {
        int u = edges[e][0], v = edges[e][1], w = edges[e][2];
        deg[u]++; sum[u] += w; deg[v]++; sum[v] += w;
    }
    for (int i = 0; i < n; i++) avg[i] = deg[i] ? sum[i] / deg[i] : DCF_MESH_INF;
    int master = 0;
    for (int i = 1; i < n; i++)
        if (deg[i] > deg[master] || (deg[i] == deg[master] && avg[i] < avg[master])) master = i;
    for (int i = 0; i < n; i++) roles[i] = (deg[i] >= relay_min_degree) ? DCF_MESH_RELAY : DCF_MESH_LEAF;
    roles[master] = DCF_MESH_MASTER;
    return master;
}

/* ── DCF-Mesh control adapter (REPORT / ROLE) ──────────────────────────────────
 * Carried as the payload of a ProtoMessage MsgMesh (=11). Big-endian, byte-exact.
 *   REPORT: type=0 | ver=1 | node_id(2) | n_peers(1) | n x [pid(2)|status(1)|rtt(2)]
 *   ROLE  : type=1 | ver=1 | node_id(2) | role(1) | master_id(2) */
#define DCF_MESH_REPORT  0
#define DCF_MESH_ROLE    1
#define DCF_MESH_VERSION 1
/* (the ProtoMessage transport type DCF_MSG_MESH=11 lives in the node layer, dcf_proto.h) */

/* Serialise a REPORT; peers[n_peers][3] = {peer_id, status, rtt}. Returns length. */
static inline int dcf_mesh_pack_report(int node_id, const int peers[][3], int n_peers, uint8_t *out) {
    out[0] = DCF_MESH_REPORT; out[1] = DCF_MESH_VERSION;
    out[2] = (uint8_t)((node_id >> 8) & 0xFF); out[3] = (uint8_t)(node_id & 0xFF);
    out[4] = (uint8_t)(n_peers & 0xFF);
    int off = 5;
    for (int i = 0; i < n_peers; i++) {
        out[off++] = (uint8_t)((peers[i][0] >> 8) & 0xFF); out[off++] = (uint8_t)(peers[i][0] & 0xFF);
        out[off++] = (uint8_t)(peers[i][1] & 0xFF);
        out[off++] = (uint8_t)((peers[i][2] >> 8) & 0xFF); out[off++] = (uint8_t)(peers[i][2] & 0xFF);
    }
    return off;
}

/* Parse a REPORT into node_id + peers[][3]; returns n_peers, or -1 on bad input. */
static inline int dcf_mesh_unpack_report(const uint8_t *buf, int len, int *node_id, int peers[][3]) {
    if (len < 5 || buf[0] != DCF_MESH_REPORT || buf[1] != DCF_MESH_VERSION) return -1;
    *node_id = (buf[2] << 8) | buf[3];
    int n = buf[4];
    if (len < 5 + 5 * n) return -1;
    int off = 5;
    for (int i = 0; i < n; i++) {
        peers[i][0] = (buf[off] << 8) | buf[off + 1];
        peers[i][1] = buf[off + 2];
        peers[i][2] = (buf[off + 3] << 8) | buf[off + 4];
        off += 5;
    }
    return n;
}

/* Serialise a ROLE (7 bytes). */
static inline int dcf_mesh_pack_role(int node_id, int role, int master_id, uint8_t *out) {
    out[0] = DCF_MESH_ROLE; out[1] = DCF_MESH_VERSION;
    out[2] = (uint8_t)((node_id >> 8) & 0xFF); out[3] = (uint8_t)(node_id & 0xFF);
    out[4] = (uint8_t)(role & 0xFF);
    out[5] = (uint8_t)((master_id >> 8) & 0xFF); out[6] = (uint8_t)(master_id & 0xFF);
    return 7;
}

/* Parse a ROLE; returns true on success. */
static inline bool dcf_mesh_unpack_role(const uint8_t *buf, int len, int *node_id, int *role, int *master_id) {
    if (len < 7 || buf[0] != DCF_MESH_ROLE || buf[1] != DCF_MESH_VERSION) return false;
    *node_id = (buf[2] << 8) | buf[3];
    *role = buf[4];
    *master_id = (buf[5] << 8) | buf[6];
    return true;
}

static inline int dcf_mesh_msg_type(const uint8_t *buf, int len) { return len > 0 ? buf[0] : -1; }

#endif /* DCF_DEMOD_MESH_H */
