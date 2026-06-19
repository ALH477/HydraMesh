/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * dcf_mesh_runtime.h — the C node's self-healing runtime: a peer table + per-peer
 * liveness driven by the certified algorithms (codec/demod_mesh.h) from live
 * PING/PONG + the DCF-Mesh control adapter (MsgMesh). Single-threaded: dcfnode's
 * start loop calls dcf_mesh_tick() on a ~1s timer between (timed) recvfrom()s.
 * Mirrors go/node/mesh_runtime.go. This is what finally makes the C SDK's
 * long-quarantined redundancy real.
 */
#ifndef DCF_MESH_RUNTIME_H
#define DCF_MESH_RUNTIME_H

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "demod_mesh.h"
#include "dcf_proto.h"

#define DCF_MESH_MAX_PEERS 32
#define DCF_MESH_WINDOW    5
#define DCF_MESH_FAIL_THR  3
#define DCF_MESH_OK_THR    2

typedef struct {
    int id;                 /* numeric peer node id            */
    struct sockaddr_in addr;
    bool answered;          /* PONG seen since the last tick    */
    uint8_t window[DCF_MESH_WINDOW];
    int wlen;
    int status;             /* DCF_MESH_HEALTHY/DEGRADED/UNREACHABLE */
    int rtt_ms;             /* last measured RTT                */
} dcf_mesh_peer_t;

typedef struct {
    int reporter;
    int peers[DCF_MESH_MAX_PEERS][3];
    int n_peers;
    struct sockaddr_in addr;
    bool valid;
} dcf_mesh_report_t;

typedef struct {
    int fd;                 /* the node's bound UDP socket      */
    char mode[8];           /* "p2p" | "auto" | "master"        */
    int node_id;
    int role;
    int master;
    int master_peer_id;     /* peer id to REPORT to (auto)      */
    int group_thr;
    dcf_mesh_peer_t peers[DCF_MESH_MAX_PEERS];
    int n_peers;
    dcf_mesh_report_t reports[DCF_MESH_MAX_PEERS];
    int n_reports;
    int tick;
} dcf_mesh_node_t;

static inline uint64_t dcf_mesh_now_us(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)tv.tv_sec * 1000000ull + (uint64_t)tv.tv_usec;
}

static inline const char *dcf_mesh_status_name(int s) {
    return s == DCF_MESH_HEALTHY ? "healthy" : s == DCF_MESH_DEGRADED ? "degraded" : "unreachable";
}
static inline const char *dcf_mesh_role_name(int r) {
    return r == DCF_MESH_MASTER ? "master" : r == DCF_MESH_RELAY ? "relay" : "leaf";
}

static inline void dcf_mesh_init(dcf_mesh_node_t *m, int fd, const char *mode,
                                 int node_id, int master_peer_id, int group_thr) {
    memset(m, 0, sizeof(*m));
    m->fd = fd;
    snprintf(m->mode, sizeof m->mode, "%s", mode);
    m->node_id = node_id;
    m->master_peer_id = master_peer_id;
    m->group_thr = group_thr;
    m->role = DCF_MESH_LEAF;
    if (strcmp(mode, "master") == 0) { m->role = DCF_MESH_MASTER; m->master = node_id; }
}

static inline bool dcf_mesh_add_peer(dcf_mesh_node_t *m, int id, const char *host, int port) {
    if (m->n_peers >= DCF_MESH_MAX_PEERS) return false;
    dcf_mesh_peer_t *p = &m->peers[m->n_peers++];
    p->id = id;
    memset(&p->addr, 0, sizeof p->addr);
    p->addr.sin_family = AF_INET;
    p->addr.sin_port = htons((uint16_t)port);
    inet_pton(AF_INET, host, &p->addr.sin_addr);
    p->status = DCF_MESH_HEALTHY;
    return true;
}

static inline dcf_mesh_peer_t *dcf_mesh_peer_by_id(dcf_mesh_node_t *m, int id) {
    for (int i = 0; i < m->n_peers; i++) if (m->peers[i].id == id) return &m->peers[i];
    return NULL;
}

static inline dcf_mesh_peer_t *dcf_mesh_peer_by_addr(dcf_mesh_node_t *m, const struct sockaddr_in *from) {
    for (int i = 0; i < m->n_peers; i++)
        if (m->peers[i].addr.sin_port == from->sin_port) return &m->peers[i];
    return NULL;
}

/* Send a ProtoMessage from the bound socket to a peer address. */
static inline void dcf_mesh_send(dcf_mesh_node_t *m, const struct sockaddr_in *to,
                                 uint8_t msg_type, const uint8_t *payload, uint32_t plen) {
    uint8_t buf[DCF_PROTO_HEADER_LEN + 512];
    size_t n = dcf_proto_serialize(msg_type, 0, dcf_mesh_now_us(), payload, plen, buf);
    sendto(m->fd, buf, n, 0, (const struct sockaddr *)to, sizeof(*to));
}

/* PONG path: mark the peer answered + record RTT. */
static inline void dcf_mesh_on_pong(dcf_mesh_node_t *m, const struct sockaddr_in *from, int rtt_ms) {
    dcf_mesh_peer_t *p = dcf_mesh_peer_by_addr(m, from);
    if (p) { p->answered = true; p->rtt_ms = rtt_ms; }
}

/* MsgMesh path: a master collects REPORTs; a node adopts a ROLE. */
static inline void dcf_mesh_on_control(dcf_mesh_node_t *m, const uint8_t *payload, int len,
                                       const struct sockaddr_in *from) {
    if (dcf_mesh_msg_type(payload, len) == DCF_MESH_REPORT) {
        int nid, peers[DCF_MESH_MAX_PEERS][3];
        int np = dcf_mesh_unpack_report(payload, len, &nid, peers);
        if (np < 0) return;
        dcf_mesh_report_t *r = NULL;
        for (int i = 0; i < m->n_reports; i++) if (m->reports[i].reporter == nid) { r = &m->reports[i]; break; }
        if (!r && m->n_reports < DCF_MESH_MAX_PEERS) r = &m->reports[m->n_reports++];
        if (!r) return;
        r->reporter = nid; r->n_peers = np; r->addr = *from; r->valid = true;
        for (int i = 0; i < np && i < DCF_MESH_MAX_PEERS; i++) memcpy(r->peers[i], peers[i], sizeof peers[i]);
    } else if (dcf_mesh_msg_type(payload, len) == DCF_MESH_ROLE) {
        int nid, role, master;
        if (dcf_mesh_unpack_role(payload, len, &nid, &role, &master) && nid == m->node_id) {
            if (m->role != role || m->master != master)
                fprintf(stderr, "mesh: role assigned -> %s (master %d)\n", dcf_mesh_role_name(role), master);
            m->role = role; m->master = master;
        }
    }
}

static inline int dcf_mesh_status_of(dcf_mesh_node_t *m, int id) {
    dcf_mesh_peer_t *p = dcf_mesh_peer_by_id(m, id);
    return p ? p->status : DCF_MESH_HEALTHY;
}

/* One tick: fold health, ping peers, run the AUTO/master loop, log status. */
static inline void dcf_mesh_tick(dcf_mesh_node_t *m) {
    /* 1. health: evaluate answered/timeout into the FSM, then ping */
    for (int i = 0; i < m->n_peers; i++) {
        dcf_mesh_peer_t *p = &m->peers[i];
        uint8_t ev = p->answered ? 1 : 0;
        p->answered = false;
        if (p->wlen < DCF_MESH_WINDOW) p->window[p->wlen++] = ev;
        else { memmove(p->window, p->window + 1, DCF_MESH_WINDOW - 1); p->window[DCF_MESH_WINDOW - 1] = ev; }
        p->status = dcf_mesh_peer_status(p->window, p->wlen, DCF_MESH_FAIL_THR, DCF_MESH_OK_THR);
        dcf_mesh_send(m, &p->addr, DCF_MSG_PING, NULL, 0);
    }

    /* 2. control */
    if (strcmp(m->mode, "auto") == 0) {
        /* REPORT this node's view to the master peer */
        int rep[DCF_MESH_MAX_PEERS][3];
        for (int i = 0; i < m->n_peers; i++) {
            rep[i][0] = m->peers[i].id; rep[i][1] = m->peers[i].status; rep[i][2] = m->peers[i].rtt_ms;
        }
        uint8_t payload[5 + 5 * DCF_MESH_MAX_PEERS];
        int plen = dcf_mesh_pack_report(m->node_id, rep, m->n_peers, payload);
        dcf_mesh_peer_t *mp = dcf_mesh_peer_by_id(m, m->master_peer_id);
        if (mp) dcf_mesh_send(m, &mp->addr, DCF_MSG_MESH, payload, (uint32_t)plen);

        /* decentralized failover: master Unreachable -> local re-election (lowest healthy id) */
        if (mp && dcf_mesh_status_of(m, m->master_peer_id) == DCF_MESH_UNREACHABLE) {
            int best = m->node_id;
            for (int i = 0; i < m->n_peers; i++)
                if (m->peers[i].status != DCF_MESH_UNREACHABLE && m->peers[i].id < best) best = m->peers[i].id;
            if (m->master != best) {
                fprintf(stderr, "mesh: master %d unreachable -> local re-election, new master %d\n", m->master, best);
                m->master = best;
                if (best == m->node_id) m->role = DCF_MESH_MASTER;
            }
        }
    } else if (strcmp(m->mode, "master") == 0) {
        /* assemble the reported topology (dedup undirected edges), elect, broadcast ROLE.
         * collect ids: self + reporters + their peers */
        int idbuf[DCF_MESH_MAX_PEERS * 2];
        int nids = 0;
        idbuf[nids++] = m->node_id;
        for (int i = 0; i < m->n_reports; i++) {
            if (!m->reports[i].valid) continue;
            int rep = m->reports[i].reporter;
            bool seen = false; for (int k = 0; k < nids; k++) if (idbuf[k] == rep) seen = true;
            if (!seen && nids < (int)(sizeof idbuf / sizeof idbuf[0])) idbuf[nids++] = rep;
            for (int j = 0; j < m->reports[i].n_peers; j++) {
                int pid = m->reports[i].peers[j][0];
                seen = false; for (int k = 0; k < nids; k++) if (idbuf[k] == pid) seen = true;
                if (!seen && nids < (int)(sizeof idbuf / sizeof idbuf[0])) idbuf[nids++] = pid;
            }
        }
        /* sort ids ascending for a deterministic mapping */
        for (int a = 0; a < nids; a++) for (int b = a + 1; b < nids; b++) if (idbuf[b] < idbuf[a]) { int t = idbuf[a]; idbuf[a] = idbuf[b]; idbuf[b] = t; }
        /* dedup edges (canonical a<b), mapped to 0..nids-1 indices */
        int edges[DCF_MESH_MAX_PEERS * DCF_MESH_MAX_PEERS][3]; int ne = 0;
        for (int i = 0; i < m->n_reports; i++) {
            if (!m->reports[i].valid) continue;
            int rep = m->reports[i].reporter;
            for (int j = 0; j < m->reports[i].n_peers; j++) {
                if (m->reports[i].peers[j][1] == DCF_MESH_UNREACHABLE) continue;
                int a = rep, b = m->reports[i].peers[j][0], w = m->reports[i].peers[j][2];
                if (a > b) { int t = a; a = b; b = t; }
                /* map to indices */
                int ai = -1, bi = -1;
                for (int k = 0; k < nids; k++) { if (idbuf[k] == a) ai = k; if (idbuf[k] == b) bi = k; }
                bool dup = false;
                for (int e = 0; e < ne; e++) if (edges[e][0] == ai && edges[e][1] == bi) { dup = true; if (w < edges[e][2]) edges[e][2] = w; break; }
                if (!dup && ne < (int)(sizeof edges / sizeof edges[0])) { edges[ne][0] = ai; edges[ne][1] = bi; edges[ne][2] = w; ne++; }
            }
        }
        int roles[DCF_MESH_MAX_PEERS * 2];
        int master_idx = dcf_mesh_elect(nids, edges, ne, 2, roles);
        int master_id = idbuf[master_idx];
        /* adopt own role + master */
        for (int k = 0; k < nids; k++) if (idbuf[k] == m->node_id) m->role = roles[k];
        m->master = master_id;
        /* broadcast ROLE to each reporter */
        for (int i = 0; i < m->n_reports; i++) {
            if (!m->reports[i].valid) continue;
            int rep = m->reports[i].reporter, idx = -1;
            for (int k = 0; k < nids; k++) if (idbuf[k] == rep) idx = k;
            if (idx < 0) continue;
            uint8_t payload[7];
            dcf_mesh_pack_role(rep, roles[idx], master_id, payload);
            dcf_mesh_send(m, &m->reports[i].addr, DCF_MSG_MESH, payload, 7);
        }
    }

    /* 3. periodic status log */
    if (++m->tick % 3 == 0) {
        char line[512]; int off = 0;
        off += snprintf(line + off, sizeof line - off, "mesh-status node=%d mode=%s role=%s master=%d peers=[",
                        m->node_id, m->mode, dcf_mesh_role_name(m->role), m->master);
        for (int i = 0; i < m->n_peers && off < (int)sizeof line - 32; i++) {
            int grp = dcf_mesh_group_of(m->peers[i].rtt_ms, m->group_thr, m->peers[i].status);
            off += snprintf(line + off, sizeof line - off, "%s%d:%s:g%d",
                            i ? " " : "", m->peers[i].id, dcf_mesh_status_name(m->peers[i].status), grp);
        }
        snprintf(line + off, sizeof line - off, "]");
        fprintf(stderr, "%s\n", line);
    }
}

#endif /* DCF_MESH_RUNTIME_H */
