/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_mesh_certify.c — certifies the C mesh algorithms (codec/demod_mesh.h)
 * against the cross-language golden vectors (codec/mesh_vectors.gen.h). Passing ==
 * value-agreement with the Python and Rust/Go references on all five primitives.
 *
 * Build: gcc -std=c11 -I codec C_SDK/tests/test_mesh_certify.c -o test_mesh
 */
#include <stdio.h>
#include <string.h>
#include "demod_mesh.h"
#include "mesh_vectors.gen.h"

static int fails = 0;
static void fail(const char *what, int i) { fprintf(stderr, "FAIL: %s [case %d]\n", what, i); fails++; }

int main(void) {
    for (int i = 0; i < MESH_N_FSM; i++) {
        const mesh_fsm_t *c = &MESH_FSM[i];
        if (dcf_mesh_peer_status(c->events, c->n_events, c->fail_threshold, c->ok_threshold) != c->status)
            fail("fsm", i);
    }
    printf("PASS: %d liveness-FSM cases\n", MESH_N_FSM);

    for (int i = 0; i < MESH_N_GROUP; i++) {
        const mesh_group_t *c = &MESH_GROUP[i];
        if (dcf_mesh_group_of(c->rtt, c->threshold, c->status) != c->group) fail("grouping", i);
    }
    printf("PASS: %d RTT-grouping cases\n", MESH_N_GROUP);

    for (int i = 0; i < MESH_N_DIJ; i++) {
        const mesh_dij_t *c = &MESH_DIJ[i];
        int dist[32], nh[32];
        dcf_mesh_dijkstra(c->n, c->edges, c->n_edges, c->source, dist, nh);
        if (memcmp(dist, c->dist, c->n * sizeof(int)) != 0) fail("dijkstra dist", i);
        if (memcmp(nh, c->next_hop, c->n * sizeof(int)) != 0) fail("dijkstra next_hop", i);
    }
    printf("PASS: %d Dijkstra cases (dist + next_hop)\n", MESH_N_DIJ);

    for (int i = 0; i < MESH_N_ROUTE; i++) {
        const mesh_route_t *c = &MESH_ROUTE[i];
        int out[32];
        int m = dcf_mesh_select_routes(c->cand, c->n_cand, out);
        if (m != c->n_ord || memcmp(out, c->ordered, m * sizeof(int)) != 0) fail("routes", i);
    }
    printf("PASS: %d route-failover cases\n", MESH_N_ROUTE);

    for (int i = 0; i < MESH_N_ELECT; i++) {
        const mesh_elect_t *c = &MESH_ELECT[i];
        int roles[32];
        int master = dcf_mesh_elect(c->n, c->edges, c->n_edges, c->relay_min_degree, roles);
        if (master != c->master || memcmp(roles, c->roles, c->n * sizeof(int)) != 0) fail("election", i);
    }
    printf("PASS: %d master-election cases\n", MESH_N_ELECT);

    for (int i = 0; i < MESH_N_REPORT; i++) {
        const mesh_report_t *c = &MESH_REPORT_V[i];
        uint8_t out[256];
        int n = dcf_mesh_pack_report(c->node_id, c->peers, c->n_peers, out);
        if (n != c->n_bytes || memcmp(out, c->bytes, n) != 0) { fail("report pack", i); continue; }
        int nid, peers[16][3];
        int np = dcf_mesh_unpack_report(c->bytes, c->n_bytes, &nid, peers);
        if (np != c->n_peers || nid != c->node_id) fail("report unpack", i);
        else for (int p = 0; p < np; p++)
            if (memcmp(peers[p], c->peers[p], 3 * sizeof(int)) != 0) { fail("report unpack peer", i); break; }
    }
    printf("PASS: %d REPORT control cases\n", MESH_N_REPORT);

    for (int i = 0; i < MESH_N_ROLE; i++) {
        const mesh_role_msg_t *c = &MESH_ROLE_V[i];
        uint8_t out[16];
        int n = dcf_mesh_pack_role(c->node_id, c->role, c->master_id, out);
        int nid, role, mid;
        if (n != c->n_bytes || memcmp(out, c->bytes, n) != 0 ||
            !dcf_mesh_unpack_role(c->bytes, c->n_bytes, &nid, &role, &mid) ||
            nid != c->node_id || role != c->role || mid != c->master_id)
            fail("role", i);
    }
    printf("PASS: %d ROLE control cases\n", MESH_N_ROLE);

    if (fails) { fprintf(stderr, "\n%d FAILURE(S)\n", fails); return 1; }
    printf("\nALL MESH VECTORS HOLD — C self-healing algorithms + control are cemented.\n");
    return 0;
}
