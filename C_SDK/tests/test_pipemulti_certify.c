/* test_pipemulti_certify.c — C certification for DCF-Pipe Multi-Control.
 *
 * Certifies codec/demod_pipemulti.h against codec/pipemulti_vectors.gen.h.
 * Passing = byte-agreement with Rust and Python references.
 *
 * Build: gcc -std=c11 -Wall -Wextra -I codec -o /tmp/test_pipemulti_certify \
 *            C_SDK/tests/test_pipemulti_certify.c
 */
#include <stdio.h>
#include <string.h>
#include "demod_pipemulti.h"
#include "pipemulti_vectors.gen.h"

static int failures = 0;
static void fail(const char *what, int idx) {
    fprintf(stderr, "FAIL: %s [case %d]\n", what, idx);
    failures++;
}

int main(void) {
    /* ── 1. MAIN cases: pack cmds → compare bytes; round-trip ── */
    for (int i = 0; i < MC_N_MAIN; i++) {
        const mc_main_t *c = &MC_MAIN[i];
        mc_cmd_t cmds[3];
        for (int j = 0; j < c->count; j++) {
            cmds[j].local_idx = c->local_idx[j];
            cmds[j].opcode = c->opcode[j];
            cmds[j].param_lsb = c->param_lsb[j];
        }
        uint8_t out[4];
        int rc = mc_pack(out, cmds, c->count, c->flags);
        if (rc != MC_OK) { fail("main: pack returned error", i); continue; }
        if (memcmp(out, c->bytes, 4) != 0) { fail("main: bytes mismatch", i); continue; }
        /* round-trip */
        uint8_t count, flags;
        mc_cmd_t rc_cmds[3];
        rc = mc_unpack(out, 4, &count, &flags, rc_cmds);
        if (rc != MC_OK) { fail("main: unpack returned error", i); continue; }
        if (count != c->count) { fail("main: count mismatch", i); continue; }
        for (int j = 0; j < count; j++) {
            if (rc_cmds[j].local_idx != cmds[j].local_idx ||
                rc_cmds[j].opcode != cmds[j].opcode ||
                rc_cmds[j].param_lsb != cmds[j].param_lsb) {
                fail("main: round-trip cmd mismatch", i); break;
            }
        }
    }
    printf("PASS: %d main cases byte-identical + round-tripped\n", MC_N_MAIN);

    /* ── 2. REJECT cases: unpack must return error ── */
    for (int i = 0; i < MC_N_REJECT; i++) {
        const mc_reject_t *r = &MC_REJECT[i];
        uint8_t count, flags;
        mc_cmd_t cmds[3];
        int rc = mc_unpack(r->bytes, 4, &count, &flags, cmds);
        if (rc == MC_OK) {
            fail("reject: unpack should have failed", i);
        }
    }
    printf("PASS: %d reject cases all raise on unpack\n", MC_N_REJECT);

    /* ── 3. DISCRIMINATOR cases: is_multicontrol / is_classic_pipe ── */
    for (int i = 0; i < MC_N_DISCRIM; i++) {
        const mc_discrim_t *d = &MC_DISCRIM[i];
        uint8_t byte0[1] = {d->byte0};
        bool is_mc = mc_is_multicontrol(byte0, 1);
        bool is_cp = mc_is_classic_pipe(byte0, 1);
        if (is_mc != (bool)d->is_mc) fail("discrim: is_multicontrol mismatch", i);
        if (is_cp != (bool)d->is_classic) fail("discrim: is_classic_pipe mismatch", i);
    }
    printf("PASS: %d discriminator samples partition cleanly\n", MC_N_DISCRIM);

    if (failures == 0)
        printf("\nALL PIPEMULTI VECTORS HOLD — C Multi-Control is cemented.\n");
    else
        fprintf(stderr, "\n%d FAILURE(S) — C Multi-Control is NOT cemented.\n", failures);
    return failures != 0;
}