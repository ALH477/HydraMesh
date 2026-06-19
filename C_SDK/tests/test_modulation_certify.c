/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_modulation_certify.c — certifies the C modulation mapping
 * (codec/demod_modulation.h) against the cross-language golden vectors
 * (codec/modulation_vectors.gen.h). Passing == byte-agreement with the Python
 * and Rust references on the byte<->symbol bijection.
 *
 * Build: gcc -std=c11 -I codec C_SDK/tests/test_modulation_certify.c -o test_mod
 */
#include <stdio.h>
#include <string.h>
#include "demod_modulation.h"
#include "modulation_vectors.gen.h"

static int fails = 0;
static void fail(const char *what, int i) { fprintf(stderr, "FAIL: %s [case %d]\n", what, i); fails++; }

int main(void) {
    /* gray bijection anchor */
    for (unsigned n = 0; n < 256; n++)
        if (dcf_ungray(dcf_gray(n)) != n) { fail("gray bijection", (int)n); break; }

    for (int i = 0; i < MOD_N_CASES; i++) {
        const mod_case_t *c = &MOD_CASES[i];
        uint8_t syms[512];
        size_t n = dcf_modulate(c->mod, c->data, c->data_len, syms, sizeof syms);
        if (n != c->n_syms) { fail("symbol count", i); continue; }
        if (memcmp(syms, c->syms, n) != 0) { fail("symbol bytes", i); continue; }
        uint8_t back[64];
        dcf_demodulate(c->mod, syms, n, back, c->data_len);
        if (memcmp(back, c->data, c->data_len) != 0) fail("round-trip", i);
    }

    if (fails) { fprintf(stderr, "\n%d FAILURE(S)\n", fails); return 1; }
    printf("PASS: %d modulation cases (modulate + round-trip, 4 schemes)\n", MOD_N_CASES);
    printf("\nALL MODULATION VECTORS HOLD — C modem mapping is cemented.\n");
    return 0;
}
