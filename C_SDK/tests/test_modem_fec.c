/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_modem_fec.c — the modem transport with forward error correction: a frame is
 * RS-coded, modulated, rendered to samples, a few symbols are CORRUPTED (a noisy
 * medium), then recovered, demodulated, and RS-decoded back to the exact frame. This
 * is what makes the modem usable over real RF/acoustic media — errors are corrected,
 * not just detected. The RS code itself is byte-certified (test_fec_certify.c); the
 * waveform is loopback-tested (test_modem_loopback.c); this ties them together.
 *
 * Build: gcc -std=c11 -I codec -I C_SDK/node C_SDK/tests/test_modem_fec.c -lm -o test_modem_fec
 */
#include <stdio.h>
#include <string.h>
#include "demod_modulation.h"
#include "demod_fec.h"
#include "dcf_modem.h"

static int fails = 0;
static const char *NAMES[4] = {"fsk", "ook", "psk", "qam"};

static void check(uint8_t mod) {
    uint8_t frame[17];
    for (int i = 0; i < 17; i++) frame[i] = (uint8_t)(i * 17 + 3);

    uint8_t code[33];                                   /* 17 + 16 parity */
    dcf_fec_encode(frame, 17, DCF_FEC_NPARITY, code);

    uint8_t syms[4096];
    size_t nsyms = dcf_modulate(mod, code, 33, syms, sizeof syms);
    static double samples[4096 * DCF_MODEM_SPS];
    size_t ns = dcf_modem_render(mod, syms, nsyms, samples, sizeof(samples) / sizeof(samples[0]));

    /* corrupt 3 symbols -> flip each to a neighbouring symbol (a noisy channel). */
    unsigned bits = DCF_MOD_BITS_PER_SYMBOL[mod];
    unsigned maxsym = 1u << bits;
    size_t pick[3] = {2, nsyms / 2, nsyms - 2};
    for (int k = 0; k < 3; k++) {
        if (pick[k] < nsyms) {
            uint8_t wrong = (uint8_t)((syms[pick[k]] + 1) % maxsym);
            double snip[DCF_MODEM_SPS];
            dcf_modem_snippet(mod, wrong, snip);
            memcpy(&samples[pick[k] * DCF_MODEM_SPS], snip, DCF_MODEM_SPS * sizeof(double));
        }
    }

    uint8_t rsyms[4096];
    size_t rn = dcf_modem_recover(mod, samples, ns, rsyms, sizeof rsyms);
    uint8_t recv[33];
    dcf_demodulate(mod, rsyms, rn, recv, 33);

    int diff = memcmp(recv, code, 33) != 0;             /* there ARE channel errors */
    int r = dcf_fec_decode(recv, 33, DCF_FEC_NPARITY, 17);
    if (r != 17 || memcmp(recv, frame, 17) != 0) {
        fprintf(stderr, "FAIL %s: RS did not recover the frame (r=%d)\n", NAMES[mod], r);
        fails++;
    } else {
        printf("  PASS %s: 3 corrupted symbols%s -> RS recovered the frame\n",
               NAMES[mod], diff ? "" : " (no byte error)");
    }
}

int main(void) {
    for (uint8_t m = 0; m < 4; m++) check(m);
    if (!fails) printf("ALL MODEM-FEC TESTS PASSED\n");
    return fails ? 1 : 0;
}
