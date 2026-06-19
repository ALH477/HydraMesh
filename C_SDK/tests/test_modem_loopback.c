/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_modem_loopback.c — end-to-end loopback test of the Faust-DSP modem
 * waveform layer (dcf_modem.h): bytes -> modulate -> render to samples -> recover
 * -> demodulate -> bytes must equal the input, for every modulation, over an ideal
 * (noiseless) medium. This is the analog half's recovery test (the byte<->symbol
 * mapping itself is separately byte-certified in test_modulation_certify.c).
 *
 * Build: gcc -std=c11 -I codec -I C_SDK/node C_SDK/tests/test_modem_loopback.c -lm -o test_modem
 */
#include <stdio.h>
#include <string.h>
#include "demod_modulation.h"
#include "dcf_modem.h"

static int fails = 0;
static const char *NAMES[4] = {"fsk", "ook", "psk", "qam"};

static void check(uint8_t mod, const uint8_t *data, size_t len) {
    uint8_t syms[4096];
    size_t nsyms = dcf_modulate(mod, data, len, syms, sizeof syms);

    static double samples[4096 * DCF_MODEM_SPS];
    size_t ns = dcf_modem_render(mod, syms, nsyms, samples, sizeof(samples) / sizeof(samples[0]));
    if (ns != nsyms * (size_t)DCF_MODEM_SPS) { fprintf(stderr, "FAIL %s render\n", NAMES[mod]); fails++; return; }

    uint8_t rsyms[4096];
    size_t rn = dcf_modem_recover(mod, samples, ns, rsyms, sizeof rsyms);
    if (rn != nsyms || memcmp(rsyms, syms, nsyms) != 0) { fprintf(stderr, "FAIL %s symbol recovery\n", NAMES[mod]); fails++; return; }

    uint8_t back[1024];
    dcf_demodulate(mod, rsyms, rn, back, len);
    if (memcmp(back, data, len) != 0) { fprintf(stderr, "FAIL %s byte recovery\n", NAMES[mod]); fails++; return; }
    printf("  PASS  %s: %zu bytes -> %zu symbols -> %zu samples -> bytes (exact)\n",
           NAMES[mod], len, nsyms, ns);
}

int main(void) {
    /* the 17-byte golden frame + a 32-byte SuperPack-sized buffer + a string */
    uint8_t frame[17] = {0xD3, 0x13, 0x12, 0x34, 0x00, 0x01, 0xFF, 0xFF,
                         0xDE, 0xAD, 0xBE, 0xEF, 0xAB, 0x12, 0xCD, 0x24, 0xC0};
    const char *msg = "modem over different mediums";
    for (uint8_t mod = 0; mod < 4; mod++) {
        check(mod, frame, sizeof frame);
        check(mod, (const uint8_t *)msg, strlen(msg));
    }
    if (fails) { fprintf(stderr, "\n%d FAILURE(S)\n", fails); return 1; }
    printf("\nALL MODEM LOOPBACKS HOLD — FSK/OOK/PSK/QAM recover frames byte-exact.\n");
    return 0;
}
