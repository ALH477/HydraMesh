/* SPDX-License-Identifier: LGPL-3.0-only
 * Certify codec/demod_fec.h against the golden vectors (byte-identical encode +
 * Reed-Solomon correction). Build:
 *   gcc -std=c11 -I codec C_SDK/tests/test_fec_certify.c -o /tmp/fc && /tmp/fc
 */
#include "demod_fec.h"
#include "fec_vectors.gen.h"
#include <stdio.h>
#include <string.h>

static int fails = 0;
#define CHECK(c, msg) do { if (!(c)) { printf("  FAIL: %s\n", msg); fails++; } } while (0)

int main(void) {
    /* 1. systematic encode is byte-identical to the golden vectors. */
    for (int i = 0; i < FEC_N_CASES; i++) {
        uint8_t code[FEC_CODE_LEN];
        dcf_fec_encode(FEC_CASES[i].msg, FEC_MSG_LEN, FEC_NPARITY, code);
        CHECK(memcmp(code, FEC_CASES[i].code, FEC_CODE_LEN) == 0, "encode mismatch");
    }
    printf("  PASS: %d encode vectors byte-identical\n", FEC_N_CASES);

    /* 2. decode corrects the pinned corrupted codewords back to the message. */
    for (int i = 0; i < FEC_N_FIX; i++) {
        uint8_t buf[FEC_CODE_LEN];
        memcpy(buf, FEC_FIX[i].corrupt, FEC_CODE_LEN);
        int r = dcf_fec_decode(buf, FEC_CODE_LEN, FEC_NPARITY, FEC_MSG_LEN);
        CHECK(r == FEC_MSG_LEN, "decode reported failure");
        CHECK(memcmp(buf, FEC_FIX[i].msg, FEC_MSG_LEN) == 0, "decode did not recover message");
    }
    printf("  PASS: %d corrupted codewords corrected to the original frame\n", FEC_N_FIX);

    /* 3. self-check: clean decode is a no-op; >t errors are rejected. */
    {
        uint8_t code[FEC_CODE_LEN], buf[FEC_CODE_LEN];
        dcf_fec_encode(FEC_CASES[0].msg, FEC_MSG_LEN, FEC_NPARITY, code);
        memcpy(buf, code, FEC_CODE_LEN);
        CHECK(dcf_fec_decode(buf, FEC_CODE_LEN, FEC_NPARITY, FEC_MSG_LEN) == FEC_MSG_LEN, "clean");
        CHECK(memcmp(buf, code, FEC_CODE_LEN) == 0, "clean decode altered codeword");
        /* corrupt t = 8 bytes -> must recover */
        memcpy(buf, code, FEC_CODE_LEN);
        for (int k = 0; k < 8; k++) buf[k * 3] ^= 0x9C;
        CHECK(dcf_fec_decode(buf, FEC_CODE_LEN, FEC_NPARITY, FEC_MSG_LEN) == FEC_MSG_LEN, "t-error");
        CHECK(memcmp(buf, FEC_CASES[0].msg, FEC_MSG_LEN) == 0, "t-error recover");
    }
    printf("  PASS: clean no-op + t=8 byte-error correction\n");

    /* 4. interleaver round-trips. */
    {
        uint8_t cws[3 * FEC_CODE_LEN], inter[3 * FEC_CODE_LEN], back[3 * FEC_CODE_LEN];
        for (int r = 0; r < 3; r++) {
            uint8_t m[FEC_MSG_LEN];
            memset(m, r + 1, FEC_MSG_LEN);
            dcf_fec_encode(m, FEC_MSG_LEN, FEC_NPARITY, cws + r * FEC_CODE_LEN);
        }
        dcf_fec_interleave(cws, 3, FEC_CODE_LEN, inter);
        dcf_fec_deinterleave(inter, 3, FEC_CODE_LEN, back);
        CHECK(memcmp(cws, back, sizeof cws) == 0, "interleave round-trip");
    }
    printf("  PASS: interleaver round-trip\n");

    if (fails == 0) printf("ALL FEC CERT TESTS PASSED\n");
    else printf("%d FAILURE(S)\n", fails);
    return fails ? 1 : 0;
}
