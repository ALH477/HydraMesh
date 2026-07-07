/* test_snake_l2.c — DCF-Snake raw-L2 SuperPack batch/unbatch round-trip.
 *
 * Verifies the header-only batching codec (hydramodem/dcf-tools/snake_l2.h): a batch of
 * DeModFrames survives batch → unbatch byte-exact (even and odd counts), and a whole
 * DCF-Snake QSS message survives packetize → batch → unbatch → reassemble byte-exact.
 * No socket / CAP_NET_RAW needed — the batching is pure.
 *
 * Build: gcc -std=c11 -Wall -Wextra -I codec -o test_snake_l2 \
 *            C_SDK/tests/test_snake_l2.c -lm
 */
#include <stdio.h>
#include <string.h>
#include "demod_snake.h"
#include "../../hydramodem/dcf-tools/snake_l2.h"

static int failures = 0;
static void fail(const char *what) { fprintf(stderr, "FAIL: %s\n", what); failures++; }

int main(void) {
    /* ── 1. batch/unbatch round-trip is byte-exact for even AND odd frame counts ── */
    for (size_t n = 1; n <= 9; n++) {
        uint8_t frames[9][DCF_FRAME_SIZE];
        for (size_t i = 0; i < n; i++) {
            dcf_frame_t f;
            memset(&f, 0, sizeof f);
            f.version = 1u; f.type = DCF_TYPE_CTRL;
            f.seq = (uint16_t)(0x1000u + i); f.src_id = 7; f.dst_id = 42;
            f.payload[0] = (uint8_t)i; f.payload[1] = 0xAB; f.timestamp_us = 0x010203u;
            dcf_frame_encode(&f, frames[i]);
        }
        uint8_t eth[1500];
        size_t elen = 0;
        if (!dcf_snake_l2_batch(frames, n, eth, sizeof eth, &elen)) { fail("batch"); continue; }
        if (elen != DCF_SNAKE_L2_HDR + ((n + 1) / 2) * DCF_SUPER_LEN) fail("batch len");

        uint8_t out[9][DCF_FRAME_SIZE];
        size_t got = 0;
        if (!dcf_snake_l2_unbatch(eth, elen, out, 9, &got)) { fail("unbatch"); continue; }
        if (got != n) fail("unbatch count");
        for (size_t i = 0; i < n; i++)
            if (memcmp(frames[i], out[i], DCF_FRAME_SIZE) != 0) fail("frame bytes");
    }
    printf("PASS: batch/unbatch byte-exact for 1..9 frames (even + odd)\n");

    /* ── 2. a whole DCF-Snake QSS message survives packetize → batch → unbatch → reassemble ── */
    uint8_t qss[500];
    for (size_t i = 0; i < sizeof qss; i++) qss[i] = (uint8_t)(i * 37u);
    uint8_t frames[200][DCF_FRAME_SIZE];
    size_t nf = 0;
    if (!dcf_snake_packetize(qss, sizeof qss, 5, 0x00ABCD, 0x00A1, 0xFFFF,
                             DCF_SNAKE_MODE_LIVE, DCF_SNAKE_FLAG_ANCHOR, frames, 200, &nf))
        fail("packetize");

    uint8_t eth[9000];
    size_t elen = 0;
    if (!dcf_snake_l2_batch(frames, nf, eth, sizeof eth, &elen)) fail("batch big");

    uint8_t out[200][DCF_FRAME_SIZE];
    size_t got = 0;
    if (!dcf_snake_l2_unbatch(eth, elen, out, 200, &got)) fail("unbatch big");
    if (got != nf) fail("unbatch big count");

    dcf_snake_reasm_t r;
    dcf_snake_reasm_init(&r, DCF_TYPE_CTRL);
    dcf_snake_msg_t msg;
    int emitted = 0;
    for (size_t i = 0; i < got; i++)
        if (dcf_snake_reasm_push(&r, out[i], &msg) == DCF_SNAKE_REASM_MESSAGE) emitted = 1;
    if (!emitted || msg.payload_len != sizeof qss || memcmp(msg.payload, qss, sizeof qss) != 0)
        fail("reassemble through L2 batch");
    printf("PASS: %zu-byte QSS message survives packetize→batch→unbatch→reassemble byte-exact\n",
           sizeof qss);

    /* ── 3. capacity + too-small buffer rejection ── */
    if (dcf_snake_l2_capacity(1500) != 92u) fail("capacity(1500) != 92");
    if (dcf_snake_l2_capacity(9000) != 562u) fail("capacity(9000) != 562");
    uint8_t tiny[10];
    size_t tl = 0;
    if (dcf_snake_l2_batch(frames, nf, tiny, sizeof tiny, &tl)) fail("tiny buffer not rejected");
    printf("PASS: MTU capacity (1500→92, 9000→562) + short-buffer rejection\n");

    if (failures == 0)
        printf("\nALL SNAKE-L2 CHECKS HOLD — raw-L2 SuperPack batching is byte-exact.\n");
    else
        fprintf(stderr, "\n%d FAILURE(S) — snake-L2 batching is broken.\n", failures);
    return failures != 0;
}
