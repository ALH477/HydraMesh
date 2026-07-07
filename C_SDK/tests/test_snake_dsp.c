/* test_snake_dsp.c — unit tests for the DCF-Snake mixer/spoke DSP primitives
 * (hydramodem/dcf-tools/snake_dsp.h): PI clock servo, jitter buffer, cubic-Hermite ASRC,
 * PCM PLC, and the cue-mix matrix.  These are the genuinely-new float DSP the sync core
 * needs; they are approximate (not byte-certified), so this asserts behaviour, not bytes.
 *
 * Build: gcc -std=c11 -Wall -Wextra -I codec -o test_snake_dsp \
 *            C_SDK/tests/test_snake_dsp.c -lm
 */
#include <math.h>
#include <stdio.h>
#include "../../hydramodem/dcf-tools/snake_dsp.h"

static int failures = 0;
static void check(int cond, const char *what) {
    if (!cond) { fprintf(stderr, "FAIL: %s\n", what); failures++; }
}

static void test_servo(void) {
    snake_servo_t s;
    /* a small persistent lag drives the trim positive and the integrator accumulates it */
    snake_servo_init(&s, 0.01, 0.001, 100.0);
    double ppm = 0, prev = 0;
    int monotonic = 1;
    for (int i = 0; i < 20; i++) {
        prev = ppm;
        ppm = snake_servo_update(&s, 5.0);
        if (i > 0 && ppm < prev) monotonic = 0;
    }
    check(ppm > 0.0, "servo: positive lag → positive ppm");
    check(monotonic, "servo: integrator ramps the trim under sustained lag");

    /* a large error saturates the trim at the clamp, symmetrically */
    snake_servo_init(&s, 0.01, 0.001, 100.0);
    check(snake_servo_update(&s, 1e9) == 100.0, "servo: ppm clamps at +limit");
    snake_servo_init(&s, 0.01, 0.001, 100.0);
    check(snake_servo_update(&s, -1e9) == -100.0, "servo: symmetric negative clamp");
    printf("PASS: PI clock servo (sign + ramp + clamp)\n");
}

static void fill_block(float *b, uint32_t len, float v) {
    for (uint32_t i = 0; i < len; i++) b[i] = v;
}

static void test_jitter(void) {
    snake_jb_t jb;
    float out[4];

    /* in-order + priming */
    snake_jb_init(&jb, 4, 2);
    float blk[4];
    for (uint64_t a = 0; a < 3; a++) { fill_block(blk, 4, (float)a); snake_jb_push(&jb, a, blk, 4); }
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 0.0f, "jb: pop idx0");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 1.0f, "jb: pop idx1");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 2.0f, "jb: pop idx2");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_UNDERRUN, "jb: underrun at the end");

    /* reordered arrival still plays in order */
    snake_jb_init(&jb, 4, 2);
    fill_block(blk, 4, 0); snake_jb_push(&jb, 0, blk, 4);
    fill_block(blk, 4, 2); snake_jb_push(&jb, 2, blk, 4);
    fill_block(blk, 4, 1); snake_jb_push(&jb, 1, blk, 4);
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 0.0f, "jb: reorder pop0");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 1.0f, "jb: reorder pop1");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 2.0f, "jb: reorder pop2");

    /* a real gap is reported (→ PLC), then playout resumes */
    snake_jb_init(&jb, 4, 2);
    fill_block(blk, 4, 0); snake_jb_push(&jb, 0, blk, 4);
    fill_block(blk, 4, 1); snake_jb_push(&jb, 1, blk, 4);
    fill_block(blk, 4, 3); snake_jb_push(&jb, 3, blk, 4);   /* idx 2 dropped */
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 0.0f, "jb: gap pop0");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 1.0f, "jb: gap pop1");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_GAP, "jb: gap at idx2");
    check(snake_jb_pop(&jb, out) == SNAKE_JB_BLOCK && out[0] == 3.0f, "jb: resume at idx3");

    /* late + duplicate are ignored */
    snake_jb_init(&jb, 4, 1);
    fill_block(blk, 4, 5); check(snake_jb_push(&jb, 5, blk, 4), "jb: push5");
    check(!snake_jb_push(&jb, 5, blk, 4), "jb: duplicate ignored");
    snake_jb_pop(&jb, out);                                  /* plays idx5, play→6 */
    check(!snake_jb_push(&jb, 5, blk, 4), "jb: late (already played) ignored");
    printf("PASS: jitter buffer (order/reorder/gap/late/duplicate)\n");
}

static void test_asrc(void) {
    float in[64], out[256];
    for (int i = 0; i < 64; i++) in[i] = sinf(0.1f * (float)i);

    /* ratio 1.0 is an exact identity (integer read positions, t == 0) */
    snake_asrc_t a; snake_asrc_init(&a, 1.0);
    size_t n = snake_asrc_process(&a, in, 64, out, 256);
    check(n == 64, "asrc: ratio 1.0 preserves sample count");
    int identical = 1;
    for (int i = 0; i < 64; i++) if (fabsf(out[i] - in[i]) > 1e-6f) identical = 0;
    check(identical, "asrc: ratio 1.0 is bit-near-identity");

    /* DC is preserved exactly at any ratio */
    float dc[32]; for (int i = 0; i < 32; i++) dc[i] = 0.7f;
    snake_asrc_init(&a, 1.001);
    n = snake_asrc_process(&a, dc, 32, out, 256);
    int dc_ok = n > 0;
    for (size_t i = 0; i < n; i++) if (fabsf(out[i] - 0.7f) > 1e-5f) dc_ok = 0;
    check(dc_ok, "asrc: DC preserved under drift ratio");

    /* upsample and downsample produce sane counts and finite output */
    snake_asrc_init(&a, 2.0);
    n = snake_asrc_process(&a, in, 64, out, 256);
    check(n >= 120 && n <= 130, "asrc: 2x upsample ~doubles the count");
    int finite = 1; for (size_t i = 0; i < n; i++) if (!isfinite(out[i])) finite = 0;
    check(finite, "asrc: output is finite");

    snake_asrc_init(&a, 0.5);
    n = snake_asrc_process(&a, in, 64, out, 256);
    check(n >= 30 && n <= 34, "asrc: 0.5x downsample ~halves the count");
    printf("PASS: cubic-Hermite ASRC (identity/DC/up/down)\n");
}

static void test_plc(void) {
    snake_plc_t p; snake_plc_init(&p, 4, 0.5f);
    float out[4];

    /* no history → silence */
    snake_plc_conceal(&p, out);
    check(out[0] == 0.0f, "plc: silence before any good block");

    float good[4] = {1.0f, -1.0f, 0.5f, -0.5f};
    snake_plc_good(&p, good, 4);
    snake_plc_conceal(&p, out);
    check(fabsf(out[0] - 0.5f) < 1e-6f, "plc: first conceal = last * 0.5");
    snake_plc_conceal(&p, out);
    check(fabsf(out[0] - 0.25f) < 1e-6f, "plc: second conceal decays again (0.25)");
    snake_plc_good(&p, good, 4);   /* recovery resets the gain */
    snake_plc_conceal(&p, out);
    check(fabsf(out[0] - 0.5f) < 1e-6f, "plc: gain resets after a good block");
    printf("PASS: PCM PLC (silence/decay/reset)\n");
}

static void test_cuemix(void) {
    snake_cuemix_t c; snake_cuemix_init(&c, 2, 2);
    /* node 0 hears src0 at 1.0 + src1 at 0.5; node 1 hears only src1 */
    snake_cuemix_set(&c, 0, 0, 1.0f);
    snake_cuemix_set(&c, 0, 1, 0.5f);
    snake_cuemix_set(&c, 1, 1, 1.0f);
    float s0[3] = {0.2f, 0.2f, 0.2f}, s1[3] = {0.4f, 0.4f, 0.4f};
    const float *ins[2] = {s0, s1};
    float bus[3];
    snake_cuemix_bus(&c, 0, ins, 3, bus);
    check(fabsf(bus[0] - (0.2f + 0.5f * 0.4f)) < 1e-6f, "cuemix: node0 weighted sum");
    snake_cuemix_bus(&c, 1, ins, 3, bus);
    check(fabsf(bus[0] - 0.4f) < 1e-6f, "cuemix: node1 = src1 only");

    /* limiter clamps a hot sum to +/-1 */
    float hot0[1] = {0.9f}, hot1[1] = {0.9f};
    const float *hot[2] = {hot0, hot1};
    snake_cuemix_set(&c, 0, 0, 1.0f); snake_cuemix_set(&c, 0, 1, 1.0f);
    snake_cuemix_bus(&c, 0, hot, 1, bus);
    check(bus[0] == 1.0f, "cuemix: limiter clamps to +1");
    printf("PASS: cue-mix matrix (weighted sum + limiter)\n");
}

int main(void) {
    test_servo();
    test_jitter();
    test_asrc();
    test_plc();
    test_cuemix();
    if (failures == 0) printf("\nALL SNAKE-DSP CHECKS HOLD.\n");
    else fprintf(stderr, "\n%d FAILURE(S).\n", failures);
    return failures != 0;
}
