/* tests/test_stream.c -- streaming receiver: chunked input, multiple frames,
 * bounded memory, and reset.
 *
 * The push API must reassemble frames regardless of how the audio is sliced
 * across calls (it will arrive in whatever block size the audio backend uses),
 * fire the callback once per frame with the correct payload and diagnostics,
 * and handle background noise plus inter-frame gaps. We drive it with a long
 * stream containing several frames, pushed in randomly-sized chunks.
 */
#include "../src/hydra_modem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static uint64_t rng = 0x5151515151ULL;
static double u(void){ rng^=rng<<13; rng^=rng>>7; rng^=rng<<17; return (double)(rng>>11)/9007199254740992.0; }
static double gs(void){ double a=u(),b=u(); if(a<1e-12)a=1e-12; return sqrt(-2*log(a))*cos(2*M_PI*b); }

#define NFRAMES 4
static int      g_got = 0;
static uint8_t  g_rx[NFRAMES + 1][HYDRA_DCF_BYTES];   /* +1 for post-reset frame */

static void cb(const uint8_t pl[17], const hydra_rx_diag *d, void *user)
{
    (void)user;
    if (g_got <= NFRAMES) memcpy(g_rx[g_got], pl, HYDRA_DCF_BYTES);
    printf("  frame %d: origin=%ld sync=%d/16 ppm=%+.0f peakE=%.2f\n",
           g_got, d->frame_origin, d->sync_score, d->clock_ppm, (double)d->peak_energy);
    ++g_got;
}

int main(void)
{
    hydra_profile p; hydra_profile_default(&p); hydra_profile_init(&p);

    /* distinct payloads so we can verify per-frame correctness */
    uint8_t pl[NFRAMES][HYDRA_DCF_BYTES];
    float  *fr[NFRAMES]; size_t fn[NFRAMES];
    int f, i; size_t total = 0;
    for (f = 0; f < NFRAMES; ++f) {
        for (i = 0; i < (int)HYDRA_DCF_BYTES; ++i) pl[f][i] = (uint8_t)(f*37 + i*11 + 1);
        if (hydra_modem_tx(&p, pl[f], &fr[f], &fn[f]) != HYDRA_OK) { printf("tx fail\n"); return 2; }
    }

    /* assemble: [noise][f0][gap][f1][gap][f2][gap][f3][noise] */
    size_t pre = 7000, gap = 5000, post = 3000;
    total = pre + post;
    for (f = 0; f < NFRAMES; ++f) total += fn[f] + (f ? gap : 0);
    float *str = calloc(total, sizeof *str);
    size_t w = 0; double nf = 0.004;
    for (i = 0; i < (int)pre; ++i) str[w++] = (float)(nf*gs());
    for (f = 0; f < NFRAMES; ++f) {
        if (f) for (i = 0; i < (int)gap; ++i) str[w++] = (float)(nf*gs());
        for (i = 0; i < (int)fn[f]; ++i) str[w++] = fr[f][i] + (float)(nf*gs());
    }
    for (i = 0; i < (int)post; ++i) str[w++] = (float)(nf*gs());

    /* push in random chunks */
    hydra_rx *rx = hydra_rx_create(&p, cb, NULL);
    size_t off = 0; int chunks = 0;
    printf("streaming %zu samples (%d frames) in random chunks:\n", total, NFRAMES);
    while (off < total) {
        size_t c = 64 + (size_t)(u()*1200); if (off + c > total) c = total - off;
        hydra_rx_push(rx, str + off, c); off += c; ++chunks;
    }

    /* reset must clear state and allow a fresh decode afterwards */
    hydra_rx_reset(rx);
    hydra_rx_push(rx, fr[0], fn[0]);     /* a lone frame after reset */

    hydra_rx_destroy(rx);

    int ok = (g_got == NFRAMES + 1);
    for (f = 0; f < NFRAMES; ++f) if (memcmp(g_rx[f], pl[f], HYDRA_DCF_BYTES) != 0) ok = 0;
    if (g_got >= NFRAMES + 1 && memcmp(g_rx[NFRAMES], pl[0], HYDRA_DCF_BYTES) != 0) ok = 0;

    printf("pushed in %d chunks; got %d frames (expected %d)\n", chunks, g_got, NFRAMES + 1);
    printf("%s\n", ok ? "STREAM PASSED (all payloads correct incl. post-reset frame)"
                      : "STREAM FAILED");

    for (f = 0; f < NFRAMES; ++f) free(fr[f]);
    free(str);
    return ok ? 0 : 1;
}
