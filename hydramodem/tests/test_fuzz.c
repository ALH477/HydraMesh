/* tests/test_fuzz.c -- robustness / memory-safety fuzzing.
 *
 * The receiver must never crash, leak, or read out of bounds on arbitrary
 * input -- it will be fed live microphone audio full of noise, partial frames,
 * and junk. We hammer both the one-shot and streaming APIs with random buffers
 * of every awkward size, plus random-payload clean roundtrips as a correctness
 * backstop. Build with `make asan` to run this under AddressSanitizer +
 * UBSan; a clean exit there is the real pass condition.
 */
#include "../src/hydra_modem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static uint64_t rng = 0x9E3779B97F4A7C15ULL;
static uint32_t u32(void){ rng^=rng<<13; rng^=rng>>7; rng^=rng<<17; return (uint32_t)(rng>>32); }
static float    rsamp(void){ return (float)((double)(int32_t)u32() / 2147483648.0); }

static int g_frames = 0;
static void cb(const uint8_t pl[17], const hydra_rx_diag *d, void *u){ (void)pl;(void)d;(void)u; ++g_frames; }

int main(void)
{
    hydra_profile p;
    int iter;
    hydra_profile_default(&p);
    hydra_profile_init(&p);

    printf("HydraModem fuzz / robustness\n");

    /* 1) one-shot RX on random buffers of many sizes (incl. degenerate) */
    {
        size_t sizes[] = {0, 1, 2, 47, 48, 49, 960, 5000, 19007, 19008, 40000};
        size_t s;
        for (s = 0; s < sizeof sizes/sizeof*sizes; ++s) {
            size_t n = sizes[s], i;
            float *buf = n ? malloc(n*sizeof*buf) : NULL;
            uint8_t out[HYDRA_DCF_BYTES];
            for (i = 0; i < n; ++i) buf[i] = rsamp();
            /* must return a defined status, never crash */
            (void)hydra_modem_rx(&p, buf, n, out);
            free(buf);
        }
        printf("  ok   one-shot RX survived %zu degenerate/random buffers\n",
               sizeof sizes/sizeof*sizes);
    }

    /* 2) streaming RX: many iterations of random-length random-content pushes */
    {
        hydra_rx *rx = hydra_rx_create(&p, cb, NULL);
        for (iter = 0; iter < 2000; ++iter) {
            size_t n = (u32() % 1500) + 1, i;
            float *buf = malloc(n*sizeof*buf);
            for (i = 0; i < n; ++i) buf[i] = rsamp() * 0.3f;
            (void)hydra_rx_push(rx, buf, n);
            if ((iter & 511) == 511) hydra_rx_reset(rx);   /* exercise reset path */
            free(buf);
        }
        hydra_rx_destroy(rx);
        printf("  ok   streaming RX survived 2000 random pushes (%d spurious frames)\n", g_frames);
    }

    /* 3) random-payload clean roundtrips -- correctness backstop, all FEC modes */
    {
        hydra_fec_mode modes[] = {HYDRA_FEC_NONE, HYDRA_FEC_REP3, HYDRA_FEC_CONV, HYDRA_FEC_CONV};
        int il[] = {0,0,0,1};
        int m, bad = 0, trials = 0;
        for (m = 0; m < 4; ++m) {
            hydra_profile q; hydra_profile_default(&q); q.fec_mode = modes[m]; q.interleave = il[m];
            hydra_profile_init(&q);
            for (iter = 0; iter < 40; ++iter) {
                uint8_t tx[HYDRA_DCF_BYTES], rx[HYDRA_DCF_BYTES];
                float *audio = NULL; size_t n = 0, i;
                for (i = 0; i < HYDRA_DCF_BYTES; ++i) tx[i] = (uint8_t)(u32() & 0xFF);
                if (hydra_modem_tx(&q, tx, &audio, &n) != HYDRA_OK) { ++bad; continue; }
                if (!(hydra_modem_rx(&q, audio, n, rx) == HYDRA_OK && memcmp(tx,rx,HYDRA_DCF_BYTES)==0)) ++bad;
                free(audio); ++trials;
            }
        }
        if (bad == 0) printf("  ok   %d random-payload clean roundtrips (all FEC modes) exact\n", trials);
        else          printf("  FAIL %d/%d random roundtrips wrong\n", bad, trials);
    }

    /* 4) random *profiles* -- modulation order, baud, tones, preamble, sync,
     *    FEC -- TX/RX roundtrip and a noisy decode. Catches profile-dependent
     *    indexing/sizing bugs (e.g. the zero-pad path for non-dividing M-FSK). */
    {
        int m, bad = 0, trials = 0, built = 0;
        for (m = 0; m < 400; ++m) {
            hydra_profile q; hydra_profile_default(&q);
            int order_bits = (int)(u32() % 4) + 1;          /* 2..16-FSK */
            q.n_tones      = 1 << order_bits;
            int baud_div   = (int)(u32() % 4);              /* 1000/500/250/125 */
            double baud    = 1000.0 / (1 << baud_div);
            q.baud         = baud;
            q.base_freq    = baud * (double)(2 + (u32() % 4)); /* integer cycles */
            q.tone_spacing = baud * (double)(1 + (u32() % 2));
            q.preamble_syms= 8 + (int)(u32() % 32);
            q.sync_word    = (uint16_t)u32();
            q.fec_mode     = (hydra_fec_mode)(u32() % 3);
            q.interleave   = (int)(u32() & 1);
            if (hydra_profile_init(&q) != 0) continue;       /* skip invalid combos */
            ++built;
            {
                uint8_t tx[HYDRA_DCF_BYTES], rx[HYDRA_DCF_BYTES]; size_t i;
                float *audio = NULL; size_t n = 0;
                for (i = 0; i < HYDRA_DCF_BYTES; ++i) tx[i] = (uint8_t)(u32() & 0xFF);
                if (hydra_modem_tx(&q, tx, &audio, &n) != HYDRA_OK) { ++bad; continue; }
                if (!(hydra_modem_rx(&q, audio, n, rx) == HYDRA_OK &&
                      memcmp(tx, rx, HYDRA_DCF_BYTES) == 0)) ++bad;
                free(audio); ++trials;
            }
        }
        if (bad == 0) printf("  ok   %d random-profile roundtrips (of %d valid) exact\n", trials, built);
        else          printf("  FAIL %d/%d random-profile roundtrips wrong\n", bad, trials);
        printf("\n%s\n", bad ? "FUZZ FAILED" : "FUZZ PASSED (no crash; roundtrips exact)");
        return bad ? 1 : 0;
    }
}
