// SPDX-License-Identifier: LGPL-3.0-only
/* dcf-tools/rx_campaign.c -- decode a captured campaign WAV and report PER.
 *
 * Feeds the capture to HydraModem's streaming RX block-by-block (the same path a
 * live sound-card callback would drive). Each CRC-valid frame's per-frame counter
 * (payload[0..1], set by tx_campaign) is recorded, so loss is exact rather than
 * inferred. Reports frames_rx, unique frames, PER, worst sync score and the
 * tracked TX/RX sample-clock offset (the two interfaces have independent crystals).
 *
 * Repo glue (DeMoD LLC, LGPL-3.0).
 *
 *   ./rx_campaign camp_rx.wav 200 [--none|--rep3|--conv]
 */
#include "../src/hydramodem.h"
#include "../../codec/demod_frame.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
    int   *seen;
    long   max_id;
    long   rx;          /* total CRC-valid frames matching our direction  */
    long   foreign;     /* CRC-valid frames with another src (crosstalk)  */
    int    have_src;    /* 1 if filtering by expected src                 */
    uint16_t src;       /* expected direction tag                         */
    int    sync_min;    /* worst sync score seen                          */
    double ppm_sum;
    double ppm_absmax;
} acc_t;

static void on_frame(const uint8_t payload[DCF_FRAME_SIZE],
                     const hydra_rx_diag *d, void *user)
{
    acc_t *acc = (acc_t *)user;
    dcf_frame_t f;
    if (!dcf_frame_decode(payload, &f)) return;   /* not a valid DCF frame */

    if (acc->have_src && f.src_id != acc->src) {  /* leaked from the other cable */
        acc->foreign++;
        return;
    }
    long id = ((long)f.payload[0] << 8) | f.payload[1];
    acc->rx++;
    if (id >= 0 && id <= acc->max_id && !acc->seen[id]) acc->seen[id] = 1;
    if (d->sync_score < acc->sync_min) acc->sync_min = d->sync_score;
    acc->ppm_sum += d->clock_ppm;
    double a = fabs(d->clock_ppm);
    if (a > acc->ppm_absmax) acc->ppm_absmax = a;
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "usage: %s in.wav N [--none|--rep3|--conv] [--src 0xHHHH]\n", argv[0]);
        return 2;
    }
    const char *path = argv[1];
    long N = strtol(argv[2], NULL, 10);
    if (N <= 0 || N > 65535) { fprintf(stderr, "N must be 1..65535\n"); return 2; }

    hydra_profile p;
    hydra_profile_default(&p);
    int have_src = 0; uint16_t exp_src = 0;
    for (int i = 3; i < argc; ++i) {
        if      (!strcmp(argv[i], "--none")) p.fec_mode = HYDRA_FEC_NONE;
        else if (!strcmp(argv[i], "--rep3")) p.fec_mode = HYDRA_FEC_REP3;
        else if (!strcmp(argv[i], "--conv")) p.fec_mode = HYDRA_FEC_CONV;
        else if (!strcmp(argv[i], "--src") && i + 1 < argc) {
            exp_src = (uint16_t)strtol(argv[++i], NULL, 0); have_src = 1;
        } else { fprintf(stderr, "bad arg: %s\n", argv[i]); return 2; }
    }
    if (hydra_profile_init(&p) != 0) { fprintf(stderr, "bad profile\n"); return 2; }

    float *audio = NULL; size_t n = 0; int sr = 0;
    if (hydra_wav_read(path, &audio, &n, &sr) != 0) {
        fprintf(stderr, "read %s failed\n", path); return 1;
    }
    if (sr != (int)p.sample_rate)
        fprintf(stderr, "warning: WAV is %d Hz, profile is %.0f Hz\n", sr, p.sample_rate);

    acc_t acc;
    memset(&acc, 0, sizeof acc);
    acc.max_id = N - 1;
    acc.have_src = have_src;
    acc.src = exp_src;
    acc.sync_min = (int)HYDRA_SYNC_BITS;
    acc.seen = calloc((size_t)N, sizeof(int));
    if (!acc.seen) { fprintf(stderr, "oom\n"); free(audio); return 1; }

    hydra_rx *rx = hydra_rx_create(&p, on_frame, &acc);
    if (!rx) { fprintf(stderr, "rx create failed\n"); free(audio); free(acc.seen); return 1; }

    const size_t block = 1024;
    for (size_t off = 0; off < n; off += block) {
        size_t m = (off + block <= n) ? block : (n - off);
        hydra_rx_push(rx, audio + off, m);
    }
    hydra_rx_destroy(rx);
    free(audio);

    long unique = 0;
    for (long i = 0; i < N; ++i) unique += acc.seen[i];
    long lost = N - unique;
    double per = 100.0 * (double)lost / (double)N;

    printf("rx_campaign: %s\n", path);
    printf("  frames_tx=%ld  frames_rx=%ld  unique=%ld  lost=%ld\n",
           N, acc.rx, unique, lost);
    printf("  PER=%.2f%%   sync_min=%d/%d   clock_ppm mean=%.0f absmax=%.0f\n",
           per, acc.sync_min, (int)HYDRA_SYNC_BITS,
           acc.rx ? acc.ppm_sum / (double)acc.rx : 0.0, acc.ppm_absmax);
    if (acc.have_src)
        printf("  expected src=0x%04X   foreign frames (crosstalk)=%ld\n",
               acc.src, acc.foreign);

    if (lost > 0) {
        printf("  lost ids:");
        long shown = 0;
        for (long i = 0; i < N && shown < 40; ++i)
            if (!acc.seen[i]) { printf(" %ld", i); ++shown; }
        if (lost > shown) printf(" ...");
        printf("\n");
    }
    free(acc.seen);

    printf(per < 1.0 ? "  PASS (PER < 1%%)\n"
                     : "  CHECK LEVELS/COUPLING (PER >= 1%%)\n");
    return per < 1.0 ? 0 : 1;
}
