// SPDX-License-Identifier: LGPL-3.0-only
/* dcf-tools/tx_campaign.c -- render N DeModFrames into one WAV for a PER run.
 *
 * Each frame is a real, valid DeModFrame (codec/demod_frame.h) carrying an
 * incrementing 16-bit counter in both `seq` and the first two payload bytes, so
 * the receiver (rx_campaign) can tell exactly which frames were lost. Frames are
 * concatenated with an extra silence gap so HydraModem's streaming RX cleanly
 * segments one burst per frame.
 *
 * Repo glue (DeMoD LLC, LGPL-3.0).
 *
 *   ./tx_campaign 200 camp_tx.wav [--none|--rep3|--conv]
 */
#include "../src/hydramodem.h"
#include "../../codec/demod_frame.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "usage: %s N out.wav [--none|--rep3|--conv]\n", argv[0]);
        return 2;
    }
    long N = strtol(argv[1], NULL, 10);
    const char *path = argv[2];
    if (N <= 0 || N > 65535) { fprintf(stderr, "N must be 1..65535\n"); return 2; }

    hydra_profile p;
    hydra_profile_default(&p);                 /* fec defaults to conv */
    if (argc >= 4) {
        if      (!strcmp(argv[3], "--none")) p.fec_mode = HYDRA_FEC_NONE;
        else if (!strcmp(argv[3], "--rep3")) p.fec_mode = HYDRA_FEC_REP3;
        else if (!strcmp(argv[3], "--conv")) p.fec_mode = HYDRA_FEC_CONV;
    }
    if (hydra_profile_init(&p) != 0) { fprintf(stderr, "bad profile\n"); return 2; }

    size_t gap = (size_t)(0.05 * p.sample_rate);   /* 50 ms inter-frame silence */
    float *out = NULL; size_t cap = 0, len = 0;

    for (long i = 0; i < N; ++i) {
        dcf_frame_t f;
        dcf_frame_init(&f, 1, DCF_TYPE_DATA, (uint16_t)i, 0x00A1, DCF_BROADCAST);
        f.payload[0] = (uint8_t)(i >> 8);
        f.payload[1] = (uint8_t)i;
        uint8_t frame[DCF_FRAME_SIZE];
        dcf_frame_encode(&f, frame);

        float *a = NULL; size_t n = 0;
        if (hydra_modem_tx(&p, frame, &a, &n) != HYDRA_OK) {
            fprintf(stderr, "tx frame %ld failed\n", i); free(out); return 1;
        }
        size_t need = len + n + gap;
        if (need > cap) {
            cap = need * 2;
            float *nb = realloc(out, cap * sizeof(float));
            if (!nb) { fprintf(stderr, "oom\n"); free(a); free(out); return 1; }
            out = nb;
        }
        memcpy(out + len, a, n * sizeof(float)); len += n;
        memset(out + len, 0, gap * sizeof(float)); len += gap;
        free(a);
    }

    if (hydra_wav_write(path, out, len, (int)p.sample_rate) != 0) {
        fprintf(stderr, "write %s failed\n", path); free(out); return 1;
    }
    const char *name = p.fec_mode == HYDRA_FEC_NONE ? "none"
                     : p.fec_mode == HYDRA_FEC_REP3 ? "rep3" : "conv";
    printf("tx_campaign: %ld frames -> %s  (%.1f s, FEC=%s, %.0f baud %d-FSK, %d Hz)\n",
           N, path, (double)len / p.sample_rate, name, p.baud, p.n_tones,
           (int)p.sample_rate);
    free(out);
    return 0;
}
