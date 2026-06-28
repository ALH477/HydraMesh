// SPDX-License-Identifier: LGPL-3.0-only
/* dcf-tools/frame_rx.c -- decode ONE DeModFrame from a WAV via HydraModem and print it
 * as 34 hex chars on stdout (nothing + nonzero exit if no frame recovered). The
 * single-frame counterpart to rx_campaign, used by the DCF `hydra:` transport.
 * Repo glue (DeMoD LLC, LGPL-3.0).
 *
 *   frame_rx in.wav [--none|--rep3|--conv]   (must match the TX FEC; default conv)
 */
#include "../src/hydramodem.h"
#include "frame_profile.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "usage: %s in.wav [--none|--rep3|--conv]\n", argv[0]);
        return 2;
    }
    hydra_profile p;
    hydra_profile_default(&p);
    if (frame_profile_args(&p, argc, argv, 2) != 0) return 2;
    if (hydra_profile_init(&p) != 0) { fprintf(stderr, "bad profile\n"); return 2; }

    float *audio = NULL; size_t n = 0; int sr = 0;
    if (hydra_wav_read(argv[1], &audio, &n, &sr) != 0) {
        fprintf(stderr, "read %s failed\n", argv[1]); return 1;
    }
    uint8_t out[HYDRA_DCF_BYTES];
    int rc = hydra_modem_rx(&p, audio, n, out);
    free(audio);
    if (rc != HYDRA_OK) return 1;            /* no frame -> no stdout, nonzero exit */

    for (int i = 0; i < (int)HYDRA_DCF_BYTES; ++i) printf("%02x", out[i]);
    printf("\n");
    return 0;
}
