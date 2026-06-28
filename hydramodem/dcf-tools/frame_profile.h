// SPDX-License-Identifier: LGPL-3.0-only
/* dcf-tools/frame_profile.h -- shared CLI -> hydra_profile parsing for frame_tx/frame_rx.
 * FEC mode + FDMA tone-channel overrides (so each node can sit on a distinct frequency
 * band of the line). Repo glue (DeMoD LLC, LGPL-3.0). */
#ifndef DCF_FRAME_PROFILE_H
#define DCF_FRAME_PROFILE_H
#include "../src/hydramodem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Apply argv[start..] onto an already-defaulted profile. Returns 0 ok, <0 on a bad arg.
 *   --none|--rep3|--conv          FEC mode
 *   --base-freq HZ                tone 0 frequency (FDMA channel)
 *   --tone-spacing HZ             spacing between tones
 *   --baud HZ  --n-tones N        symbol rate / M-FSK order
 * base_freq and tone_spacing must stay integer multiples of baud (HydraModem enforces it). */
static int frame_profile_args(hydra_profile *p, int argc, char **argv, int start)
{
    for (int i = start; i < argc; ++i) {
        if      (!strcmp(argv[i], "--none")) p->fec_mode = HYDRA_FEC_NONE;
        else if (!strcmp(argv[i], "--rep3")) p->fec_mode = HYDRA_FEC_REP3;
        else if (!strcmp(argv[i], "--conv")) p->fec_mode = HYDRA_FEC_CONV;
        else if (!strcmp(argv[i], "--base-freq")    && i + 1 < argc) p->base_freq    = atof(argv[++i]);
        else if (!strcmp(argv[i], "--tone-spacing") && i + 1 < argc) p->tone_spacing = atof(argv[++i]);
        else if (!strcmp(argv[i], "--baud")         && i + 1 < argc) p->baud         = atof(argv[++i]);
        else if (!strcmp(argv[i], "--n-tones")      && i + 1 < argc) p->n_tones      = atoi(argv[++i]);
        else { fprintf(stderr, "bad arg: %s\n", argv[i]); return -1; }
    }
    return 0;
}
#endif
