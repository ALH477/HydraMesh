// SPDX-License-Identifier: LGPL-3.0-only
/* dcf-tools/sstv_recv.c -- recover a still image from a HydraModem acoustic capture.
 *
 * Feeds a capture WAV to HydraModem's streaming RX block-by-block (the same path a live
 * sound-card callback drives) and reassembles the DeModFrame DATA frames with the certified
 * DCF-SSTV L2 reassembler (codec/demod_sstv.h). On a complete image it writes the recovered
 * bytes to the output file, byte-exact with what sstv_send read in. Reports frames received,
 * fragment coverage, and the reconstructed image length/format.
 *
 * Repo glue (DeMoD LLC, LGPL-3.0).
 *
 *   ./sstv_recv in.wav out-image [--none|--rep3|--conv]
 */
#include "../src/hydramodem.h"
#include "../../codec/demod_sstv.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    dcf_sstv_reasm_t *r;
    const char       *out_path;
    long              frames_rx;    /* CRC-valid DATA frames fed to the reassembler */
    int               got_image;
    dcf_sstv_image_t  image;
    int               sync_min;
} acc_t;

static void on_frame(const uint8_t payload[DCF_FRAME_SIZE],
                     const hydra_rx_diag *d, void *user)
{
    acc_t *acc = (acc_t *)user;
    if (d->sync_score < acc->sync_min) acc->sync_min = d->sync_score;
    acc->frames_rx++;
    dcf_sstv_image_t img;
    if (dcf_sstv_reasm_push(acc->r, payload, &img) == DCF_SSTV_REASM_IMAGE) {
        if (!acc->got_image) {          /* keep the first fully assembled image */
            acc->got_image = 1;
            acc->image = img;
        }
    }
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "usage: %s in.wav out-image [--none|--rep3|--conv]\n", argv[0]);
        return 2;
    }
    const char *wav_path = argv[1];
    const char *out_path = argv[2];

    hydra_profile p;
    hydra_profile_default(&p);
    for (int i = 3; i < argc; ++i) {
        if      (!strcmp(argv[i], "--none")) p.fec_mode = HYDRA_FEC_NONE;
        else if (!strcmp(argv[i], "--rep3")) p.fec_mode = HYDRA_FEC_REP3;
        else if (!strcmp(argv[i], "--conv")) p.fec_mode = HYDRA_FEC_CONV;
        else { fprintf(stderr, "bad arg: %s\n", argv[i]); return 2; }
    }
    if (hydra_profile_init(&p) != 0) { fprintf(stderr, "bad profile\n"); return 2; }

    float *audio = NULL; size_t n = 0; int sr = 0;
    if (hydra_wav_read(wav_path, &audio, &n, &sr) != 0) {
        fprintf(stderr, "read %s failed\n", wav_path); return 1;
    }
    if (sr != (int)p.sample_rate)
        fprintf(stderr, "warning: WAV is %d Hz, profile is %.0f Hz\n", sr, p.sample_rate);

    dcf_sstv_reasm_t *reasm = malloc(sizeof *reasm);
    if (!reasm) { fprintf(stderr, "oom\n"); free(audio); return 1; }
    dcf_sstv_reasm_init(reasm);

    acc_t acc;
    memset(&acc, 0, sizeof acc);
    acc.r = reasm;
    acc.out_path = out_path;
    acc.sync_min = (int)HYDRA_SYNC_BITS;

    hydra_rx *rx = hydra_rx_create(&p, on_frame, &acc);
    if (!rx) { fprintf(stderr, "rx create failed\n"); free(audio); free(reasm); return 1; }
    const size_t block = 1024;
    for (size_t off = 0; off < n; off += block) {
        size_t m = (off + block <= n) ? block : (n - off);
        hydra_rx_push(rx, audio + off, m);
    }
    hydra_rx_destroy(rx);
    free(audio);

    printf("sstv_recv: %s\n  frames_rx=%ld  sync_min=%d/%d\n",
           wav_path, acc.frames_rx, acc.sync_min, (int)HYDRA_SYNC_BITS);

    int rc = 0;
    if (acc.got_image) {
        FILE *fp = fopen(out_path, "wb");
        if (!fp) { fprintf(stderr, "open %s failed\n", out_path); free(reasm); return 1; }
        fwrite(acc.image.payload, 1, acc.image.payload_len, fp);
        fclose(fp);
        printf("  RECOVERED image_id=%u  %u B  format=%u  flags=0x%02X -> %s\n",
               acc.image.image_id, acc.image.payload_len, acc.image.format_id,
               acc.image.flags, out_path);
    } else {
        /* No image completed — report which fragments are still missing per slot. */
        printf("  NO complete image (link loss); still-incomplete image(s):\n");
        for (size_t i = 0; i < DCF_SSTV_REASM_SLOTS; ++i) {
            const dcf_sstv_slot_t *s = &reasm->slots[i];
            if (!s->in_use) continue;
            printf("    image_id=%u desc=%s frag_total=%u\n",
                   s->image_id, s->have_desc ? "yes" : "no", s->frag_total);
        }
        rc = 1;
    }
    free(reasm);
    return rc;
}
