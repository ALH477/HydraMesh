// SPDX-License-Identifier: LGPL-3.0-only
/* dcf-tools/sstv_send.c -- carry a still image across the HydraModem acoustic link.
 *
 * Reads an image file (opaque bytes, <= 8188 B — a low-res thumbnail: JPEG/PNG/raw),
 * fragments it into DeModFrame DATA frames with the certified DCF-SSTV L2 framing
 * (codec/demod_sstv.h), and modulates each 17-byte frame onto sound with HydraModem,
 * concatenating them into one WAV (like tx_campaign). HydraModem carries the frames
 * OPAQUELY — the 17-byte wire quantum and its 246-vector certificate are untouched.
 *
 * "Slow-scan": at the default conv profile the link is ~8-12 application-bytes/s, so a
 * few-KB image is minutes of air time; the receiver (sstv_recv) renders it as fragments
 * arrive. Images larger than 8188 B are chained app-side across image_ids (FLAG_MORE).
 *
 * Repo glue (DeMoD LLC, LGPL-3.0).
 *
 *   ./sstv_send photo.jpg out.wav [--none|--rep3|--conv] [--image-id N] \
 *               [--format raw|jpeg|png|webp|rgb565] [--src 0xHHHH] [--dst 0xHHHH]
 */
#include "../src/hydramodem.h"
#include "../../codec/demod_sstv.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static int parse_format(const char *s) {
    if (!strcmp(s, "raw"))    return DCF_SSTV_FMT_RAW;
    if (!strcmp(s, "jpeg"))   return DCF_SSTV_FMT_JPEG;
    if (!strcmp(s, "png"))    return DCF_SSTV_FMT_PNG;
    if (!strcmp(s, "webp"))   return DCF_SSTV_FMT_WEBP;
    if (!strcmp(s, "rgb565")) return DCF_SSTV_FMT_RGB565;
    return -1;
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "usage: %s image-file out.wav [--none|--rep3|--conv]\n"
                        "          [--image-id N] [--format raw|jpeg|png|webp|rgb565]\n"
                        "          [--src 0xHHHH] [--dst 0xHHHH]\n", argv[0]);
        return 2;
    }
    const char *img_path = argv[1];
    const char *wav_path = argv[2];

    hydra_profile p;
    hydra_profile_default(&p);                 /* fec defaults to conv */
    uint16_t src = 0x00A1, dst = DCF_BROADCAST, image_id = 0;
    uint8_t format_id = DCF_SSTV_FMT_JPEG;
    for (int i = 3; i < argc; ++i) {
        if      (!strcmp(argv[i], "--none")) p.fec_mode = HYDRA_FEC_NONE;
        else if (!strcmp(argv[i], "--rep3")) p.fec_mode = HYDRA_FEC_REP3;
        else if (!strcmp(argv[i], "--conv")) p.fec_mode = HYDRA_FEC_CONV;
        else if (!strcmp(argv[i], "--image-id") && i + 1 < argc)
            image_id = (uint16_t)strtol(argv[++i], NULL, 0);
        else if (!strcmp(argv[i], "--format") && i + 1 < argc) {
            int f = parse_format(argv[++i]);
            if (f < 0) { fprintf(stderr, "bad --format\n"); return 2; }
            format_id = (uint8_t)f;
        }
        else if (!strcmp(argv[i], "--src") && i + 1 < argc)
            src = (uint16_t)strtol(argv[++i], NULL, 0);
        else if (!strcmp(argv[i], "--dst") && i + 1 < argc)
            dst = (uint16_t)strtol(argv[++i], NULL, 0);
        else { fprintf(stderr, "bad arg: %s\n", argv[i]); return 2; }
    }
    if (image_id > DCF_SSTV_MAX_IMAGEID) { fprintf(stderr, "image-id must be 0..%u\n", DCF_SSTV_MAX_IMAGEID); return 2; }
    if (hydra_profile_init(&p) != 0) { fprintf(stderr, "bad profile\n"); return 2; }

    /* Read the image file (bounded to one image_id's capacity). */
    FILE *fp = fopen(img_path, "rb");
    if (!fp) { fprintf(stderr, "open %s failed\n", img_path); return 1; }
    static uint8_t img[DCF_SSTV_MAX_PAYLOAD + 1];
    size_t img_len = fread(img, 1, sizeof img, fp);
    fclose(fp);
    if (img_len > DCF_SSTV_MAX_PAYLOAD) {
        fprintf(stderr, "image %zu B exceeds %u B cap (chain across image_ids with FLAG_MORE, "
                        "or shrink the thumbnail)\n", img_len, DCF_SSTV_MAX_PAYLOAD);
        return 2;
    }

    /* Packetize into DeModFrame DATA frames. */
    uint8_t (*frames)[DCF_FRAME_SIZE] = malloc((size_t)DCF_SSTV_MAX_FRAMES * DCF_FRAME_SIZE);
    if (!frames) { fprintf(stderr, "oom\n"); return 1; }
    size_t n_frames = 0;
    if (!dcf_sstv_packetize(img, img_len, image_id, /*ts_us*/0, src, dst,
                            format_id, DCF_SSTV_FLAG_KEYFRAME,
                            frames, DCF_SSTV_MAX_FRAMES, &n_frames)) {
        fprintf(stderr, "packetize failed\n"); free(frames); return 1;
    }

    /* Modulate each frame; concatenate into one WAV with warm-up + inter-frame gaps. */
    size_t gap = (size_t)(0.05 * p.sample_rate);   /* 50 ms inter-frame silence */
    float *out = NULL; size_t cap = 0, len = 0;
    {   /* ~1 s warm-up tone + gap so a real analog path settles (ignored by RX). */
        size_t warm = (size_t)(1.0 * p.sample_rate);
        cap = warm + gap;
        out = malloc(cap * sizeof(float));
        if (!out) { fprintf(stderr, "oom\n"); free(frames); return 1; }
        for (size_t i = 0; i < warm; ++i)
            out[i] = (float)(p.tx_gain * sin(2.0 * 3.14159265358979323846
                                             * p.base_freq * (double)i / p.sample_rate));
        len = warm;
        memset(out + len, 0, gap * sizeof(float)); len += gap;
    }

    for (size_t k = 0; k < n_frames; ++k) {
        float *a = NULL; size_t an = 0;
        if (hydra_modem_tx(&p, frames[k], &a, &an) != HYDRA_OK) {
            fprintf(stderr, "tx frame %zu failed\n", k); free(a); free(out); free(frames); return 1;
        }
        size_t need = len + an + gap;
        if (need > cap) {
            cap = need * 2;
            float *nb = realloc(out, cap * sizeof(float));
            if (!nb) { fprintf(stderr, "oom\n"); free(a); free(out); free(frames); return 1; }
            out = nb;
        }
        memcpy(out + len, a, an * sizeof(float)); len += an;
        memset(out + len, 0, gap * sizeof(float)); len += gap;
        free(a);
    }
    free(frames);

    if (hydra_wav_write(wav_path, out, len, (int)p.sample_rate) != 0) {
        fprintf(stderr, "write %s failed\n", wav_path); free(out); return 1;
    }
    const char *fec = p.fec_mode == HYDRA_FEC_NONE ? "none"
                    : p.fec_mode == HYDRA_FEC_REP3 ? "rep3" : "conv";
    printf("sstv_send: %s (%zu B, format=%u, image_id=%u) -> %zu frames -> %s\n"
           "           %.1f s audio  (FEC=%s, %.0f baud %d-FSK, %d Hz, src=0x%04X dst=0x%04X)\n",
           img_path, img_len, format_id, image_id, n_frames, wav_path,
           (double)len / p.sample_rate, fec, p.baud, p.n_tones, (int)p.sample_rate, src, dst);
    free(out);
    return 0;
}
