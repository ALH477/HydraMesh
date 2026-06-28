// SPDX-License-Identifier: LGPL-3.0-only
/* dcf-tools/sense_node.c -- portable C reference for a DCF-Sense sensor node (MCU target).
 *
 * Read a sensor -> pack a DCF-Sense reading into a 17-byte DeModFrame (the repo wire
 * codec) -> HydraModem TX -> emit the modulated audio. The THREE PLATFORM HOOKS
 * (sensor_read / sample_sink / node_sleep) are the only things an MCU port must supply;
 * everything else is portable C. On the host this writes one WAV per reading, so it is
 * buildable/testable without hardware -- and because the frame is built with the same
 * codec the Python gateway uses, a host WAV from this node decodes in dcf.sense.gateway.
 *
 * Repo glue (DeMoD LLC, LGPL-3.0).
 *   sense_node <node_id_hex> <sensor_type> <count> [out_prefix]
 */
#include "../src/hydramodem.h"
#include "../../codec/demod_frame.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- PLATFORM HOOKS (replace on an MCU) ------------------------------------ */
/* Read one sensor in physical units. MCU: your ADC / I2C / 1-Wire driver. */
static double sensor_read(uint8_t sensor_type, unsigned tick) {
    (void)sensor_type;
    return 22.0 + (tick % 5) * 0.5;                 /* host stub: synthetic */
}
/* Sink the modulated audio for one reading. MCU: stream `audio` to the DAC/line-out. */
static int sample_sink(const float *audio, size_t n, int fs, void *ctx) {
    return hydra_wav_write((const char *)ctx, audio, n, fs);   /* host stub: WAV file */
}
/* Wait until the node's next report (cadence / MAC slot). MCU: low-power sleep. */
static void node_sleep(double seconds) { (void)seconds; }      /* host stub: no-op */

/* DCF-Sense value scaling — MUST match python/dcf/sense/schema.py SENSORS table. */
static int sense_scale(uint8_t t) {
    switch (t) {
        case 0: case 1: case 2: case 5: case 6: case 8: case 9: return 100;
        case 7: return 10;
        default: return 1;                          /* co2(3), light_par(4) */
    }
}

int main(int argc, char **argv) {
    if (argc < 4) {
        fprintf(stderr, "usage: %s <node_id_hex> <sensor_type> <count> [out_prefix]\n",
                argv[0]);
        return 2;
    }
    uint16_t node_id = (uint16_t)strtol(argv[1], NULL, 16);
    uint8_t  stype   = (uint8_t)atoi(argv[2]);
    int      count   = atoi(argv[3]);
    const char *prefix = (argc > 4) ? argv[4] : "/tmp/sense_node";

    hydra_profile p;
    hydra_profile_default(&p);
    if (hydra_profile_init(&p) != 0) { fprintf(stderr, "bad profile\n"); return 2; }

    for (int i = 0; i < count; ++i) {
        double v = sensor_read(stype, (unsigned)i);
        long raw = lround(v * sense_scale(stype));
        if (raw >  32767) raw =  32767;
        if (raw < -32768) raw = -32768;

        /* DCF-Sense bare reading: src=node_id, payload [type | value i16 BE | flags]. */
        dcf_frame_t f;
        dcf_frame_init(&f, 1, DCF_TYPE_DATA, (uint16_t)i, node_id, DCF_BROADCAST);
        f.payload[0] = stype;
        f.payload[1] = (uint8_t)((raw >> 8) & 0xFF);
        f.payload[2] = (uint8_t)(raw & 0xFF);
        f.payload[3] = 0;
        uint8_t frame[DCF_FRAME_SIZE];
        dcf_frame_encode(&f, frame);

        float *audio = NULL; size_t n = 0;
        if (hydra_modem_tx(&p, frame, &audio, &n) != HYDRA_OK) {
            fprintf(stderr, "tx failed\n"); return 1;
        }
        char path[600];
        snprintf(path, sizeof path, "%s-%04x-%03d.wav", prefix, node_id, i);
        int rc = sample_sink(audio, n, (int)p.sample_rate, path);
        free(audio);
        if (rc != 0) { fprintf(stderr, "sink failed\n"); return 1; }
        printf("node 0x%04x reading %d: type=%u value=%.2f -> %s\n",
               node_id, i, stype, v, path);
        node_sleep(1.0);
    }
    return 0;
}
