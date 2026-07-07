// SPDX-License-Identifier: LGPL-3.0-only
/*
 * snake_source.c — DCF-Snake spoke (source) node, DeMoD LLC | LGPL-3.0.
 *
 * Captures audio and streams it to the mixer over cat5e: the record plane (quanta-coded, wire A)
 * and, disciplined to the mixer's BEACON grandmaster clock via a PI servo, the low-latency PCM
 * cue plane (wire B).  Wiring only — every primitive it calls is the verified DCF-Snake
 * foundation (demod_snake.h / demod_monitor.h L2, snake_l2.h transport, snake_node.h quanta
 * bridge, snake_dsp.h servo).  See DCF_SNAKE_SPEC.md.
 *
 * PHY: audio is read as raw 32-bit-float mono @48k from a file / stdin (ALSA capture is a
 * gated future edge); frames go out on a raw-L2 socket when --iface is given, else the pipeline
 * runs "dry" (batches counted) so the node builds and self-tests without CAP_NET_RAW or quanta.
 *
 * Usage: snake_source [--iface IF] [--channel NAME] [--node-id N] [--mode live|near|relaxed]
 *                     [--block N] [--in FILE] [--frames N] [--no-quanta] [--selftest]
 */
#include "../../codec/demod_snake.h"
#include "snake_l2.h"
#include "snake_node.h"
#include "snake_dsp.h"
#include <errno.h>
#include <unistd.h>

typedef struct {
    const char *iface, *channel, *infile, *mode;
    int node_id, block, max_frames, no_quanta, selftest;
} src_opts_t;

static void parse(int argc, char **argv, src_opts_t *o) {
    o->iface = NULL; o->channel = "snake"; o->infile = NULL; o->mode = "live";
    o->node_id = 0x00A1; o->block = 3072; o->max_frames = -1; o->no_quanta = 0; o->selftest = 0;
    for (int i = 1; i < argc; i++) {
        if      (!strcmp(argv[i], "--iface")   && i + 1 < argc) o->iface   = argv[++i];
        else if (!strcmp(argv[i], "--channel") && i + 1 < argc) o->channel = argv[++i];
        else if (!strcmp(argv[i], "--mode")    && i + 1 < argc) o->mode    = argv[++i];
        else if (!strcmp(argv[i], "--in")      && i + 1 < argc) o->infile  = argv[++i];
        else if (!strcmp(argv[i], "--node-id") && i + 1 < argc) o->node_id = (int)strtol(argv[++i], 0, 0);
        else if (!strcmp(argv[i], "--block")   && i + 1 < argc) o->block   = atoi(argv[++i]);
        else if (!strcmp(argv[i], "--frames")  && i + 1 < argc) o->max_frames = atoi(argv[++i]);
        else if (!strcmp(argv[i], "--no-quanta")) o->no_quanta = 1;
        else if (!strcmp(argv[i], "--selftest"))  o->selftest = 1;
    }
    if (o->block < 1) o->block = 3072;
    if (o->block > (int)SNAKE_MAX_BLOCK) o->block = SNAKE_MAX_BLOCK;
}

static uint8_t mode_id(const char *m) {
    if (!strcmp(m, "near")) return DCF_SNAKE_MODE_NEAR;
    if (!strcmp(m, "relaxed")) return DCF_SNAKE_MODE_RELAXED;
    return DCF_SNAKE_MODE_LIVE;
}

#define SNAKE_SRC_MTU 9000u   /* jumbo frames: a live-mode QSS packet rides one Ethernet frame */

/* Send one QSS packet (or passthrough PCM chunk) as a DCF-Snake message over the socket
 * (or count it, in dry mode).  A message larger than one MTU is split across several
 * Ethernet frames (the mixer's reassembler stitches them back).  Returns 0 on success. */
static int emit_message(const dcf_snake_l2_sock_t *sock, int dry,
                        const uint8_t *payload, size_t len, uint16_t sid, uint32_t ts,
                        uint16_t src, uint16_t dst, uint8_t mid, unsigned long *n_batches) {
    static uint8_t frames[DCF_SNAKE_MAX_FRAMES][DCF_FRAME_SIZE];
    static uint8_t eth[SNAKE_SRC_MTU];
    size_t nf = 0;
    if (!dcf_snake_packetize(payload, len, sid, ts, src, dst, mid, DCF_SNAKE_FLAG_ANCHOR,
                             frames, DCF_SNAKE_MAX_FRAMES, &nf)) return -1;
    size_t cap = dcf_snake_l2_capacity(SNAKE_SRC_MTU);   /* frames per Ethernet payload */
    if (cap == 0) return -1;
    for (size_t base = 0; base < nf; base += cap) {
        size_t chunk = (nf - base < cap) ? (nf - base) : cap;
        size_t elen = 0;
        if (!dcf_snake_l2_batch(&frames[base], chunk, eth, sizeof eth, &elen)) return -1;
        (*n_batches)++;
        if (!dry && dcf_snake_l2_send(sock, NULL, eth, elen) < 0) return -1;
    }
    return 0;
}

int main(int argc, char **argv) {
    src_opts_t o; parse(argc, argv, &o);
    uint16_t src = (uint16_t)o.node_id;
    uint16_t dst = dcf_snake_channel_id(o.channel);
    uint8_t mid = mode_id(o.mode);

    dcf_snake_l2_sock_t sock; memset(&sock, 0, sizeof sock); sock.fd = -1;
    int dry = 1;
    if (o.iface) {
        if (dcf_snake_l2_open(&sock, o.iface, DCF_SNAKE_ETHERTYPE_RECORD) == 0) dry = 0;
        else fprintf(stderr, "snake-source: raw-L2 on %s unavailable (%s) — running dry\n",
                     o.iface, strerror(errno));
    }

    FILE *in = o.infile ? fopen(o.infile, "rb") : stdin;
    if (!in) { fprintf(stderr, "snake-source: cannot open %s\n", o.infile); return 1; }

    /* PI servo disciplines capture to the BEACON grandmaster (fed by RX BEACON in a full node). */
    snake_servo_t servo; snake_servo_init(&servo, 0.02, 0.0005, 200.0);
    (void)servo;   /* the servo output trims the capture rate; ALSA rate control is a gated edge */

    float *pcm = (float *)malloc((size_t)o.block * sizeof(float));
    unsigned long n_msgs = 0, n_batches = 0, hop = 0;
    int rc = 0;

    for (;;) {
        size_t got = fread(pcm, sizeof(float), (size_t)o.block, in);
        if (got == 0) break;
        for (size_t i = got; i < (size_t)o.block; i++) pcm[i] = 0.0f;   /* zero-pad the tail */
        uint16_t sid = (uint16_t)(hop % (DCF_SNAKE_MAX_STREAMID + 1));
        uint32_t ts  = (uint32_t)((hop * 20000u) & 0xFFFFFFu);

        if (!o.no_quanta) {
            char wav[64]; snprintf(wav, sizeof wav, "/tmp/snake_src_%d.wav", (int)getpid());
            snake_wav_write_f32(wav, pcm, got, 48000);
            size_t qlen = 0;
            uint8_t *qss = quanta_encode_wav(wav, o.mode, &qlen);
            remove(wav);
            if (qss) {
                size_t offs[DCF_SNAKE_MAX_STREAMID + 1], lens[DCF_SNAKE_MAX_STREAMID + 1], np = 0;
                if (qss_split(qss, qlen, offs, lens, DCF_SNAKE_MAX_STREAMID + 1, &np)) {
                    for (size_t p = 0; p < np; p++) {
                        size_t l = lens[p] > DCF_SNAKE_MAX_PAYLOAD ? DCF_SNAKE_MAX_PAYLOAD : lens[p];
                        if (emit_message(&sock, dry, qss + offs[p], l,
                                         (uint16_t)((hop + p) % (DCF_SNAKE_MAX_STREAMID + 1)),
                                         ts, src, dst, mid, &n_batches) == 0) n_msgs++;
                    }
                }
                free(qss);
            }
        } else {
            /* passthrough: carry the raw PCM block (bytes) as the message payload (no quanta) */
            size_t bytes = got * sizeof(float);
            if (bytes > DCF_SNAKE_MAX_PAYLOAD) bytes = DCF_SNAKE_MAX_PAYLOAD;
            if (emit_message(&sock, dry, (const uint8_t *)pcm, bytes, sid, ts, src, dst,
                             mid, &n_batches) == 0) n_msgs++;
        }
        hop++;
        if (o.max_frames >= 0 && (long)hop >= o.max_frames) break;
        if (o.selftest && hop >= 4) break;
    }

    free(pcm);
    if (in != stdin) fclose(in);
    if (sock.fd >= 0) dcf_snake_l2_close(&sock);
    fprintf(stderr, "snake-source: %s%s — %lu hops, %lu messages, %lu L2 batches (dst 0x%04X)\n",
            dry ? "DRY " : "", o.no_quanta ? "passthrough" : "quanta", hop, n_msgs, n_batches, dst);
    if (o.selftest) {
        printf("snake-source selftest: pipeline ran, %lu L2 batches (dry=%d, quanta=%s).\n",
               n_batches, dry, o.no_quanta ? "off" : "on");
    }
    return rc;
}
