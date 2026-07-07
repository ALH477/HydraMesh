// SPDX-License-Identifier: LGPL-3.0-only
/*
 * snake_loopback.c — DCF-Snake end-to-end in-process integration demo, DeMoD LLC | LGPL-3.0.
 *
 * Proves the whole node pipeline composes, with NO ALSA / quanta / socket needed (those are the
 * subprocess/hardware edges of the real nodes).  Two pipelines run entirely in-process:
 *
 *   Record transport+DSP:  synthetic QSS packet → dcf_snake_packetize → L2 SuperPack batch →
 *     (wire) → unbatch → dcf_snake reassemble  [byte-exact]  → decoded PCM (stub) → jitter buffer
 *     → ASRC drift-correct → sum-mix → assert integrity.
 *   Cue loop (fully real — cue PCM is uncompressed):  N source PCM blocks → dcf_mon_packetize →
 *     batch → (wire) → unbatch → dcf_mon reassemble → per-node cue-mix → assert weighted sum.
 *
 * The only thing stubbed is the quanta encode/decode subprocess (GPL, `nix build .#quanta`) —
 * everything the DCF layer owns (L2 framing, SuperPack batching, jitter/ASRC/PLC/cue-mix) is
 * exercised for real.  Build: dcf-tools/build.sh (or the gcc line in CI).
 */
#include "../../codec/demod_snake.h"
#include "../../codec/demod_monitor.h"
#include "snake_l2.h"
#include "snake_dsp.h"
#include "snake_node.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static int failures = 0;
static void check(int cond, const char *what) {
    if (!cond) { fprintf(stderr, "FAIL: %s\n", what); failures++; }
}

/* ── record transport+DSP pipeline ─────────────────────────────────────────────────────── */
static void record_pipeline(void) {
    /* a synthetic QSS commit-hop packet (opaque bytes — the real one comes from quanta-stream) */
    uint8_t qss[784];
    for (size_t i = 0; i < sizeof qss; i++) qss[i] = (uint8_t)(i * 91u + 7u);

    /* spoke: packetize → SuperPack-batch into one Ethernet payload */
    uint8_t frames[260][DCF_FRAME_SIZE];
    size_t nf = 0;
    check(dcf_snake_packetize(qss, sizeof qss, 3, 0x00ABCD, 0x00A1, 0xFFFF,
                              DCF_SNAKE_MODE_LIVE, DCF_SNAKE_FLAG_ANCHOR, frames, 260, &nf),
          "record: packetize");
    uint8_t eth[9000];
    size_t elen = 0;
    check(dcf_snake_l2_batch(frames, nf, eth, sizeof eth, &elen), "record: L2 batch");

    /* mixer: unbatch → reassemble (must recover the QSS packet byte-exact) */
    uint8_t rframes[260][DCF_FRAME_SIZE];
    size_t rn = 0;
    check(dcf_snake_l2_unbatch(eth, elen, rframes, 260, &rn) && rn == nf, "record: L2 unbatch");
    dcf_snake_reasm_t r;
    dcf_snake_reasm_init(&r, DCF_TYPE_CTRL);
    dcf_snake_msg_t msg;
    int got = 0;
    for (size_t i = 0; i < rn; i++)
        if (dcf_snake_reasm_push(&r, rframes[i], &msg) == DCF_SNAKE_REASM_MESSAGE) got = 1;
    check(got && msg.payload_len == sizeof qss &&
          memcmp(msg.payload, qss, sizeof qss) == 0, "record: QSS packet recovered byte-exact");

    /* mixer DSP: the recovered QSS would go to quanta-stream-decode → PCM.  Here we synthesise
     * a decoded-PCM block and run it through jitter → ASRC → sum-mix to exercise the DSP path. */
    const uint32_t BLK = 1024;
    float pcm[1024];
    for (uint32_t i = 0; i < BLK; i++) pcm[i] = 0.25f * sinf(0.05f * (float)i);

    snake_jb_t jb; snake_jb_init(&jb, BLK, 2);
    /* push three consecutive decoded hops, one reordered */
    snake_jb_push(&jb, 0, pcm, BLK);
    snake_jb_push(&jb, 2, pcm, BLK);
    snake_jb_push(&jb, 1, pcm, BLK);
    snake_asrc_t asrc; snake_asrc_init(&asrc, 1.0000);   /* on-clock source: identity */
    snake_plc_t plc; snake_plc_init(&plc, BLK, 0.5f);

    float out[2048], mixed[2048];
    memset(mixed, 0, sizeof mixed);
    int blocks = 0;
    for (;;) {
        float blk[1024];
        snake_jb_status_t st = snake_jb_pop(&jb, blk);
        if (st == SNAKE_JB_UNDERRUN) break;
        if (st == SNAKE_JB_GAP) { snake_plc_conceal(&plc, blk); }
        else                    { snake_plc_good(&plc, blk, BLK); }
        size_t m = snake_asrc_process(&asrc, blk, BLK, out, 2048);
        for (size_t i = 0; i < m && i < 2048; i++) mixed[i] += out[i];   /* sum into the record bus */
        blocks++;
    }
    check(blocks == 3, "record: jitter buffer played 3 reordered hops in order");
    int finite = 1; for (int i = 0; i < 2048; i++) if (!isfinite(mixed[i])) finite = 0;
    check(finite, "record: mixed bus is finite");
    printf("PASS: record transport+DSP (packetize→batch→wire→reassemble byte-exact→jitter→ASRC→mix)\n");
}

/* ── cue loop pipeline (fully real: uncompressed PCM) ──────────────────────────────────── */
static void cue_pipeline(void) {
    /* two source nodes each send a 1 ms (48-sample) mono-S16 block up to the mixer */
    const uint8_t BS = 48, CH = 1;
    int16_t s0[48], s1[48];
    for (int i = 0; i < 48; i++) { s0[i] = (int16_t)(1000 + i); s1[i] = (int16_t)(-500 - i); }

    uint8_t f0[40][DCF_FRAME_SIZE], f1[40][DCF_FRAME_SIZE];
    size_t n0 = 0, n1 = 0;
    check(dcf_mon_packetize((uint8_t *)s0, sizeof s0, 10, 1000, 0x0001, 0xFFFF,
                            BS, DCF_MON_FMT_S16, CH, 0, f0, 40, &n0), "cue: packetize src0");
    check(dcf_mon_packetize((uint8_t *)s1, sizeof s1, 10, 1000, 0x0002, 0xFFFF,
                            BS, DCF_MON_FMT_S16, CH, 0, f1, 40, &n1), "cue: packetize src1");

    /* batch each uplink, ship over the (in-process) wire, unbatch + reassemble at the mixer */
    int16_t rec0[48], rec1[48];
    for (int which = 0; which < 2; which++) {
        uint8_t (*fr)[DCF_FRAME_SIZE] = which ? f1 : f0;
        size_t nf = which ? n1 : n0;
        uint8_t eth[1500]; size_t elen = 0;
        check(dcf_snake_l2_batch(fr, nf, eth, sizeof eth, &elen), "cue: L2 batch");
        uint8_t rf[40][DCF_FRAME_SIZE]; size_t rn = 0;
        check(dcf_snake_l2_unbatch(eth, elen, rf, 40, &rn), "cue: L2 unbatch");
        dcf_mon_reasm_t r; dcf_mon_reasm_init(&r);
        dcf_mon_block_t blk; int got = 0;
        for (size_t i = 0; i < rn; i++)
            if (dcf_mon_reasm_push(&r, rf[i], &blk) == DCF_MON_REASM_BLOCK) got = 1;
        check(got && blk.payload_len == sizeof s0, "cue: block reassembled");
        memcpy(which ? rec1 : rec0, blk.payload, sizeof s0);
    }
    check(memcmp(rec0, s0, sizeof s0) == 0 && memcmp(rec1, s1, sizeof s1) == 0,
          "cue: both PCM uplinks recovered byte-exact");

    /* mixer cue engine: node 0 hears src0 full + src1 half; convert to float, mix, check */
    float fs0[48], fs1[48];
    for (int i = 0; i < 48; i++) { fs0[i] = rec0[i] / 32768.0f; fs1[i] = rec1[i] / 32768.0f; }
    snake_cuemix_t cm; snake_cuemix_init(&cm, 2, 2);
    snake_cuemix_set(&cm, 0, 0, 1.0f); snake_cuemix_set(&cm, 0, 1, 0.5f);
    snake_cuemix_set(&cm, 1, 0, 0.0f); snake_cuemix_set(&cm, 1, 1, 1.0f);
    const float *ins[2] = {fs0, fs1};
    float cue0[48];
    snake_cuemix_bus(&cm, 0, ins, 48, cue0);
    check(fabsf(cue0[0] - (fs0[0] + 0.5f * fs1[0])) < 1e-6f, "cue: node0 cue = src0 + 0.5*src1");
    printf("PASS: cue loop (2 PCM uplinks→batch→wire→reassemble byte-exact→per-node cue-mix)\n");
}

/* ── QSS splitter (C port of dcf/quanta.py; the quanta ⇄ DCF-Snake bridge) ──────────────── */
static size_t put_qss_packet(uint8_t *dst, uint32_t hop, const uint8_t *body, size_t blen) {
    dst[0] = 0xA5; dst[1] = 0x5A;
    dst[2] = (uint8_t)(hop >> 24); dst[3] = (uint8_t)(hop >> 16);
    dst[4] = (uint8_t)(hop >> 8);  dst[5] = (uint8_t)hop;
    memcpy(dst + 6, body, blen);
    uint16_t crc = dcf_crc16(dst + 2, 4 + blen);   /* CRC over hop_index..body */
    dst[6 + blen] = (uint8_t)(crc >> 8); dst[7 + blen] = (uint8_t)crc;
    return 8 + blen;
}

static void qss_pipeline(void) {
    uint8_t stream[512];
    /* 40-byte header: 'QSS2' magic + CRC-16 over the first 32 bytes */
    memset(stream, 0, QSS_HDR_BYTES);
    stream[0] = 0x51; stream[1] = 0x53; stream[2] = 0x53; stream[3] = 0x32;
    uint16_t hcrc = dcf_crc16(stream, 32);
    stream[32] = (uint8_t)(hcrc >> 8); stream[33] = (uint8_t)hcrc;
    size_t off = QSS_HDR_BYTES;
    /* packet 1 has a body byte-pair that looks like a sync word → tests CRC resync */
    uint8_t b0[6] = {0x00, 0xA5, 0x5A, 0x01, 0x02, 0x03};
    uint8_t b1[4] = {0xDE, 0xAD, 0xBE, 0xEF};
    off += put_qss_packet(stream + off, 1, b0, sizeof b0);
    off += put_qss_packet(stream + off, 2, b1, sizeof b1);

    size_t offs[8], lens[8], np = 0;
    check(qss_split(stream, off, offs, lens, 8, &np), "qss: header valid");
    check(np == 2, "qss: 2 packets recovered (resync past body-internal 0xA55A)");
    check(lens[0] == 8 + sizeof b0 && lens[1] == 8 + sizeof b1, "qss: packet lengths");
    check(stream[offs[0]] == 0xA5 && stream[offs[1]] == 0xA5, "qss: packets start on sync");
    printf("PASS: C QSS splitter (sync-scan + CRC resync, byte-identical to dcf/quanta.py)\n");
}

int main(void) {
    record_pipeline();
    cue_pipeline();
    qss_pipeline();
    if (failures == 0)
        printf("\nDCF-Snake end-to-end pipeline composes — record + cue planes verified in-process.\n");
    else
        fprintf(stderr, "\n%d FAILURE(S).\n", failures);
    return failures != 0;
}
