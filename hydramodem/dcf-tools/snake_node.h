// SPDX-License-Identifier: LGPL-3.0-only
/*
 * snake_node.h — shared node glue for the DCF-Snake spoke/mixer, DeMoD LLC | LGPL-3.0.
 *   - qss_split / qss_join : the quanta ⇄ DCF-Snake bridge in C (sync-scan + CRC resync),
 *     byte-identical to python/dcf/quanta.py.  Pure + unit-testable.
 *   - quanta_encode / quanta_decode : shell out to quanta-stream / quanta-stream-decode
 *     (GPL; `nix build .#quanta`) on temp WAV/QSS files — never linked (mere aggregation).
 *   - minimal 32-bit-float WAV read/write for that subprocess boundary.
 * The quanta binaries are optional: quanta_available() is false without them and callers fall
 * back / skip.  See DCF_SNAKE_SPEC.md.
 */
#pragma once
#include "../../codec/demod_frame.h"   /* dcf_crc16 (== quanta qss_crc16, DCF convention) */
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define QSS_HDR_BYTES 40u
#define QSS_SYNC_HI   0xA5u
#define QSS_SYNC_LO   0x5Au

/* True iff a candidate packet [sync|hop..body|crc16] carries a valid trailing CRC. */
static inline bool qss__pkt_crc_ok(const uint8_t *p, size_t len) {
    if (len < 4u || p[0] != QSS_SYNC_HI || p[1] != QSS_SYNC_LO) return false;
    uint16_t crc = dcf_crc16(p + 2, len - 4u);
    return crc == (uint16_t)(((uint16_t)p[len - 2] << 8) | p[len - 1]);
}

/*
 * Split a QSS byte stream into packets.  `stream`/`len` in; on return `offs[i]`/`lens[i]` point
 * at packet i within `stream` (i < *n_pkts, capped at `max_pkts`).  The 40-byte header is at
 * stream[0..39].  Delimits by scanning 0xA55A sync words and validating each candidate's CRC
 * (resyncing past a body byte-pair that merely looks like a sync).  Returns false on a bad/short
 * header.  Version-independent — needs no atom/band knowledge.
 */
static inline bool qss_split(const uint8_t *stream, size_t len,
                             size_t *offs, size_t *lens, size_t max_pkts, size_t *n_pkts) {
    *n_pkts = 0;
    if (len < QSS_HDR_BYTES) return false;
    uint32_t magic = ((uint32_t)stream[0] << 24) | ((uint32_t)stream[1] << 16) |
                     ((uint32_t)stream[2] << 8) | stream[3];
    if (magic != 0x51535331u && magic != 0x51535332u) return false;
    if (dcf_crc16(stream, 32u) != (uint16_t)(((uint16_t)stream[32] << 8) | stream[33]))
        return false;

    const uint8_t *body = stream + QSS_HDR_BYTES;
    size_t n = len - QSS_HDR_BYTES;
    size_t i = 0;
    while (i + 1 < n && !(body[i] == QSS_SYNC_HI && body[i + 1] == QSS_SYNC_LO)) i++;
    while (i + 1 < n && body[i] == QSS_SYNC_HI && body[i + 1] == QSS_SYNC_LO) {
        /* find the smallest end (next sync or EOF) at which the packet CRC validates */
        size_t end = i + 2;
        bool ok = false;
        while (end <= n) {
            bool at_sync = (end + 1 < n && body[end] == QSS_SYNC_HI && body[end + 1] == QSS_SYNC_LO);
            if (at_sync || end == n) {
                if (qss__pkt_crc_ok(body + i, end - i)) { ok = true; break; }
            }
            end++;
        }
        if (!ok) break;                         /* truncated / trailing garbage */
        if (*n_pkts < max_pkts) { offs[*n_pkts] = QSS_HDR_BYTES + i; lens[*n_pkts] = end - i; (*n_pkts)++; }
        i = end;
    }
    return true;
}

/* ── quanta subprocess (optional GPL dependency) ─────────────────────────────────────────── */
static inline const char *quanta_stream_bin(void) {
    const char *e = getenv("QUANTA_STREAM"); return e ? e : "quanta-stream";
}
static inline const char *quanta_decode_bin(void) {
    const char *e = getenv("QUANTA_STREAM_DECODE"); return e ? e : "quanta-stream-decode";
}

/* Minimal 32-bit-float mono WAV writer (48 kHz) for the quanta subprocess boundary. */
static inline bool snake_wav_write_f32(const char *path, const float *pcm, size_t n, uint32_t rate) {
    FILE *f = fopen(path, "wb");
    if (!f) return false;
    uint32_t data = (uint32_t)(n * 4u), riff = 36u + data, byte_rate = rate * 4u;
    uint16_t fmt = 3, ch = 1, ba = 4, bits = 32;   /* WAVE_FORMAT_IEEE_FLOAT */
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); uint32_t sz16 = 16; fwrite(&sz16, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&rate, 4, 1, f);
    fwrite(&byte_rate, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&data, 4, 1, f);
    fwrite(pcm, 4, n, f);
    fclose(f);
    return true;
}

/* Run quanta-stream on a WAV; returns malloc'd QSS bytes (caller frees) or NULL. *out_len set. */
static inline uint8_t *quanta_encode_wav(const char *wav, const char *mode, size_t *out_len) {
    char qss[256], cmd[1024];
    snprintf(qss, sizeof qss, "%s.qss", wav);
    snprintf(cmd, sizeof cmd, "'%s' '%s' -o '%s' --mode %s >/dev/null 2>&1",
             quanta_stream_bin(), wav, qss, mode ? mode : "live");
    if (system(cmd) != 0) return NULL;
    FILE *f = fopen(qss, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = (sz > 0) ? (uint8_t *)malloc((size_t)sz) : NULL;
    if (buf && fread(buf, 1, (size_t)sz, f) == (size_t)sz) *out_len = (size_t)sz;
    else { free(buf); buf = NULL; }
    fclose(f);
    remove(qss);
    return buf;
}
