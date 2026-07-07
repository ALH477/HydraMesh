// SPDX-License-Identifier: LGPL-3.0-only
/*
 * transport/demod_monitor.h — DCF-Cue: the low-latency PCM cue plane of the audio snake
 * DeMoD LLC | LGPL-3.0
 *
 * The cue plane is the bidirectional uncompressed-PCM monitor bus of the snake, on a second
 * cat5e wire: source nodes send tiny (~1 ms) PCM blocks up to the mixer; the mixer returns
 * each node its own cue mix.  No atom-pursuit encode, so it escapes the record plane's
 * ≥64 ms latency floor (~4–6 ms round-trip).  Like the record plane it is an ADAPTER over
 * the 17-byte DeModFrame quantum (demod_frame.h): one PCM block is fragmented into ordinary
 * CTRL(3) frames carrying raw little-endian PCM.  Byte-deterministic across C/Rust/Python —
 * pinned by Documentation/monitor_vectors.json.  See DCF_SNAKE_SPEC.md.
 *
 * The cue plane rides its own dedicated cat5e wire (a separate EtherType), so it never
 * contends with or is confused for the record plane, even though both use CTRL(3).  Uplink
 * (source→mixer) and downlink (mixer→node cue) share this framing, told apart by direction
 * (src/dst) and the FLAG_CUE_RETURN descriptor bit.
 *
 * L2 framing (all frames version=1, type=CTRL(3), big-endian; see WIRE_QUANTUM_SPEC.md):
 *   seq (u16) = block_seq[15:7] (9 bits, 0..511) | frag_idx[6:0] (7 bits, 0..127)
 *   frag_idx 0  descriptor : payload = [block_samples, format, channels, flags]
 *   frag_idx k  data       : payload = pcm[(k-1)*4 .. +4]  (last frame zero-padded)
 *   length = block_samples * channels * bytes_per_sample(format)  (derived, self-describing)
 *   frag_total = ceil(length/4)  (<= 127  =>  length <= 508 bytes / block)
 */
#pragma once
#include "demod_frame.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ── L2 constants ──────────────────────────────────────────────────────────── */
#define DCF_MON_FRAG_BITS     7u
#define DCF_MON_FRAG_MASK     0x7Fu                       /* low 7 bits of seq            */
#define DCF_MON_MAX_FRAGS     127u                        /* max data fragments           */
#define DCF_MON_MAX_PAYLOAD   (DCF_MON_MAX_FRAGS * 4u)    /* 508 bytes / block            */
#define DCF_MON_MAX_BLOCK_SEQ 511u                        /* 9-bit rolling block counter  */
#define DCF_MON_BLOCK_SEQ_MOD 512u                        /* unwrap_pid modulus (cue)     */
#define DCF_MON_MAX_FRAMES    (1u + DCF_MON_MAX_FRAGS)    /* 128 frames / block           */

/* PCM sample formats (little-endian on the wire). */
#define DCF_MON_FMT_S16   0u   /* signed 16-bit (2 bytes/sample)  */
#define DCF_MON_FMT_S24   1u   /* signed 24-bit (3 bytes/sample)  */
#define DCF_MON_FMT_F32   2u   /* 32-bit float  (4 bytes/sample)  */

/* Descriptor flag bits — opaque to L2 (they do not change the framing bytes). */
#define DCF_MON_FLAG_CUE_RETURN 0x01u  /* downlink: mixer → node cue/monitor feed (vs uplink) */
#define DCF_MON_FLAG_END        0x02u  /* last block of a stream / talkspurt                  */

/* Bytes per sample for a format id, or 0 if unknown. */
static inline uint8_t dcf_mon_bytes_per_sample(uint8_t format_id) {
    switch (format_id) {
        case DCF_MON_FMT_S16: return 2u;
        case DCF_MON_FMT_S24: return 3u;
        case DCF_MON_FMT_F32: return 4u;
        default:              return 0u;
    }
}

/* Self-describing PCM byte-length of a block of the given geometry (0 on unknown format). */
static inline size_t dcf_mon_pcm_len(uint8_t block_samples, uint8_t format_id, uint8_t channels) {
    return (size_t)block_samples * (size_t)channels * (size_t)dcf_mon_bytes_per_sample(format_id);
}

static inline uint16_t dcf_mon_channel_id(const char *name) {
    if (!name || name[0] == '\0') return 0xFFFFu;
    return dcf_crc16((const uint8_t *)name, strlen(name));
}

/* ── L2: packetize ─────────────────────────────────────────────────────────── */
/* Serialise one PCM block into CTRL(3) frames.  `pcm_len` must equal the geometry-derived
 * length; returns false on mismatch, unknown format, oversize, or bad block_seq. */
static inline bool dcf_mon_packetize(const uint8_t *pcm, size_t pcm_len,
                                     uint16_t block_seq, uint32_t ts_us,
                                     uint16_t src, uint16_t dst,
                                     uint8_t block_samples, uint8_t format_id, uint8_t channels,
                                     uint8_t flags,
                                     uint8_t frames[][DCF_FRAME_SIZE],
                                     size_t max_frames, size_t *out_n) {
    if (block_seq > DCF_MON_MAX_BLOCK_SEQ) return false;
    size_t expect = dcf_mon_pcm_len(block_samples, format_id, channels);
    if (expect == 0 || pcm_len != expect)  return false;
    if (pcm_len > DCF_MON_MAX_PAYLOAD)     return false;

    uint16_t frag_total = (uint16_t)((pcm_len + 3u) / 4u);
    size_t   need       = (size_t)frag_total + 1u;
    if (need > max_frames) return false;

    dcf_frame_t f;
    f.version = 1u;
    f.type    = DCF_TYPE_CTRL;
    f.src_id  = src;
    f.dst_id  = dst;
    f.timestamp_us = ts_us;

    f.seq = (uint16_t)((uint16_t)block_seq << DCF_MON_FRAG_BITS);
    f.payload[0] = block_samples;
    f.payload[1] = format_id;
    f.payload[2] = channels;
    f.payload[3] = flags;
    dcf_frame_encode(&f, frames[0]);

    for (uint16_t k = 1; k <= frag_total; k++) {
        size_t off = (size_t)(k - 1) * 4u;
        uint8_t chunk[4] = {0, 0, 0, 0};
        size_t n = pcm_len - off;
        if (n > 4u) n = 4u;
        memcpy(chunk, pcm + off, n);
        f.seq = (uint16_t)(((uint16_t)block_seq << DCF_MON_FRAG_BITS) | k);
        memcpy(f.payload, chunk, 4);
        dcf_frame_encode(&f, frames[k]);
    }

    *out_n = need;
    return true;
}

/* ── L2: reassembler ───────────────────────────────────────────────────────── */
#ifndef DCF_MON_REASM_SLOTS
#define DCF_MON_REASM_SLOTS 8
#endif
#define DCF_MON_BITWORDS 2u        /* 2*64 = 128 bits: data frag idx 1..127 present-set */

typedef struct {
    uint16_t block_seq;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  block_samples;
    uint8_t  format;
    uint8_t  channels;
    uint8_t  flags;
    uint16_t payload_len;
    uint8_t  payload[DCF_MON_MAX_PAYLOAD];
} dcf_mon_block_t;

typedef struct {
    bool     in_use;
    bool     have_desc;
    uint16_t block_seq;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  block_samples;
    uint8_t  format;
    uint8_t  channels;
    uint8_t  flags;
    uint16_t payload_len;
    uint16_t frag_total;
    uint64_t frags_present[DCF_MON_BITWORDS];
    uint8_t  data[DCF_MON_MAX_PAYLOAD];
} dcf_mon_slot_t;

typedef struct {
    dcf_mon_slot_t slots[DCF_MON_REASM_SLOTS];
} dcf_mon_reasm_t;

typedef enum {
    DCF_MON_REASM_NONE    = 0,
    DCF_MON_REASM_BLOCK   = 1,
    DCF_MON_REASM_IGNORED = 2,
} dcf_mon_reasm_status_t;

static inline void dcf_mon_reasm_init(dcf_mon_reasm_t *r) {
    memset(r, 0, sizeof(*r));
}

static inline void dcf__mon_setbit(uint64_t *bits, uint16_t k) {
    bits[k >> 6] |= (uint64_t)1u << (k & 63u);
}
static inline bool dcf__mon_testbit(const uint64_t *bits, uint16_t k) {
    return (bits[k >> 6] >> (k & 63u)) & 1u;
}

static inline dcf_mon_slot_t *dcf__mon_slot_for(dcf_mon_reasm_t *r, uint16_t bseq) {
    dcf_mon_slot_t *free_slot = NULL;
    for (size_t i = 0; i < DCF_MON_REASM_SLOTS; i++) {
        if (r->slots[i].in_use && r->slots[i].block_seq == bseq) return &r->slots[i];
        if (!r->slots[i].in_use && !free_slot) free_slot = &r->slots[i];
    }
    if (free_slot) {
        memset(free_slot, 0, sizeof(*free_slot));
        free_slot->in_use    = true;
        free_slot->block_seq = bseq;
    }
    return free_slot;
}

static inline dcf_mon_reasm_status_t dcf__mon_slot_emit(dcf_mon_slot_t *s, dcf_mon_block_t *out) {
    if (!s->have_desc) return DCF_MON_REASM_NONE;
    for (uint16_t k = 1; k <= s->frag_total; k++)
        if (!dcf__mon_testbit(s->frags_present, k)) return DCF_MON_REASM_NONE;
    out->block_seq     = s->block_seq;
    out->ts_us         = s->ts_us;
    out->src           = s->src;
    out->dst           = s->dst;
    out->block_samples = s->block_samples;
    out->format        = s->format;
    out->channels      = s->channels;
    out->flags         = s->flags;
    out->payload_len   = s->payload_len;
    memcpy(out->payload, s->data, s->payload_len);
    memset(s, 0, sizeof(*s));
    return DCF_MON_REASM_BLOCK;
}

/* Push one 17-byte frame.  On DCF_MON_REASM_BLOCK, *out_blk holds the assembled PCM block. */
static inline dcf_mon_reasm_status_t dcf_mon_reasm_push(dcf_mon_reasm_t *r,
                                                        const uint8_t frame[DCF_FRAME_SIZE],
                                                        dcf_mon_block_t *out_blk) {
    dcf_frame_t d;
    if (!dcf_frame_decode(frame, &d))  return DCF_MON_REASM_IGNORED;
    if (d.type != DCF_TYPE_CTRL)       return DCF_MON_REASM_IGNORED;

    uint16_t bseq     = (uint16_t)(d.seq >> DCF_MON_FRAG_BITS);
    uint16_t frag_idx = (uint16_t)(d.seq & DCF_MON_FRAG_MASK);

    dcf_mon_slot_t *s = dcf__mon_slot_for(r, bseq);
    if (!s) return DCF_MON_REASM_IGNORED;

    s->ts_us = d.timestamp_us;
    s->src   = d.src_id;
    s->dst   = d.dst_id;
    if (frag_idx == 0) {
        if (!s->have_desc) {
            s->block_samples = d.payload[0];
            s->format        = d.payload[1];
            s->channels      = d.payload[2];
            s->flags         = d.payload[3];
            size_t len       = dcf_mon_pcm_len(s->block_samples, s->format, s->channels);
            if (len == 0 || len > DCF_MON_MAX_PAYLOAD) return DCF_MON_REASM_IGNORED;
            s->payload_len = (uint16_t)len;
            s->frag_total  = (uint16_t)((len + 3u) / 4u);
            s->have_desc   = true;
        }
    } else if (frag_idx <= DCF_MON_MAX_FRAGS) {
        if (!dcf__mon_testbit(s->frags_present, frag_idx)) {
            dcf__mon_setbit(s->frags_present, frag_idx);
            memcpy(s->data + (size_t)(frag_idx - 1) * 4u, d.payload, 4);
        }
    }
    return dcf__mon_slot_emit(s, out_blk);
}

/* Report every still-incomplete block as lost (ascending block_seq) and clear. */
static inline size_t dcf_mon_reasm_finalize(dcf_mon_reasm_t *r, uint16_t *lost, size_t max) {
    size_t n = 0;
    for (size_t i = 0; i < DCF_MON_REASM_SLOTS; i++)
        if (r->slots[i].in_use && n < max) lost[n++] = r->slots[i].block_seq;
    for (size_t i = 1; i < n; i++) {
        uint16_t v = lost[i]; size_t j = i;
        while (j > 0 && lost[j - 1] > v) { lost[j] = lost[j - 1]; j--; }
        lost[j] = v;
    }
    dcf_mon_reasm_init(r);
    return n;
}
