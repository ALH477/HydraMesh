// SPDX-License-Identifier: LGPL-3.0-only
/*
 * transport/demod_sstv.h — DCF-SSTV: slow-scan television (still-image) transport over DeModFrame
 * DeMoD LLC | LGPL-3.0
 *
 * Image traffic is an ADAPTER over the 17-byte DeModFrame quantum (demod_frame.h), not a
 * new wire format — exactly like DCF-Text (demod_text.h) and DCF-Game (demod_game.h).  One
 * still image is serialised into 1 + frag_total ordinary DATA frames.  This L2 framing is
 * byte-deterministic across C/Rust/Python/Go/Node — it is pinned by
 * Documentation/sstv_vectors.json.  See DCF_SSTV_SPEC.md.
 *
 * Text and game ride DATA(0) too, but with different seq splits (text 6:10, game 11:5,
 * sstv 5:11 — a wider fragment index for larger images).  The image bytes are opaque; the
 * descriptor's format_id is a hint (JPEG/PNG/raw) that never changes these vectors.  A node
 * runs exactly one reassembler per dst channel to tell the DATA adapters apart.
 *
 * L2 framing (all frames version=1, type=DATA(0); see WIRE_QUANTUM_SPEC.md):
 *   seq (u16) = image_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
 *   frag_idx 0  descriptor : payload = [len_hi, len_lo, format_id, flags]
 *   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
 *   frag_total = ceil(len/4)  (<= 2047  =>  len <= 8188 bytes / image)
 */
#pragma once
#include "demod_frame.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ── L2 constants ──────────────────────────────────────────────────────────── */
#define DCF_SSTV_FRAG_BITS    11u
#define DCF_SSTV_FRAG_MASK    0x7FFu                    /* low 11 bits of seq         */
#define DCF_SSTV_MAX_FRAGS    2047u                     /* max data fragments         */
#define DCF_SSTV_MAX_PAYLOAD  (DCF_SSTV_MAX_FRAGS * 4u) /* 8188 bytes / image         */
#define DCF_SSTV_MAX_IMAGEID  31u                       /* 5-bit image id             */
#define DCF_SSTV_MAX_FRAMES   (1u + DCF_SSTV_MAX_FRAGS) /* 2048 frames / image        */

/* Image format ids — opaque hints (L2 never parses the bytes). */
#define DCF_SSTV_FMT_RAW      0u
#define DCF_SSTV_FMT_JPEG     1u
#define DCF_SSTV_FMT_PNG      2u
#define DCF_SSTV_FMT_WEBP     3u
#define DCF_SSTV_FMT_RGB565   4u

/* Descriptor flag bits — opaque to L2; they do not change the framing bytes. */
#define DCF_SSTV_FLAG_MORE     0x01u
#define DCF_SSTV_FLAG_KEYFRAME 0x02u
#define DCF_SSTV_FLAG_RELIABLE 0x04u

/* Map a channel/passphrase to a 16-bit rendezvous dst (crc16, same hash as the rest of
 * the repo).  NULL/"" => broadcast (0xFFFF). */
static inline uint16_t dcf_sstv_channel_id(const char *name) {
    if (!name || name[0] == '\0') return 0xFFFFu;
    return dcf_crc16((const uint8_t *)name, strlen(name));
}

/* ── L2: packetize ─────────────────────────────────────────────────────────── */
/*
 * Serialise one image into DeModFrame DATA frames.
 * `frames` must hold at least 1+ceil(payload_len/4) rows of DCF_FRAME_SIZE bytes.
 * Returns false on invalid args or insufficient room; on success *out_n is the
 * number of frames written (descriptor first, then data fragments in order).
 */
static inline bool dcf_sstv_packetize(const uint8_t *payload, size_t payload_len,
                                      uint16_t image_id, uint32_t ts_us,
                                      uint16_t src, uint16_t dst,
                                      uint8_t format_id, uint8_t flags,
                                      uint8_t frames[][DCF_FRAME_SIZE],
                                      size_t max_frames, size_t *out_n) {
    if (payload_len > DCF_SSTV_MAX_PAYLOAD) return false;
    if (image_id > DCF_SSTV_MAX_IMAGEID)    return false;

    uint16_t frag_total = (uint16_t)((payload_len + 3u) / 4u);
    size_t   need       = (size_t)frag_total + 1u;
    if (need > max_frames) return false;

    dcf_frame_t f;
    f.version = 1u;
    f.type    = DCF_TYPE_DATA;
    f.src_id  = src;
    f.dst_id  = dst;
    f.timestamp_us = ts_us;

    /* frag_idx 0 — descriptor (2-byte big-endian length, format_id, flags) */
    f.seq = (uint16_t)((uint16_t)image_id << DCF_SSTV_FRAG_BITS);
    f.payload[0] = (uint8_t)((payload_len >> 8) & 0xFFu);
    f.payload[1] = (uint8_t)(payload_len & 0xFFu);
    f.payload[2] = format_id;
    f.payload[3] = flags;
    dcf_frame_encode(&f, frames[0]);

    /* frag_idx 1..frag_total — data, last chunk zero-padded */
    for (uint16_t k = 1; k <= frag_total; k++) {
        size_t off = (size_t)(k - 1) * 4u;
        uint8_t chunk[4] = {0, 0, 0, 0};
        size_t n = payload_len - off;
        if (n > 4u) n = 4u;
        memcpy(chunk, payload + off, n);
        f.seq = (uint16_t)(((uint16_t)image_id << DCF_SSTV_FRAG_BITS) | k);
        memcpy(f.payload, chunk, 4);
        dcf_frame_encode(&f, frames[k]);
    }

    *out_n = need;
    return true;
}

/* ── L2: reassembler ───────────────────────────────────────────────────────── */
#ifndef DCF_SSTV_REASM_SLOTS
#define DCF_SSTV_REASM_SLOTS 4    /* concurrent in-flight images (override for embedded) */
#endif
#define DCF_SSTV_BITWORDS 32u     /* 32*64 = 2048 bits: data frag idx 1..2047 present-set */

typedef struct {
    uint16_t image_id;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  format_id;
    uint8_t  flags;
    uint16_t payload_len;
    uint8_t  payload[DCF_SSTV_MAX_PAYLOAD];
} dcf_sstv_image_t;

typedef struct {
    bool     in_use;
    bool     have_desc;
    uint16_t image_id;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  format_id;
    uint8_t  flags;
    uint16_t payload_len;
    uint16_t frag_total;
    uint64_t frags_present[DCF_SSTV_BITWORDS];  /* bit k = data frag k present (1..2047) */
    uint8_t  data[DCF_SSTV_MAX_PAYLOAD];
} dcf_sstv_slot_t;

typedef struct {
    dcf_sstv_slot_t slots[DCF_SSTV_REASM_SLOTS];
} dcf_sstv_reasm_t;

typedef enum {
    DCF_SSTV_REASM_NONE    = 0,   /* frame accepted, image not yet complete            */
    DCF_SSTV_REASM_IMAGE   = 1,   /* a complete image is in *out_img                    */
    DCF_SSTV_REASM_IGNORED = 2,   /* not a DATA sstv frame, a duplicate, or no slot     */
} dcf_sstv_reasm_status_t;

static inline void dcf_sstv_reasm_init(dcf_sstv_reasm_t *r) {
    memset(r, 0, sizeof(*r));
}

static inline void dcf__sstv_setbit(uint64_t *bits, uint16_t k) {
    bits[k >> 6] |= (uint64_t)1u << (k & 63u);
}
static inline bool dcf__sstv_testbit(const uint64_t *bits, uint16_t k) {
    return (bits[k >> 6] >> (k & 63u)) & 1u;
}

static inline dcf_sstv_slot_t *dcf__sstv_slot_for(dcf_sstv_reasm_t *r, uint16_t iid) {
    dcf_sstv_slot_t *free_slot = NULL;
    for (size_t i = 0; i < DCF_SSTV_REASM_SLOTS; i++) {
        if (r->slots[i].in_use && r->slots[i].image_id == iid) return &r->slots[i];
        if (!r->slots[i].in_use && !free_slot) free_slot = &r->slots[i];
    }
    if (free_slot) {
        memset(free_slot, 0, sizeof(*free_slot));
        free_slot->in_use   = true;
        free_slot->image_id = iid;
    }
    return free_slot;
}

static inline dcf_sstv_reasm_status_t dcf__sstv_slot_emit(dcf_sstv_slot_t *s,
                                                          dcf_sstv_image_t *out) {
    if (!s->have_desc) return DCF_SSTV_REASM_NONE;
    for (uint16_t k = 1; k <= s->frag_total; k++)
        if (!dcf__sstv_testbit(s->frags_present, k)) return DCF_SSTV_REASM_NONE;
    out->image_id    = s->image_id;
    out->ts_us       = s->ts_us;
    out->src         = s->src;
    out->dst         = s->dst;
    out->format_id   = s->format_id;
    out->flags       = s->flags;
    out->payload_len = s->payload_len;
    memcpy(out->payload, s->data, s->payload_len);
    memset(s, 0, sizeof(*s));   /* frees the slot */
    return DCF_SSTV_REASM_IMAGE;
}

/*
 * Push one 17-byte frame.  On DCF_SSTV_REASM_IMAGE, *out_img holds the assembled image.
 * Duplicates and non-sstv (non-DATA) frames are ignored.  Mirrors the refs.
 */
static inline dcf_sstv_reasm_status_t dcf_sstv_reasm_push(dcf_sstv_reasm_t *r,
                                                          const uint8_t frame[DCF_FRAME_SIZE],
                                                          dcf_sstv_image_t *out_img) {
    dcf_frame_t d;
    if (!dcf_frame_decode(frame, &d))      return DCF_SSTV_REASM_IGNORED;
    if (d.type != DCF_TYPE_DATA)           return DCF_SSTV_REASM_IGNORED;

    uint16_t iid      = (uint16_t)(d.seq >> DCF_SSTV_FRAG_BITS);
    uint16_t frag_idx = (uint16_t)(d.seq & DCF_SSTV_FRAG_MASK);

    dcf_sstv_slot_t *s = dcf__sstv_slot_for(r, iid);
    if (!s) return DCF_SSTV_REASM_IGNORED;  /* table full */

    s->ts_us = d.timestamp_us;
    s->src   = d.src_id;
    s->dst   = d.dst_id;
    if (frag_idx == 0) {
        if (!s->have_desc) {
            s->have_desc   = true;
            s->payload_len = (uint16_t)(((uint16_t)d.payload[0] << 8) | d.payload[1]);
            s->frag_total  = (uint16_t)((s->payload_len + 3u) / 4u);
            s->format_id   = d.payload[2];
            s->flags       = d.payload[3];
        }
    } else if (frag_idx <= DCF_SSTV_MAX_FRAGS) {
        if (!dcf__sstv_testbit(s->frags_present, frag_idx)) {
            dcf__sstv_setbit(s->frags_present, frag_idx);
            memcpy(s->data + (size_t)(frag_idx - 1) * 4u, d.payload, 4);
        }
    }
    return dcf__sstv_slot_emit(s, out_img);
}

/*
 * Report every still-incomplete image as lost (ascending image_id) and clear the
 * reassembler.  Returns the count; up to `max` ids are written to `lost`.
 */
static inline size_t dcf_sstv_reasm_finalize(dcf_sstv_reasm_t *r,
                                             uint16_t *lost, size_t max) {
    size_t n = 0;
    for (size_t i = 0; i < DCF_SSTV_REASM_SLOTS; i++)
        if (r->slots[i].in_use && n < max) lost[n++] = r->slots[i].image_id;
    for (size_t i = 1; i < n; i++) {
        uint16_t v = lost[i]; size_t j = i;
        while (j > 0 && lost[j - 1] > v) { lost[j] = lost[j - 1]; j--; }
        lost[j] = v;
    }
    dcf_sstv_reasm_init(r);
    return n;
}
