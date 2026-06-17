// SPDX-License-Identifier: LGPL-3.0-only
/*
 * transport/demod_text.h — DCF-Text: chat / agent-to-agent text transport over DeModFrame
 * DeMoD LLC | LGPL-3.0
 *
 * Text traffic is an ADAPTER over the 17-byte DeModFrame quantum (demod_frame.h), not a
 * new wire format — exactly like DCF-Audio (demod_audio.h) and DCF-Game (demod_game.h).
 * One UTF-8 message is serialised into 1 + frag_total ordinary DATA frames.  This L2
 * framing is byte-deterministic across C/Rust/Python — it is pinned by
 * Documentation/text_vectors.json.  See DCF_TEXT_SPEC.md.
 *
 * Audio uses CTRL(3); text and game ride DATA(0) but use different seq splits (text's
 * 10-bit fragment index vs game's 5-bit); the descriptor that opens every message tells
 * a receiver which adapter owns the packet_id.
 *
 * L2 framing (all frames version=1, type=DATA(0); see WIRE_QUANTUM_SPEC.md):
 *   seq (u16) = packet_id[15:10] (6 bits, 0..63) | frag_idx[9:0] (10 bits, 0..1023)
 *   frag_idx 0  descriptor : payload = [len_hi, len_lo, flags, 0]
 *   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
 *   frag_total = ceil(len/4)  (<= 1023  =>  len <= 4092 bytes / message)
 */
#pragma once
#include "demod_frame.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ── L2 constants ──────────────────────────────────────────────────────────── */
#define DCF_TEXT_FRAG_BITS    10u
#define DCF_TEXT_FRAG_MASK    0x3FFu                  /* low 10 bits of seq         */
#define DCF_TEXT_MAX_FRAGS    1023u                   /* max data fragments         */
#define DCF_TEXT_MAX_PAYLOAD  (DCF_TEXT_MAX_FRAGS * 4u)  /* 4092 bytes / message    */
#define DCF_TEXT_MAX_PACKETID 63u                     /* 6-bit packet id            */
#define DCF_TEXT_MAX_FRAMES   (1u + DCF_TEXT_MAX_FRAGS)  /* 1024 frames / message   */

/* Descriptor flag bits — opaque to L2; they do not change the framing bytes. */
#define DCF_TEXT_FLAG_AGENT    0x01u
#define DCF_TEXT_FLAG_MORE     0x02u
#define DCF_TEXT_FLAG_RELIABLE 0x04u

/* Map a channel/passphrase to a 16-bit rendezvous dst (crc16, same hash as the rest of
 * the repo).  NULL/"" => broadcast (0xFFFF). */
static inline uint16_t dcf_text_channel_id(const char *name) {
    if (!name || name[0] == '\0') return 0xFFFFu;
    return dcf_crc16((const uint8_t *)name, strlen(name));
}

/* ── L2: packetize ─────────────────────────────────────────────────────────── */
/*
 * Serialise one UTF-8 message into DeModFrame DATA frames.
 * `frames` must hold at least 1+ceil(payload_len/4) rows of DCF_FRAME_SIZE bytes.
 * Returns false on invalid args or insufficient room; on success *out_n is the
 * number of frames written (descriptor first, then data fragments in order).
 */
static inline bool dcf_text_packetize(const uint8_t *payload, size_t payload_len,
                                      uint16_t packet_id, uint32_t ts_us,
                                      uint16_t src, uint16_t dst, uint8_t flags,
                                      uint8_t frames[][DCF_FRAME_SIZE],
                                      size_t max_frames, size_t *out_n) {
    if (payload_len > DCF_TEXT_MAX_PAYLOAD) return false;
    if (packet_id > DCF_TEXT_MAX_PACKETID)  return false;

    uint16_t frag_total = (uint16_t)((payload_len + 3u) / 4u);
    size_t   need       = (size_t)frag_total + 1u;
    if (need > max_frames) return false;

    dcf_frame_t f;
    f.version = 1u;
    f.type    = DCF_TYPE_DATA;
    f.src_id  = src;
    f.dst_id  = dst;
    f.timestamp_us = ts_us;

    /* frag_idx 0 — descriptor (2-byte big-endian length) */
    f.seq = (uint16_t)((uint16_t)packet_id << DCF_TEXT_FRAG_BITS);
    f.payload[0] = (uint8_t)((payload_len >> 8) & 0xFFu);
    f.payload[1] = (uint8_t)(payload_len & 0xFFu);
    f.payload[2] = flags;
    f.payload[3] = 0u;
    dcf_frame_encode(&f, frames[0]);

    /* frag_idx 1..frag_total — data, last chunk zero-padded */
    for (uint16_t k = 1; k <= frag_total; k++) {
        size_t off = (size_t)(k - 1) * 4u;
        uint8_t chunk[4] = {0, 0, 0, 0};
        size_t n = payload_len - off;
        if (n > 4u) n = 4u;
        memcpy(chunk, payload + off, n);
        f.seq = (uint16_t)(((uint16_t)packet_id << DCF_TEXT_FRAG_BITS) | k);
        memcpy(f.payload, chunk, 4);
        dcf_frame_encode(&f, frames[k]);
    }

    *out_n = need;
    return true;
}

/* ── L2: reassembler ───────────────────────────────────────────────────────── */
#ifndef DCF_TEXT_REASM_SLOTS
#define DCF_TEXT_REASM_SLOTS 8    /* concurrent in-flight messages (override for embedded) */
#endif
#define DCF_TEXT_BITWORDS 16u     /* 16*64 = 1024 bits: data frag idx 1..1023 present-set */

typedef struct {
    uint16_t packet_id;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  flags;
    uint16_t payload_len;
    uint8_t  payload[DCF_TEXT_MAX_PAYLOAD];
} dcf_text_packet_t;

typedef struct {
    bool     in_use;
    bool     have_desc;
    uint16_t packet_id;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  flags;
    uint16_t payload_len;
    uint16_t frag_total;
    uint64_t frags_present[DCF_TEXT_BITWORDS];  /* bit k = data frag k present (1..1023) */
    uint8_t  data[DCF_TEXT_MAX_PAYLOAD];
} dcf_text_slot_t;

typedef struct {
    dcf_text_slot_t slots[DCF_TEXT_REASM_SLOTS];
} dcf_text_reasm_t;

typedef enum {
    DCF_TEXT_REASM_NONE    = 0,   /* frame accepted, message not yet complete       */
    DCF_TEXT_REASM_MESSAGE = 1,   /* a complete message is in *out_msg               */
    DCF_TEXT_REASM_IGNORED = 2,   /* not a DATA text frame, a duplicate, or no slot  */
} dcf_text_reasm_status_t;

static inline void dcf_text_reasm_init(dcf_text_reasm_t *r) {
    memset(r, 0, sizeof(*r));
}

static inline void dcf__text_setbit(uint64_t *bits, uint16_t k) {
    bits[k >> 6] |= (uint64_t)1u << (k & 63u);
}
static inline bool dcf__text_testbit(const uint64_t *bits, uint16_t k) {
    return (bits[k >> 6] >> (k & 63u)) & 1u;
}

static inline dcf_text_slot_t *dcf__text_slot_for(dcf_text_reasm_t *r, uint16_t pid) {
    dcf_text_slot_t *free_slot = NULL;
    for (size_t i = 0; i < DCF_TEXT_REASM_SLOTS; i++) {
        if (r->slots[i].in_use && r->slots[i].packet_id == pid) return &r->slots[i];
        if (!r->slots[i].in_use && !free_slot) free_slot = &r->slots[i];
    }
    if (free_slot) {
        memset(free_slot, 0, sizeof(*free_slot));
        free_slot->in_use    = true;
        free_slot->packet_id = pid;
    }
    return free_slot;
}

static inline dcf_text_reasm_status_t dcf__text_slot_emit(dcf_text_slot_t *s,
                                                          dcf_text_packet_t *out) {
    if (!s->have_desc) return DCF_TEXT_REASM_NONE;
    for (uint16_t k = 1; k <= s->frag_total; k++)
        if (!dcf__text_testbit(s->frags_present, k)) return DCF_TEXT_REASM_NONE;
    out->packet_id   = s->packet_id;
    out->ts_us       = s->ts_us;
    out->src         = s->src;
    out->dst         = s->dst;
    out->flags       = s->flags;
    out->payload_len = s->payload_len;
    memcpy(out->payload, s->data, s->payload_len);
    memset(s, 0, sizeof(*s));   /* frees the slot */
    return DCF_TEXT_REASM_MESSAGE;
}

/*
 * Push one 17-byte frame.  On DCF_TEXT_REASM_MESSAGE, *out_msg holds the assembled
 * message.  Duplicates and non-text (non-DATA) frames are ignored.  Mirrors the refs.
 */
static inline dcf_text_reasm_status_t dcf_text_reasm_push(dcf_text_reasm_t *r,
                                                          const uint8_t frame[DCF_FRAME_SIZE],
                                                          dcf_text_packet_t *out_msg) {
    dcf_frame_t d;
    if (!dcf_frame_decode(frame, &d))      return DCF_TEXT_REASM_IGNORED;
    if (d.type != DCF_TYPE_DATA)           return DCF_TEXT_REASM_IGNORED;

    uint16_t pid      = (uint16_t)(d.seq >> DCF_TEXT_FRAG_BITS);
    uint16_t frag_idx = (uint16_t)(d.seq & DCF_TEXT_FRAG_MASK);

    dcf_text_slot_t *s = dcf__text_slot_for(r, pid);
    if (!s) return DCF_TEXT_REASM_IGNORED;  /* table full */

    s->ts_us = d.timestamp_us;
    s->src   = d.src_id;
    s->dst   = d.dst_id;
    if (frag_idx == 0) {
        if (!s->have_desc) {
            s->have_desc   = true;
            s->payload_len = (uint16_t)(((uint16_t)d.payload[0] << 8) | d.payload[1]);
            s->frag_total  = (uint16_t)((s->payload_len + 3u) / 4u);
            s->flags       = d.payload[2];
        }
    } else if (frag_idx <= DCF_TEXT_MAX_FRAGS) {
        if (!dcf__text_testbit(s->frags_present, frag_idx)) {
            dcf__text_setbit(s->frags_present, frag_idx);
            memcpy(s->data + (size_t)(frag_idx - 1) * 4u, d.payload, 4);
        }
    }
    return dcf__text_slot_emit(s, out_msg);
}

/*
 * Report every still-incomplete message as lost (ascending packet_id) and clear the
 * reassembler.  Returns the count; up to `max` ids are written to `lost`.
 */
static inline size_t dcf_text_reasm_finalize(dcf_text_reasm_t *r,
                                             uint16_t *lost, size_t max) {
    size_t n = 0;
    for (size_t i = 0; i < DCF_TEXT_REASM_SLOTS; i++)
        if (r->slots[i].in_use && n < max) lost[n++] = r->slots[i].packet_id;
    for (size_t i = 1; i < n; i++) {
        uint16_t v = lost[i]; size_t j = i;
        while (j > 0 && lost[j - 1] > v) { lost[j] = lost[j - 1]; j--; }
        lost[j] = v;
    }
    dcf_text_reasm_init(r);
    return n;
}
