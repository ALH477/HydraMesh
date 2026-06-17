// SPDX-License-Identifier: LGPL-3.0-only
/*
 * transport/demod_game.h — DCF-Game: low-latency multiplayer transport over DeModFrame
 * DeMoD LLC | LGPL-3.0
 *
 * Game traffic is an ADAPTER over the 17-byte DeModFrame quantum (demod_frame.h), not a
 * new wire format — exactly like DCF-Audio (demod_audio.h).  One game message (a state
 * snapshot, an input frame, or an opaque event) is serialised into 1 + frag_total ordinary
 * DATA frames.  This L2 framing is message-type-agnostic and byte-deterministic across
 * C/Rust/Python — it is pinned by Documentation/game_vectors.json.  See DCF_GAME_SPEC.md.
 *
 * Audio uses CTRL(3); game uses DATA(0), so the two adapters never collide on the wire.
 *
 * Layers in this header:
 *   L2  dcf_game_packetize / dcf_game_reasm_*    — framing (always compiled, no deps)
 *   L1  dcf_snapshot_pack/unpack  (msg_type 0)   — 14-byte player state (certified)
 *       dcf_input_pack/unpack     (msg_type 1)   — 6-byte input frame (certified)
 *       dcf_join_pack/unpack      (msg_type 3)   — player id + name (certified)
 *
 * L2 framing (all frames version=1, type=DATA(0); see WIRE_QUANTUM_SPEC.md):
 *   seq (u16) = packet_id[15:5] (11 bits) | frag_idx[4:0] (5 bits)
 *   frag_idx 0  descriptor : payload = [payload_len, frag_total, msg_type_id, flags]
 *   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
 *   frag_total = ceil(payload_len/4)  (<= 31  =>  payload_len <= 124 bytes / message)
 */
#pragma once
#include "demod_frame.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ── L2 constants ──────────────────────────────────────────────────────────── */
#define DCF_GAME_FRAG_BITS    5u
#define DCF_GAME_FRAG_MASK    0x1Fu                  /* low 5 bits of seq          */
#define DCF_GAME_MAX_FRAGS    31u                    /* max data fragments         */
#define DCF_GAME_MAX_PAYLOAD  (DCF_GAME_MAX_FRAGS * 4u)  /* 124 bytes / message    */
#define DCF_GAME_MAX_PACKETID 2047u                  /* 11-bit packet id           */
#define DCF_GAME_MAX_FRAMES   (1u + DCF_GAME_MAX_FRAGS)  /* 32 frames / message    */

/* Message-type registry ids (profiles in DCF_GAME_SPEC.md). */
#define DCF_GMSG_SNAPSHOT  0u
#define DCF_GMSG_INPUT     1u
#define DCF_GMSG_EVENT     2u
#define DCF_GMSG_JOIN      3u
/* ids 4..255 reserved */

/* Descriptor flag bits — request transport behaviour; do not change the L2 bytes. */
#define DCF_GAME_FLAG_RELIABLE 0x01u
#define DCF_GAME_FLAG_ORDERED  0x02u
#define DCF_GAME_FLAG_END_TICK 0x04u

/* ── L2: packetize ─────────────────────────────────────────────────────────── */
/*
 * Serialise one game message into DeModFrame DATA frames.
 * `frames` must hold at least 1+ceil(payload_len/4) rows of DCF_FRAME_SIZE bytes.
 * Returns false on invalid args or insufficient room; on success *out_n is the
 * number of frames written (descriptor first, then data fragments in order).
 */
static inline bool dcf_game_packetize(uint8_t msg_type_id, const uint8_t *payload,
                                      size_t payload_len, uint16_t packet_id,
                                      uint32_t ts_us, uint16_t src, uint16_t dst,
                                      uint8_t flags,
                                      uint8_t frames[][DCF_FRAME_SIZE],
                                      size_t max_frames, size_t *out_n) {
    if (payload_len > DCF_GAME_MAX_PAYLOAD) return false;
    if (packet_id > DCF_GAME_MAX_PACKETID)  return false;

    uint8_t frag_total = (uint8_t)((payload_len + 3u) / 4u);
    size_t  need       = (size_t)frag_total + 1u;
    if (need > max_frames) return false;

    dcf_frame_t f;
    f.version = 1u;
    f.type    = DCF_TYPE_DATA;
    f.src_id  = src;
    f.dst_id  = dst;
    f.timestamp_us = ts_us;

    /* frag_idx 0 — descriptor */
    f.seq = (uint16_t)((uint16_t)packet_id << DCF_GAME_FRAG_BITS);
    f.payload[0] = (uint8_t)payload_len;
    f.payload[1] = frag_total;
    f.payload[2] = msg_type_id;
    f.payload[3] = flags;
    dcf_frame_encode(&f, frames[0]);

    /* frag_idx 1..frag_total — data, last chunk zero-padded */
    for (uint8_t k = 1; k <= frag_total; k++) {
        size_t off = (size_t)(k - 1) * 4u;
        uint8_t chunk[4] = {0, 0, 0, 0};
        size_t n = payload_len - off;
        if (n > 4u) n = 4u;
        memcpy(chunk, payload + off, n);
        f.seq = (uint16_t)(((uint16_t)packet_id << DCF_GAME_FRAG_BITS) | k);
        memcpy(f.payload, chunk, 4);
        dcf_frame_encode(&f, frames[k]);
    }

    *out_n = need;
    return true;
}

/* ── L2: reassembler ───────────────────────────────────────────────────────── */
#ifndef DCF_GAME_REASM_SLOTS
#define DCF_GAME_REASM_SLOTS 16   /* concurrent in-flight messages                 */
#endif

typedef struct {
    uint16_t packet_id;
    uint32_t ts_us;
    uint8_t  msg_type_id;
    uint8_t  flags;
    uint8_t  payload_len;
    uint8_t  payload[DCF_GAME_MAX_PAYLOAD];
} dcf_game_packet_t;

typedef struct {
    bool     in_use;
    bool     have_desc;
    uint16_t packet_id;
    uint32_t ts_us;
    uint8_t  msg_type_id;
    uint8_t  flags;
    uint8_t  payload_len;
    uint8_t  frag_total;
    uint32_t frags_present;                 /* bit k = data frag k present (1..31) */
    uint8_t  data[DCF_GAME_MAX_PAYLOAD];
} dcf_game_slot_t;

typedef struct {
    dcf_game_slot_t slots[DCF_GAME_REASM_SLOTS];
} dcf_game_reasm_t;

typedef enum {
    DCF_GAME_REASM_NONE    = 0,   /* frame accepted, message not yet complete       */
    DCF_GAME_REASM_PACKET  = 1,   /* a complete message is in *out_pkt               */
    DCF_GAME_REASM_IGNORED = 2,   /* not a DATA game frame, a duplicate, or no slot  */
} dcf_game_reasm_status_t;

static inline void dcf_game_reasm_init(dcf_game_reasm_t *r) {
    memset(r, 0, sizeof(*r));
}

static inline uint32_t dcf__game_need_mask(uint8_t frag_total) {
    return frag_total ? ((((uint32_t)1u << frag_total) - 1u) << 1) : 0u;
}

static inline dcf_game_slot_t *dcf__game_slot_for(dcf_game_reasm_t *r, uint16_t pid) {
    dcf_game_slot_t *free_slot = NULL;
    for (size_t i = 0; i < DCF_GAME_REASM_SLOTS; i++) {
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

static inline dcf_game_reasm_status_t dcf__game_slot_emit(dcf_game_reasm_t *r,
                                                          dcf_game_slot_t *s,
                                                          dcf_game_packet_t *out) {
    if (!s->have_desc) return DCF_GAME_REASM_NONE;
    uint32_t need = dcf__game_need_mask(s->frag_total);
    if ((s->frags_present & need) != need) return DCF_GAME_REASM_NONE;
    out->packet_id   = s->packet_id;
    out->ts_us       = s->ts_us;
    out->msg_type_id = s->msg_type_id;
    out->flags       = s->flags;
    out->payload_len = s->payload_len;
    memcpy(out->payload, s->data, s->payload_len);
    memset(s, 0, sizeof(*s));   /* frees the slot */
    (void)r;
    return DCF_GAME_REASM_PACKET;
}

/*
 * Push one 17-byte frame.  On DCF_GAME_REASM_PACKET, *out_pkt holds the assembled
 * message.  Duplicates and non-game (non-DATA) frames are ignored.  Mirrors the refs.
 */
static inline dcf_game_reasm_status_t dcf_game_reasm_push(dcf_game_reasm_t *r,
                                                          const uint8_t frame[DCF_FRAME_SIZE],
                                                          dcf_game_packet_t *out_pkt) {
    dcf_frame_t d;
    if (!dcf_frame_decode(frame, &d))      return DCF_GAME_REASM_IGNORED;
    if (d.type != DCF_TYPE_DATA)           return DCF_GAME_REASM_IGNORED;

    uint16_t pid      = (uint16_t)(d.seq >> DCF_GAME_FRAG_BITS);
    uint8_t  frag_idx = (uint8_t)(d.seq & DCF_GAME_FRAG_MASK);

    dcf_game_slot_t *s = dcf__game_slot_for(r, pid);
    if (!s) return DCF_GAME_REASM_IGNORED;  /* table full */

    s->ts_us = d.timestamp_us;
    if (frag_idx == 0) {
        if (!s->have_desc) {
            s->have_desc   = true;
            s->payload_len = d.payload[0];
            s->frag_total  = d.payload[1];
            s->msg_type_id = d.payload[2];
            s->flags       = d.payload[3];
        }
    } else if (frag_idx <= DCF_GAME_MAX_FRAGS) {
        uint32_t bit = (uint32_t)1u << frag_idx;
        if (!(s->frags_present & bit)) {
            s->frags_present |= bit;
            memcpy(s->data + (size_t)(frag_idx - 1) * 4u, d.payload, 4);
        }
    }
    return dcf__game_slot_emit(r, s, out_pkt);
}

/*
 * Report every still-incomplete message as lost (ascending packet_id) and clear the
 * reassembler.  Returns the count; up to `max` ids are written to `lost`.
 */
static inline size_t dcf_game_reasm_finalize(dcf_game_reasm_t *r,
                                             uint16_t *lost, size_t max) {
    size_t n = 0;
    for (size_t i = 0; i < DCF_GAME_REASM_SLOTS; i++)
        if (r->slots[i].in_use && n < max) lost[n++] = r->slots[i].packet_id;
    for (size_t i = 1; i < n; i++) {
        uint16_t v = lost[i]; size_t j = i;
        while (j > 0 && lost[j - 1] > v) { lost[j] = lost[j - 1]; j--; }
        lost[j] = v;
    }
    dcf_game_reasm_init(r);
    return n;
}

/* ── L1: SNAPSHOT body (msg_type 0) — 14-byte player state, Q8.8 fixed-point ── */
#define DCF_SNAPSHOT_LEN 14u

typedef struct {
    float    x, y, z;       /* metres                                              */
    float    vx, vy, vz;    /* metres per tick                                     */
    uint16_t yaw;           /* 0..65535 maps 0..2pi                                */
} dcf_snapshot_t;

static inline void dcf__q88(float v, uint8_t out[2]) {
    float r = v * 256.0f + (v >= 0 ? 0.5f : -0.5f);
    long  q = (long)r;
    if (q > 32767) q = 32767;
    if (q < -32768) q = -32768;
    uint16_t u = (uint16_t)(int16_t)q;
    out[0] = (uint8_t)(u >> 8);
    out[1] = (uint8_t)(u & 0xFF);
}
static inline float dcf__unq88(const uint8_t b[2]) {
    int16_t q = (int16_t)(((uint16_t)b[0] << 8) | b[1]);
    return (float)q / 256.0f;
}

/* Pack a snapshot into 14 deterministic bytes.  The byte layout is certified. */
static inline void dcf_snapshot_pack(const dcf_snapshot_t *s, uint8_t out[DCF_SNAPSHOT_LEN]) {
    dcf__q88(s->x,  out + 0);
    dcf__q88(s->y,  out + 2);
    dcf__q88(s->z,  out + 4);
    dcf__q88(s->vx, out + 6);
    dcf__q88(s->vy, out + 8);
    dcf__q88(s->vz, out + 10);
    out[12] = (uint8_t)(s->yaw >> 8);
    out[13] = (uint8_t)(s->yaw & 0xFF);
}
/* Unpack 14 bytes.  dcf_snapshot_pack(dcf_snapshot_unpack(b)) == b. */
static inline void dcf_snapshot_unpack(const uint8_t b[DCF_SNAPSHOT_LEN], dcf_snapshot_t *s) {
    s->x  = dcf__unq88(b + 0);
    s->y  = dcf__unq88(b + 2);
    s->z  = dcf__unq88(b + 4);
    s->vx = dcf__unq88(b + 6);
    s->vy = dcf__unq88(b + 8);
    s->vz = dcf__unq88(b + 10);
    s->yaw = (uint16_t)(((uint16_t)b[12] << 8) | b[13]);
}

/* ── L1: INPUT body (msg_type 1) — 6 bytes: tick u32 + buttons u16 bitfield ─── */
#define DCF_INPUT_LEN 6u

typedef struct {
    uint32_t tick;
    uint16_t buttons;
} dcf_input_t;

static inline void dcf_input_pack(const dcf_input_t *p, uint8_t out[DCF_INPUT_LEN]) {
    out[0] = (uint8_t)(p->tick >> 24);
    out[1] = (uint8_t)(p->tick >> 16);
    out[2] = (uint8_t)(p->tick >> 8);
    out[3] = (uint8_t)(p->tick & 0xFF);
    out[4] = (uint8_t)(p->buttons >> 8);
    out[5] = (uint8_t)(p->buttons & 0xFF);
}
static inline void dcf_input_unpack(const uint8_t b[DCF_INPUT_LEN], dcf_input_t *p) {
    p->tick = ((uint32_t)b[0] << 24) | ((uint32_t)b[1] << 16) |
              ((uint32_t)b[2] << 8) | (uint32_t)b[3];
    p->buttons = (uint16_t)(((uint16_t)b[4] << 8) | b[5]);
}

/* ── L1: JOIN body (msg_type 3) — player_id u16 + len-prefixed UTF-8 name ───── */
/* Writes 3 + name_len bytes into out (which must hold up to DCF_GAME_MAX_PAYLOAD).
 * name_len is clamped to DCF_GAME_MAX_PAYLOAD-3.  Returns total bytes written. */
static inline size_t dcf_join_pack(uint16_t player_id, const char *name, size_t name_len,
                                   uint8_t *out) {
    if (name_len > DCF_GAME_MAX_PAYLOAD - 3u) name_len = DCF_GAME_MAX_PAYLOAD - 3u;
    out[0] = (uint8_t)(player_id >> 8);
    out[1] = (uint8_t)(player_id & 0xFF);
    out[2] = (uint8_t)name_len;
    memcpy(out + 3, name, name_len);
    return 3u + name_len;
}
/* Parses player_id and name (not NUL-terminated; *name points into `in`). */
static inline bool dcf_join_unpack(const uint8_t *in, size_t in_len, uint16_t *player_id,
                                   const char **name, size_t *name_len) {
    if (in_len < 3u) return false;
    *player_id = (uint16_t)(((uint16_t)in[0] << 8) | in[1]);
    size_t n = in[2];
    if (3u + n > in_len) n = in_len - 3u;
    *name = (const char *)(in + 3);
    *name_len = n;
    return true;
}
