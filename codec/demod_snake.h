// SPDX-License-Identifier: LGPL-3.0-only
/*
 * transport/demod_snake.h — DCF-Snake: the record plane of a synchronized cat5e audio snake
 * DeMoD LLC | LGPL-3.0
 *
 * DCF-Snake carries synchronized, quanta-coded audio from a star of source ("spoke") nodes
 * to one central "mixer" hub, for studio multitrack capture.  Like DCF-SSTV/Text/Game it is
 * an ADAPTER over the 17-byte DeModFrame quantum (demod_frame.h), not a new wire format: one
 * self-delimiting quanta QSS commit-hop packet is serialised into 1 + frag_total ordinary
 * frames whose 4-byte payloads carry the (opaque) QSS bytes.  This L2 framing is
 * byte-deterministic across C/Rust/Python — it is pinned by Documentation/snake_vectors.json.
 * See DCF_SNAKE_SPEC.md.
 *
 * The record plane rides CTRL(3) frames (the audio/control family, alongside DCF-Audio) so it
 * never collides with the DATA(0) adapters (text/game/sstv).  It uses a *different* seq split
 * from DCF-Audio (11:5) — DCF-Snake is 5:11, a wider 11-bit fragment index (2047 frags =>
 * 8188 bytes / message) so a whole QSS packet fits one message.  The descriptor mode_id
 * (quanta live/near/relaxed) is an opaque hint that never changes these vectors, exactly like
 * DCF-Audio's codec_id.  A node runs exactly one reassembler per dst channel to tell CTRL
 * adapters apart — never multiplex DCF-Audio and DCF-Snake on the same dst.
 *
 * This header also hosts the BEACON grandmaster media clock (the mixer's clock, byte-certified
 * like the DCF-Mesh REPORT/ROLE bytes) and the certified unwrap_pid timeline primitive ported
 * from client/src-tauri/src/sync.rs.
 *
 * L2 framing (all frames version=1, big-endian; see WIRE_QUANTUM_SPEC.md):
 *   record: type=CTRL(3)   ; BEACON clock: type=BEACON(2)   (same 5:11 fragmenter)
 *   seq (u16) = stream_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
 *   frag_idx 0  descriptor : payload = [len_hi, len_lo, kind, flags]   (kind = mode_id / clk_ver)
 *   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
 *   frag_total = ceil(len/4)  (<= 2047  =>  len <= 8188 bytes / message)
 */
#pragma once
#include "demod_frame.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ── L2 constants ──────────────────────────────────────────────────────────── */
#define DCF_SNAKE_FRAG_BITS   11u
#define DCF_SNAKE_FRAG_MASK   0x7FFu                      /* low 11 bits of seq          */
#define DCF_SNAKE_MAX_FRAGS   2047u                       /* max data fragments          */
#define DCF_SNAKE_MAX_PAYLOAD (DCF_SNAKE_MAX_FRAGS * 4u)  /* 8188 bytes / message        */
#define DCF_SNAKE_MAX_STREAMID 31u                        /* 5-bit stream id             */
#define DCF_SNAKE_MAX_FRAMES  (1u + DCF_SNAKE_MAX_FRAGS)  /* 2048 frames / message       */

/* quanta streaming modes — opaque hints (L2 never parses the QSS bytes). */
#define DCF_SNAKE_MODE_LIVE    0u   /* 64 ms hop  (~98 kbps) — lowest snake latency        */
#define DCF_SNAKE_MODE_NEAR    1u   /* 128 ms hop (~83 kbps)                               */
#define DCF_SNAKE_MODE_RELAXED 2u   /* 256 ms hop (~64 kbps) — lowest bitrate              */

/* Descriptor flag bits — opaque to L2; they do not change the framing bytes. */
#define DCF_SNAKE_FLAG_MORE   0x01u  /* the talkspurt/stream continues in the next stream_id */
#define DCF_SNAKE_FLAG_ANCHOR 0x02u  /* this QSS packet re-anchors (low prediction dep.)     */
#define DCF_SNAKE_FLAG_END    0x04u  /* end-of-talkspurt / end-of-stream                     */

/* BEACON grandmaster media clock. */
#define DCF_SNAKE_CLOCK_LEN   16u    /* bytes in a packed clock payload                     */
#define DCF_SNAKE_CLOCK_VER   1u     /* clk_ver descriptor byte for a beacon message        */
#define DCF_SNAKE_NOMINAL_RATE_MHZ_48K 48000000u  /* 48 kHz in milli-Hz                     */

/* unwrap_pid rolling space (ported verbatim from sync.rs PID_MOD). */
#define DCF_SNAKE_PID_MOD     2048u

/* Map a channel/passphrase to a 16-bit rendezvous dst (crc16, same hash as the rest of the
 * repo).  NULL/"" => broadcast (0xFFFF). */
static inline uint16_t dcf_snake_channel_id(const char *name) {
    if (!name || name[0] == '\0') return 0xFFFFu;
    return dcf_crc16((const uint8_t *)name, strlen(name));
}

/* ── L2: packetize (generic 5:11 fragmenter, shared by CTRL record + BEACON clock) ─────── */
static inline bool dcf__snake_frag_packetize(const uint8_t *payload, size_t payload_len,
                                             uint16_t stream_id, uint32_t ts_us,
                                             uint16_t src, uint16_t dst,
                                             dcf_frame_type_t ftype, uint8_t kind, uint8_t flags,
                                             uint8_t frames[][DCF_FRAME_SIZE],
                                             size_t max_frames, size_t *out_n) {
    if (payload_len > DCF_SNAKE_MAX_PAYLOAD) return false;
    if (stream_id > DCF_SNAKE_MAX_STREAMID)  return false;

    uint16_t frag_total = (uint16_t)((payload_len + 3u) / 4u);
    size_t   need       = (size_t)frag_total + 1u;
    if (need > max_frames) return false;

    dcf_frame_t f;
    f.version = 1u;
    f.type    = ftype;
    f.src_id  = src;
    f.dst_id  = dst;
    f.timestamp_us = ts_us;

    /* frag_idx 0 — descriptor (2-byte big-endian length, kind = mode_id/clk_ver, flags) */
    f.seq = (uint16_t)((uint16_t)stream_id << DCF_SNAKE_FRAG_BITS);
    f.payload[0] = (uint8_t)((payload_len >> 8) & 0xFFu);
    f.payload[1] = (uint8_t)(payload_len & 0xFFu);
    f.payload[2] = kind;
    f.payload[3] = flags;
    dcf_frame_encode(&f, frames[0]);

    /* frag_idx 1..frag_total — data, last chunk zero-padded */
    for (uint16_t k = 1; k <= frag_total; k++) {
        size_t off = (size_t)(k - 1) * 4u;
        uint8_t chunk[4] = {0, 0, 0, 0};
        size_t n = payload_len - off;
        if (n > 4u) n = 4u;
        memcpy(chunk, payload + off, n);
        f.seq = (uint16_t)(((uint16_t)stream_id << DCF_SNAKE_FRAG_BITS) | k);
        memcpy(f.payload, chunk, 4);
        dcf_frame_encode(&f, frames[k]);
    }

    *out_n = need;
    return true;
}

/* Serialise one QSS commit-hop packet into CTRL(3) record frames. */
static inline bool dcf_snake_packetize(const uint8_t *payload, size_t payload_len,
                                       uint16_t stream_id, uint32_t ts_us,
                                       uint16_t src, uint16_t dst,
                                       uint8_t mode_id, uint8_t flags,
                                       uint8_t frames[][DCF_FRAME_SIZE],
                                       size_t max_frames, size_t *out_n) {
    return dcf__snake_frag_packetize(payload, payload_len, stream_id, ts_us, src, dst,
                                     DCF_TYPE_CTRL, mode_id, flags, frames, max_frames, out_n);
}

/* Serialise a 16-byte grandmaster clock payload into BEACON(2) frames (descriptor + 4 data). */
static inline bool dcf_snake_beacon_packetize(const uint8_t clock[DCF_SNAKE_CLOCK_LEN],
                                              uint16_t beacon_slot, uint32_t ts_us,
                                              uint16_t src, uint16_t dst, uint8_t flags,
                                              uint8_t frames[][DCF_FRAME_SIZE],
                                              size_t max_frames, size_t *out_n) {
    return dcf__snake_frag_packetize(clock, DCF_SNAKE_CLOCK_LEN, beacon_slot, ts_us, src, dst,
                                     DCF_TYPE_BEACON, DCF_SNAKE_CLOCK_VER, flags,
                                     frames, max_frames, out_n);
}

/* ── BEACON grandmaster media clock (byte-certified 16-byte payload) ──────────────────── */
typedef struct {
    uint64_t gm_sample_count;   /* monotonic capture-sample index (never wraps)         */
    uint32_t nominal_rate_mhz;  /* nominal sample rate in milli-Hz (48000000 = 48 kHz)  */
    uint16_t tx_seq;            /* beacon sequence (PLL discipline / loss detect)       */
    uint16_t epoch;             /* session epoch (bumps on rate change / mixer restart) */
} dcf_snake_clock_t;

/* Pack the media clock into the 16-byte big-endian BEACON payload. */
static inline void dcf_snake_pack_clock(const dcf_snake_clock_t *c, uint8_t out[DCF_SNAKE_CLOCK_LEN]) {
    out[0] = (uint8_t)(c->gm_sample_count >> 56); out[1] = (uint8_t)(c->gm_sample_count >> 48);
    out[2] = (uint8_t)(c->gm_sample_count >> 40); out[3] = (uint8_t)(c->gm_sample_count >> 32);
    out[4] = (uint8_t)(c->gm_sample_count >> 24); out[5] = (uint8_t)(c->gm_sample_count >> 16);
    out[6] = (uint8_t)(c->gm_sample_count >>  8); out[7] = (uint8_t)(c->gm_sample_count      );
    out[8]  = (uint8_t)(c->nominal_rate_mhz >> 24); out[9]  = (uint8_t)(c->nominal_rate_mhz >> 16);
    out[10] = (uint8_t)(c->nominal_rate_mhz >>  8); out[11] = (uint8_t)(c->nominal_rate_mhz      );
    out[12] = (uint8_t)(c->tx_seq >> 8); out[13] = (uint8_t)(c->tx_seq);
    out[14] = (uint8_t)(c->epoch  >> 8); out[15] = (uint8_t)(c->epoch);
}

/* Inverse of dcf_snake_pack_clock. */
static inline void dcf_snake_unpack_clock(const uint8_t in[DCF_SNAKE_CLOCK_LEN], dcf_snake_clock_t *c) {
    c->gm_sample_count =
        ((uint64_t)in[0] << 56) | ((uint64_t)in[1] << 48) | ((uint64_t)in[2] << 40) |
        ((uint64_t)in[3] << 32) | ((uint64_t)in[4] << 24) | ((uint64_t)in[5] << 16) |
        ((uint64_t)in[6] <<  8) |  (uint64_t)in[7];
    c->nominal_rate_mhz = ((uint32_t)in[8] << 24) | ((uint32_t)in[9] << 16) |
                          ((uint32_t)in[10] << 8) |  (uint32_t)in[11];
    c->tx_seq = (uint16_t)(((uint16_t)in[12] << 8) | in[13]);
    c->epoch  = (uint16_t)(((uint16_t)in[14] << 8) | in[15]);
}

/* ── unwrap_pid — ported verbatim from client/src-tauri/src/sync.rs ───────────────────── */
/* Unwrap a rolling packet_id to a monotonic absolute index relative to prev_abs (forward
 * progress dominates; a small backward delta = reorder → step back / saturating_sub). */
static inline uint64_t dcf_snake_unwrap_pid(uint64_t prev_abs, uint16_t raw, uint32_t mod) {
    uint64_t prev_lo = prev_abs % mod;
    uint64_t r       = (uint64_t)raw % mod;
    uint64_t fwd     = (r + mod - prev_lo) % mod;
    if (fwd <= mod / 2u) return prev_abs + fwd;
    uint64_t back = mod - fwd;
    return prev_abs > back ? prev_abs - back : 0u;   /* saturating_sub */
}

/* ── L2: reassembler ───────────────────────────────────────────────────────── */
#ifndef DCF_SNAKE_REASM_SLOTS
#define DCF_SNAKE_REASM_SLOTS 8    /* concurrent in-flight messages (override for embedded) */
#endif
#define DCF_SNAKE_BITWORDS 32u     /* 32*64 = 2048 bits: data frag idx 1..2047 present-set   */

typedef struct {
    uint16_t stream_id;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  mode_id;   /* kind byte: mode_id (record) or clk_ver (beacon) */
    uint8_t  flags;
    uint16_t payload_len;
    uint8_t  payload[DCF_SNAKE_MAX_PAYLOAD];
} dcf_snake_msg_t;

typedef struct {
    bool     in_use;
    bool     have_desc;
    uint16_t stream_id;
    uint32_t ts_us;
    uint16_t src;
    uint16_t dst;
    uint8_t  mode_id;
    uint8_t  flags;
    uint16_t payload_len;
    uint16_t frag_total;
    uint64_t frags_present[DCF_SNAKE_BITWORDS];  /* bit k = data frag k present (1..2047) */
    uint8_t  data[DCF_SNAKE_MAX_PAYLOAD];
} dcf_snake_slot_t;

typedef struct {
    dcf_frame_type_t accept_type;                /* CTRL(3) for record, BEACON(2) for clock */
    dcf_snake_slot_t slots[DCF_SNAKE_REASM_SLOTS];
} dcf_snake_reasm_t;

typedef enum {
    DCF_SNAKE_REASM_NONE    = 0,   /* frame accepted, message not yet complete           */
    DCF_SNAKE_REASM_MESSAGE = 1,   /* a complete message is in *out_msg                   */
    DCF_SNAKE_REASM_IGNORED = 2,   /* wrong type, a duplicate, or no slot                 */
} dcf_snake_reasm_status_t;

/* Initialise a reassembler filtering the given frame type (DCF_TYPE_CTRL / DCF_TYPE_BEACON). */
static inline void dcf_snake_reasm_init(dcf_snake_reasm_t *r, dcf_frame_type_t accept_type) {
    memset(r, 0, sizeof(*r));
    r->accept_type = accept_type;
}

static inline void dcf__snake_setbit(uint64_t *bits, uint16_t k) {
    bits[k >> 6] |= (uint64_t)1u << (k & 63u);
}
static inline bool dcf__snake_testbit(const uint64_t *bits, uint16_t k) {
    return (bits[k >> 6] >> (k & 63u)) & 1u;
}

static inline dcf_snake_slot_t *dcf__snake_slot_for(dcf_snake_reasm_t *r, uint16_t sid) {
    dcf_snake_slot_t *free_slot = NULL;
    for (size_t i = 0; i < DCF_SNAKE_REASM_SLOTS; i++) {
        if (r->slots[i].in_use && r->slots[i].stream_id == sid) return &r->slots[i];
        if (!r->slots[i].in_use && !free_slot) free_slot = &r->slots[i];
    }
    if (free_slot) {
        memset(free_slot, 0, sizeof(*free_slot));
        free_slot->in_use    = true;
        free_slot->stream_id = sid;
    }
    return free_slot;
}

static inline dcf_snake_reasm_status_t dcf__snake_slot_emit(dcf_snake_slot_t *s,
                                                            dcf_snake_msg_t *out) {
    if (!s->have_desc) return DCF_SNAKE_REASM_NONE;
    for (uint16_t k = 1; k <= s->frag_total; k++)
        if (!dcf__snake_testbit(s->frags_present, k)) return DCF_SNAKE_REASM_NONE;
    out->stream_id   = s->stream_id;
    out->ts_us       = s->ts_us;
    out->src         = s->src;
    out->dst         = s->dst;
    out->mode_id     = s->mode_id;
    out->flags       = s->flags;
    out->payload_len = s->payload_len;
    memcpy(out->payload, s->data, s->payload_len);
    memset(s, 0, sizeof(*s));   /* frees the slot */
    return DCF_SNAKE_REASM_MESSAGE;
}

/*
 * Push one 17-byte frame.  On DCF_SNAKE_REASM_MESSAGE, *out_msg holds the assembled message.
 * Duplicates and frames of the wrong type are ignored.  Mirrors the Python/Rust refs.
 */
static inline dcf_snake_reasm_status_t dcf_snake_reasm_push(dcf_snake_reasm_t *r,
                                                            const uint8_t frame[DCF_FRAME_SIZE],
                                                            dcf_snake_msg_t *out_msg) {
    dcf_frame_t d;
    if (!dcf_frame_decode(frame, &d))    return DCF_SNAKE_REASM_IGNORED;
    if (d.type != r->accept_type)        return DCF_SNAKE_REASM_IGNORED;

    uint16_t sid      = (uint16_t)(d.seq >> DCF_SNAKE_FRAG_BITS);
    uint16_t frag_idx = (uint16_t)(d.seq & DCF_SNAKE_FRAG_MASK);

    dcf_snake_slot_t *s = dcf__snake_slot_for(r, sid);
    if (!s) return DCF_SNAKE_REASM_IGNORED;  /* table full */

    s->ts_us = d.timestamp_us;
    s->src   = d.src_id;
    s->dst   = d.dst_id;
    if (frag_idx == 0) {
        if (!s->have_desc) {
            s->have_desc   = true;
            s->payload_len = (uint16_t)(((uint16_t)d.payload[0] << 8) | d.payload[1]);
            s->frag_total  = (uint16_t)((s->payload_len + 3u) / 4u);
            s->mode_id     = d.payload[2];
            s->flags       = d.payload[3];
        }
    } else if (frag_idx <= DCF_SNAKE_MAX_FRAGS) {
        if (!dcf__snake_testbit(s->frags_present, frag_idx)) {
            dcf__snake_setbit(s->frags_present, frag_idx);
            memcpy(s->data + (size_t)(frag_idx - 1) * 4u, d.payload, 4);
        }
    }
    return dcf__snake_slot_emit(s, out_msg);
}

/*
 * Report every still-incomplete message as lost (ascending stream_id) and clear the
 * reassembler.  Returns the count; up to `max` ids are written to `lost`.
 */
static inline size_t dcf_snake_reasm_finalize(dcf_snake_reasm_t *r,
                                              uint16_t *lost, size_t max) {
    size_t n = 0;
    for (size_t i = 0; i < DCF_SNAKE_REASM_SLOTS; i++)
        if (r->slots[i].in_use && n < max) lost[n++] = r->slots[i].stream_id;
    for (size_t i = 1; i < n; i++) {
        uint16_t v = lost[i]; size_t j = i;
        while (j > 0 && lost[j - 1] > v) { lost[j] = lost[j - 1]; j--; }
        lost[j] = v;
    }
    dcf_frame_type_t at = r->accept_type;
    dcf_snake_reasm_init(r, at);
    return n;
}
