// SPDX-License-Identifier: LGPL-3.0-only
/*
 * transport/demod_audio.h — DCF-Audio: collaborative audio over the DeModFrame wire
 * DeMoD LLC | LGPL-3.0
 *
 * Audio is an ADAPTER over the 17-byte DeModFrame quantum (demod_frame.h), not a new
 * wire format.  A 20 ms codec block is serialised into 1 + frag_total ordinary CTRL
 * frames.  This L2 framing is codec-agnostic and byte-deterministic across C/Rust/Python
 * — it is pinned by Documentation/audio_vectors.json.  See Documentation/DCF_AUDIO_SPEC.md.
 *
 * Layers in this header:
 *   L2  dcf_audio_packetize / dcf_audio_reasm_*   — framing (always compiled, no deps)
 *   L1  dcf_pcm_diag_*  (codec_id 1)              — byte-deterministic reference codec
 *       dcf_pm_pack / dcf_pm_unpack  (codec_id 2) — PM 8-byte param layout (certified)
 *       dcf_codec_get / vtable                    — codec registry
 *       Opus  (codec_id 0)   behind  DCF_AUDIO_OPUS  (needs <opus/opus.h>, -lopus)
 *       Faust-PM synthesis   behind  DCF_AUDIO_PM    (needs faust/dcf_pm_codec.gen.c)
 *   L3  dcf_jitterbuf_*                           behind  DCF_AUDIO_RT
 *
 * L2 framing (all frames version=1, type=CTRL(3); see WIRE_QUANTUM_SPEC.md):
 *   seq (u16) = packet_id[15:5] (11 bits) | frag_idx[4:0] (5 bits)
 *   frag_idx 0  descriptor : payload = [payload_len, frag_total, codec_id, flags]
 *   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
 *   frag_total = ceil(payload_len/4)  (<= 31  =>  payload_len <= 124 bytes / 20 ms block)
 *   timestamp_us identical across a packet's frames; src/dst = node ids.
 */
#pragma once
#include "demod_frame.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ── L2 constants ──────────────────────────────────────────────────────────── */
#define DCF_AUDIO_FRAG_BITS    5u
#define DCF_AUDIO_FRAG_MASK    0x1Fu                 /* low 5 bits of seq          */
#define DCF_AUDIO_MAX_FRAGS    31u                   /* max data fragments         */
#define DCF_AUDIO_MAX_PAYLOAD  (DCF_AUDIO_MAX_FRAGS * 4u)  /* 124 bytes / 20 ms    */
#define DCF_AUDIO_MAX_PACKETID 2047u                 /* 11-bit packet id           */
#define DCF_AUDIO_MAX_FRAMES   (1u + DCF_AUDIO_MAX_FRAGS)  /* 32 frames / packet   */

/* Codec ids (profiles in DCF_AUDIO_SPEC.md). */
#define DCF_CODEC_OPUS      0u
#define DCF_CODEC_PCM_DIAG  1u
#define DCF_CODEC_FAUST_PM  2u
/* id 3 reserved (declined low-bitrate speech codec) */

/* Descriptor flag bits. */
#define DCF_AUDIO_FLAG_END_TALKSPURT 0x01u
#define DCF_AUDIO_FLAG_PM_VOICE      0x02u

/* ── L2: packetize ─────────────────────────────────────────────────────────── */
/*
 * Serialise one codec block into DeModFrame CTRL frames.
 * `frames` must hold at least 1+ceil(payload_len/4) rows of DCF_FRAME_SIZE bytes.
 * Returns false on invalid args or insufficient room; on success *out_n is the
 * number of frames written (descriptor first, then data fragments in order).
 */
static inline bool dcf_audio_packetize(uint8_t codec_id, const uint8_t *payload,
                                       size_t payload_len, uint16_t packet_id,
                                       uint32_t ts_us, uint16_t src, uint16_t dst,
                                       uint8_t flags,
                                       uint8_t frames[][DCF_FRAME_SIZE],
                                       size_t max_frames, size_t *out_n) {
    if (payload_len > DCF_AUDIO_MAX_PAYLOAD) return false;
    if (packet_id > DCF_AUDIO_MAX_PACKETID)  return false;

    uint8_t frag_total = (uint8_t)((payload_len + 3u) / 4u);
    size_t  need       = (size_t)frag_total + 1u;
    if (need > max_frames) return false;

    dcf_frame_t f;
    f.version = 1u;
    f.type    = DCF_TYPE_CTRL;
    f.src_id  = src;
    f.dst_id  = dst;
    f.timestamp_us = ts_us;

    /* frag_idx 0 — descriptor */
    f.seq = (uint16_t)((uint16_t)packet_id << DCF_AUDIO_FRAG_BITS);
    f.payload[0] = (uint8_t)payload_len;
    f.payload[1] = frag_total;
    f.payload[2] = codec_id;
    f.payload[3] = flags;
    dcf_frame_encode(&f, frames[0]);

    /* frag_idx 1..frag_total — data, last chunk zero-padded */
    for (uint8_t k = 1; k <= frag_total; k++) {
        size_t off = (size_t)(k - 1) * 4u;
        uint8_t chunk[4] = {0, 0, 0, 0};
        size_t n = payload_len - off;
        if (n > 4u) n = 4u;
        memcpy(chunk, payload + off, n);
        f.seq = (uint16_t)(((uint16_t)packet_id << DCF_AUDIO_FRAG_BITS) | k);
        memcpy(f.payload, chunk, 4);
        dcf_frame_encode(&f, frames[k]);
    }

    *out_n = need;
    return true;
}

/* ── L2: reassembler ───────────────────────────────────────────────────────── */
#ifndef DCF_AUDIO_REASM_SLOTS
#define DCF_AUDIO_REASM_SLOTS 16   /* concurrent in-flight packets                */
#endif

typedef struct {
    uint16_t packet_id;
    uint32_t ts_us;
    uint8_t  codec_id;
    uint8_t  flags;
    uint8_t  payload_len;
    uint8_t  payload[DCF_AUDIO_MAX_PAYLOAD];
} dcf_audio_packet_t;

typedef struct {
    bool     in_use;
    bool     have_desc;
    uint16_t packet_id;
    uint32_t ts_us;
    uint8_t  codec_id;
    uint8_t  flags;
    uint8_t  payload_len;
    uint8_t  frag_total;
    uint32_t frags_present;                 /* bit k = data frag k present (1..31) */
    uint8_t  data[DCF_AUDIO_MAX_PAYLOAD];
} dcf_audio_slot_t;

typedef struct {
    dcf_audio_slot_t slots[DCF_AUDIO_REASM_SLOTS];
} dcf_audio_reasm_t;

typedef enum {
    DCF_REASM_NONE    = 0,   /* frame accepted, packet not yet complete            */
    DCF_REASM_PACKET  = 1,   /* a complete packet is in *out_pkt                    */
    DCF_REASM_IGNORED = 2,   /* not a CTRL audio frame, a duplicate, or no slot     */
} dcf_reasm_status_t;

static inline void dcf_audio_reasm_init(dcf_audio_reasm_t *r) {
    memset(r, 0, sizeof(*r));
}

static inline uint32_t dcf__need_mask(uint8_t frag_total) {
    /* bits 1..frag_total set; frag_total<=31 so 1u<<frag_total is well-defined */
    return frag_total ? ((((uint32_t)1u << frag_total) - 1u) << 1) : 0u;
}

static inline dcf_audio_slot_t *dcf__slot_for(dcf_audio_reasm_t *r, uint16_t pid) {
    dcf_audio_slot_t *free_slot = NULL;
    for (size_t i = 0; i < DCF_AUDIO_REASM_SLOTS; i++) {
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

static inline dcf_reasm_status_t dcf__slot_emit(dcf_audio_reasm_t *r,
                                                dcf_audio_slot_t *s,
                                                dcf_audio_packet_t *out) {
    if (!s->have_desc) return DCF_REASM_NONE;
    uint32_t need = dcf__need_mask(s->frag_total);
    if ((s->frags_present & need) != need) return DCF_REASM_NONE;
    out->packet_id   = s->packet_id;
    out->ts_us       = s->ts_us;
    out->codec_id    = s->codec_id;
    out->flags       = s->flags;
    out->payload_len = s->payload_len;
    memcpy(out->payload, s->data, s->payload_len);
    memset(s, 0, sizeof(*s));   /* frees the slot */
    (void)r;
    return DCF_REASM_PACKET;
}

/*
 * Push one 17-byte frame.  On DCF_REASM_PACKET, *out_pkt holds the assembled packet.
 * Duplicates and non-audio frames are ignored.  Mirrors the Python reference.
 */
static inline dcf_reasm_status_t dcf_audio_reasm_push(dcf_audio_reasm_t *r,
                                                      const uint8_t frame[DCF_FRAME_SIZE],
                                                      dcf_audio_packet_t *out_pkt) {
    dcf_frame_t d;
    if (!dcf_frame_decode(frame, &d))      return DCF_REASM_IGNORED;
    if (d.type != DCF_TYPE_CTRL)           return DCF_REASM_IGNORED;

    uint16_t pid      = (uint16_t)(d.seq >> DCF_AUDIO_FRAG_BITS);
    uint8_t  frag_idx = (uint8_t)(d.seq & DCF_AUDIO_FRAG_MASK);

    dcf_audio_slot_t *s = dcf__slot_for(r, pid);
    if (!s) return DCF_REASM_IGNORED;       /* table full */

    s->ts_us = d.timestamp_us;
    if (frag_idx == 0) {
        if (!s->have_desc) {
            s->have_desc   = true;
            s->payload_len = d.payload[0];
            s->frag_total  = d.payload[1];
            s->codec_id    = d.payload[2];
            s->flags       = d.payload[3];
        }
    } else if (frag_idx <= DCF_AUDIO_MAX_FRAGS) {
        uint32_t bit = (uint32_t)1u << frag_idx;
        if (!(s->frags_present & bit)) {
            s->frags_present |= bit;
            memcpy(s->data + (size_t)(frag_idx - 1) * 4u, d.payload, 4);
        }
    }
    return dcf__slot_emit(r, s, out_pkt);
}

/*
 * Report every still-incomplete packet as lost (ascending packet_id) and clear the
 * reassembler.  Returns the count; up to `max` ids are written to `lost`.
 */
static inline size_t dcf_audio_reasm_finalize(dcf_audio_reasm_t *r,
                                              uint16_t *lost, size_t max) {
    size_t n = 0;
    for (size_t i = 0; i < DCF_AUDIO_REASM_SLOTS; i++)
        if (r->slots[i].in_use && n < max) lost[n++] = r->slots[i].packet_id;
    /* insertion sort ascending (n is small) */
    for (size_t i = 1; i < n; i++) {
        uint16_t v = lost[i]; size_t j = i;
        while (j > 0 && lost[j - 1] > v) { lost[j] = lost[j - 1]; j--; }
        lost[j] = v;
    }
    dcf_audio_reasm_init(r);
    return n;
}

/* ── L1: PCM-diagnostic codec (id 1) — 6 kHz, 8-bit linear, mono, 120 B/block ── */
#define DCF_PCM_DIAG_RATE   6000u
#define DCF_PCM_DIAG_BLOCK  120u     /* samples == bytes per 20 ms block            */

/* float [-1,1] -> unsigned 8-bit (mid 128). decode∘encode is byte-lossless. */
static inline void dcf_pcm_diag_encode(const float *s, uint16_t n, uint8_t *out) {
    for (uint16_t i = 0; i < n; i++) {
        long v = (long)(s[i] * 128.0f + (s[i] >= 0 ? 0.5f : -0.5f)) + 128;
        out[i] = (uint8_t)(v < 0 ? 0 : v > 255 ? 255 : v);
    }
}
static inline void dcf_pcm_diag_decode(const uint8_t *in, uint16_t n, float *out) {
    for (uint16_t i = 0; i < n; i++) out[i] = ((float)in[i] - 128.0f) / 128.0f;
}

/* ── L1: PM (Faust phase-mod, id 2) — 8-byte parameter layout (certified) ────── */
typedef struct {
    uint16_t f0;          /* quantised fundamental (cents-mapped)                  */
    uint8_t  amp;
    uint8_t  mod_index;
    uint8_t  mod_ratio;   /* carrier:modulator, u8 fixed-point                     */
    uint8_t  bright;      /* brightness / feedback                                 */
    uint8_t  env;         /* envelope slope                                        */
    uint8_t  flags;
} dcf_pm_params_t;

static inline void dcf_pm_pack(const dcf_pm_params_t *p, uint8_t out[8]) {
    out[0] = (uint8_t)(p->f0 >> 8);
    out[1] = (uint8_t)(p->f0 & 0xFF);
    out[2] = p->amp;
    out[3] = p->mod_index;
    out[4] = p->mod_ratio;
    out[5] = p->bright;
    out[6] = p->env;
    out[7] = p->flags;
}
static inline bool dcf_pm_unpack(const uint8_t in[8], dcf_pm_params_t *p) {
    p->f0        = (uint16_t)(((uint16_t)in[0] << 8) | in[1]);
    p->amp       = in[2];
    p->mod_index = in[3];
    p->mod_ratio = in[4];
    p->bright    = in[5];
    p->env       = in[6];
    p->flags     = in[7];
    return true;
}

/* ── Codec registry (vtable) ───────────────────────────────────────────────── */
typedef struct {
    uint8_t  codec_id;
    uint32_t sample_rate;
    uint8_t  channels;
    uint16_t block_samples;
    uint16_t max_payload;
    /* encode: pcm[block_samples] -> out bytes (*out_len). returns 0 on success.  */
    int (*encode)(const float *pcm, uint16_t n, uint8_t *out, uint16_t *out_len);
    /* decode: in bytes -> pcm (*n samples). returns 0 on success.                */
    int (*decode)(const uint8_t *in, uint16_t in_len, float *pcm, uint16_t *n);
    /* plc: synthesise one concealment block into pcm (*n). returns 0 on success. */
    int (*plc)(float *pcm, uint16_t *n);
} dcf_codec_vtable_t;

static int dcf__pcm_diag_encode(const float *pcm, uint16_t n, uint8_t *out, uint16_t *out_len) {
    if (n != DCF_PCM_DIAG_BLOCK) return -1;
    dcf_pcm_diag_encode(pcm, n, out);
    *out_len = n;
    return 0;
}
static int dcf__pcm_diag_decode(const uint8_t *in, uint16_t in_len, float *pcm, uint16_t *n) {
    if (in_len > DCF_PCM_DIAG_BLOCK) return -1;
    dcf_pcm_diag_decode(in, in_len, pcm);
    *n = in_len;
    return 0;
}
static int dcf__pcm_diag_plc(float *pcm, uint16_t *n) {
    for (uint16_t i = 0; i < DCF_PCM_DIAG_BLOCK; i++) pcm[i] = 0.0f;  /* silence */
    *n = DCF_PCM_DIAG_BLOCK;
    return 0;
}
static const dcf_codec_vtable_t dcf__codec_pcm_diag = {
    DCF_CODEC_PCM_DIAG, DCF_PCM_DIAG_RATE, 1, DCF_PCM_DIAG_BLOCK, DCF_PCM_DIAG_BLOCK,
    dcf__pcm_diag_encode, dcf__pcm_diag_decode, dcf__pcm_diag_plc,
};

/* dcf_codec_get is defined at the end of this header, after every codec vtable. */

/* ── L3: jitter buffer (optional) ──────────────────────────────────────────── */
#ifdef DCF_AUDIO_RT
#ifndef DCF_JITTERBUF_SLOTS
#define DCF_JITTERBUF_SLOTS 8
#endif
typedef struct {
    dcf_audio_packet_t pkts[DCF_JITTERBUF_SLOTS];
    bool     used[DCF_JITTERBUF_SLOTS];
    uint16_t next_id;        /* next packet_id expected at playout                 */
    uint8_t  target_depth;   /* packets buffered before playout starts             */
    bool     primed;
} dcf_jitterbuf_t;

static inline void dcf_jitterbuf_init(dcf_jitterbuf_t *j, uint8_t target_depth) {
    memset(j, 0, sizeof(*j));
    j->target_depth = target_depth ? target_depth : 2;
}
static inline size_t dcf__jb_count(const dcf_jitterbuf_t *j) {
    size_t c = 0;
    for (size_t i = 0; i < DCF_JITTERBUF_SLOTS; i++) c += j->used[i] ? 1u : 0u;
    return c;
}
/* Insert a reassembled packet (drops late packets older than next_id, and dups). */
static inline void dcf_jitterbuf_push(dcf_jitterbuf_t *j, const dcf_audio_packet_t *p) {
    if (j->primed && (uint16_t)(p->packet_id - j->next_id) > (DCF_AUDIO_MAX_PACKETID / 2))
        return;  /* stale: id is "behind" next_id in modular order */
    for (size_t i = 0; i < DCF_JITTERBUF_SLOTS; i++)
        if (j->used[i] && j->pkts[i].packet_id == p->packet_id) return;  /* dup */
    for (size_t i = 0; i < DCF_JITTERBUF_SLOTS; i++)
        if (!j->used[i]) { j->pkts[i] = *p; j->used[i] = true; return; }
    /* full: drop newest-incoming (back-pressure) */
}
/*
 * Pop the packet due for playout. Returns true and fills *out when the next_id
 * packet is available; if a gap is detected at/after priming, returns true with
 * *lost=true (caller runs codec PLC) and advances. Returns false while filling.
 */
static inline bool dcf_jitterbuf_pop(dcf_jitterbuf_t *j, dcf_audio_packet_t *out, bool *lost) {
    if (!j->primed) {
        if (dcf__jb_count(j) < j->target_depth) return false;
        /* prime to the lowest buffered id */
        uint16_t lo = 0; bool any = false;
        for (size_t i = 0; i < DCF_JITTERBUF_SLOTS; i++)
            if (j->used[i] && (!any || j->pkts[i].packet_id < lo)) { lo = j->pkts[i].packet_id; any = true; }
        if (!any) return false;
        j->next_id = lo;
        j->primed  = true;
    }
    for (size_t i = 0; i < DCF_JITTERBUF_SLOTS; i++) {
        if (j->used[i] && j->pkts[i].packet_id == j->next_id) {
            *out = j->pkts[i];
            j->used[i] = false;
            *lost = false;
            j->next_id = (uint16_t)((j->next_id + 1u) & DCF_AUDIO_MAX_PACKETID);
            return true;
        }
    }
    /* gap: conceal one packet if anything newer is waiting, else keep filling */
    if (dcf__jb_count(j) > 0) {
        *lost = true;
        j->next_id = (uint16_t)((j->next_id + 1u) & DCF_AUDIO_MAX_PACKETID);
        return true;
    }
    return false;
}
#endif /* DCF_AUDIO_RT */

/* ── L1: Opus codec (id 0) ─────────────────────────────────────────────────── */
#ifdef DCF_AUDIO_OPUS
#include <opus/opus.h>
/* 48 kHz mono, 20 ms = 960 samples. Encoder/decoder are process-global singletons
 * here for brevity; a real deployment threads one pair per peer. */
#define DCF_OPUS_RATE   48000
#define DCF_OPUS_BLOCK  960
static OpusEncoder *dcf__opus_enc(void) {
    static OpusEncoder *e = NULL; int err;
    if (!e) { e = opus_encoder_create(DCF_OPUS_RATE, 1, OPUS_APPLICATION_AUDIO, &err);
              if (e) opus_encoder_ctl(e, OPUS_SET_BITRATE(24000)); }
    return e;
}
static OpusDecoder *dcf__opus_dec(void) {
    static OpusDecoder *d = NULL; int err;
    if (!d) d = opus_decoder_create(DCF_OPUS_RATE, 1, &err);
    return d;
}
static int dcf__opus_encode(const float *pcm, uint16_t n, uint8_t *out, uint16_t *out_len) {
    if (n != DCF_OPUS_BLOCK) return -1;
    OpusEncoder *e = dcf__opus_enc(); if (!e) return -1;
    int r = opus_encode_float(e, pcm, n, out, DCF_AUDIO_MAX_PAYLOAD);
    if (r < 0) return r; *out_len = (uint16_t)r; return 0;
}
static int dcf__opus_decode(const uint8_t *in, uint16_t in_len, float *pcm, uint16_t *n) {
    OpusDecoder *d = dcf__opus_dec(); if (!d) return -1;
    int r = opus_decode_float(d, in, in_len, pcm, DCF_OPUS_BLOCK, 0);
    if (r < 0) return r; *n = (uint16_t)r; return 0;
}
static int dcf__opus_plc(float *pcm, uint16_t *n) {
    OpusDecoder *d = dcf__opus_dec(); if (!d) return -1;
    int r = opus_decode_float(d, NULL, 0, pcm, DCF_OPUS_BLOCK, 0);
    if (r < 0) return r; *n = (uint16_t)r; return 0;
}
static const dcf_codec_vtable_t dcf__codec_opus = {
    DCF_CODEC_OPUS, DCF_OPUS_RATE, 1, DCF_OPUS_BLOCK, DCF_AUDIO_MAX_PAYLOAD,
    dcf__opus_encode, dcf__opus_decode, dcf__opus_plc,
};
#endif /* DCF_AUDIO_OPUS */

/* ── L1: Faust phase-mod synthesis (id 2) ──────────────────────────────────── */
#ifdef DCF_AUDIO_PM
/* Synthesis lives in faust/dcf_pm_codec.gen.c, compiled and linked SEPARATELY
 * (not #included here, to avoid multiple-definition across TUs). The host fills
 * the params via its pitch/RMS/centroid analysis and packetizes the 8 bytes; the
 * vtable decode resynthesises a block from those 8 bytes. Link gen.c + -lm. */
extern void dcf_pm_synth_block_ffi(const uint8_t params8[8], float *out, int n);
#define DCF_PM_RATE   48000
#define DCF_PM_BLOCK  960
static int dcf__pm_decode(const uint8_t *in, uint16_t in_len, float *pcm, uint16_t *n) {
    if (in_len != 8) return -1;
    dcf_pm_synth_block_ffi(in, pcm, DCF_PM_BLOCK);
    *n = DCF_PM_BLOCK;
    return 0;
}
static int dcf__pm_plc(float *pcm, uint16_t *n) {
    for (uint16_t i = 0; i < DCF_PM_BLOCK; i++) pcm[i] = 0.0f;
    *n = DCF_PM_BLOCK;
    return 0;
}
static const dcf_codec_vtable_t dcf__codec_faust_pm = {
    DCF_CODEC_FAUST_PM, DCF_PM_RATE, 1, DCF_PM_BLOCK, 8,
    NULL, dcf__pm_decode, dcf__pm_plc,
};
#endif /* DCF_AUDIO_PM */

/* ── Codec registry lookup (after all vtables are defined) ─────────────────── */
/* Returns the codec vtable for `codec_id`, or NULL if not built in this config. */
static inline const dcf_codec_vtable_t *dcf_codec_get(uint8_t codec_id) {
    switch (codec_id) {
        case DCF_CODEC_PCM_DIAG: return &dcf__codec_pcm_diag;
#ifdef DCF_AUDIO_OPUS
        case DCF_CODEC_OPUS:     return &dcf__codec_opus;
#endif
#ifdef DCF_AUDIO_PM
        case DCF_CODEC_FAUST_PM: return &dcf__codec_faust_pm;
#endif
        default: return NULL;
    }
}
