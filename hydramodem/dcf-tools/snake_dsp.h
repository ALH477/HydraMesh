// SPDX-License-Identifier: LGPL-3.0-only
/*
 * snake_dsp.h — DCF-Snake mixer/spoke DSP primitives, DeMoD LLC | LGPL-3.0.
 *
 * The genuinely new (non-byte-certified, float) DSP the synchronization core needs, gathered
 * as pure header-only components so they are unit-testable off-line (test_snake_dsp.c):
 *   - snake_servo   : a PI clock servo (spoke locks capture to the BEACON grandmaster).
 *   - snake_jb      : a live jitter buffer keyed on a monotonic index (QSS hop_index / block_seq).
 *   - snake_asrc    : a fractional (cubic-Hermite) async sample-rate converter — the mixer's
 *                     per-source drift-correction fallback.
 *   - snake_plc     : host-side PCM packet-loss concealment (energy-decaying last-block repeat).
 *   - snake_cuemix  : the mixer's per-node aux-send cue-mix matrix.
 *
 * These are approximate/float (like Opus/PM synthesis) — NOT part of any byte certificate.
 * The ASRC is a cubic-Hermite v1; a higher-order polyphase/Farrow kernel is a documented
 * refinement (see DCF_SNAKE_SPEC.md). All state is caller-owned; no malloc, no libm beyond
 * fabs/floor (kept out where possible for embeddability).
 */
#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifndef SNAKE_MAX_BLOCK
#define SNAKE_MAX_BLOCK 12288u   /* 256 ms @ 48 kHz (the relaxed-mode hop) */
#endif

/* ── PI clock servo ─────────────────────────────────────────────────────────────────────
 * Drives a rate trim (ppm) from the phase error between the local sample counter and the
 * grandmaster gm_sample_count (corrected by one-way delay).  err > 0 => we are behind. */
typedef struct {
    double kp;        /* proportional gain */
    double ki;        /* integral gain     */
    double integ;     /* integrator state  */
    double ppm_clamp; /* |ppm| limit       */
} snake_servo_t;

static inline void snake_servo_init(snake_servo_t *s, double kp, double ki, double ppm_clamp) {
    s->kp = kp; s->ki = ki; s->integ = 0.0; s->ppm_clamp = ppm_clamp;
}

/* Feed the current phase error (in samples); returns the rate trim in ppm (clamped). */
static inline double snake_servo_update(snake_servo_t *s, double err_samples) {
    s->integ += err_samples;
    double ppm = s->kp * err_samples + s->ki * s->integ;
    if (ppm >  s->ppm_clamp) ppm =  s->ppm_clamp;
    if (ppm < -s->ppm_clamp) ppm = -s->ppm_clamp;
    return ppm;
}

/* ── jitter buffer (keyed on a monotonic absolute index) ────────────────────────────────
 * Holds decoded fixed-length PCM blocks in a small ring; a play cursor advances one block per
 * pop, emitting the block if present, else signalling a gap (loss → PLC) once primed. */
#ifndef SNAKE_JB_SLOTS
#define SNAKE_JB_SLOTS 16u
#endif

typedef enum { SNAKE_JB_UNDERRUN = 0, SNAKE_JB_BLOCK = 1, SNAKE_JB_GAP = 2 } snake_jb_status_t;

typedef struct {
    uint32_t block_len;                 /* samples per block (<= SNAKE_MAX_BLOCK)     */
    uint32_t target_depth;              /* blocks buffered before playout starts       */
    bool     primed;
    uint64_t play;                      /* next absolute index to emit                 */
    uint64_t hi;                        /* highest absolute index pushed + 1           */
    bool     present[SNAKE_JB_SLOTS];
    uint64_t idx[SNAKE_JB_SLOTS];       /* absolute index stored in each slot          */
    float    slot[SNAKE_JB_SLOTS][SNAKE_MAX_BLOCK];
} snake_jb_t;

static inline void snake_jb_init(snake_jb_t *jb, uint32_t block_len, uint32_t target_depth) {
    memset(jb, 0, sizeof *jb);
    jb->block_len = block_len > SNAKE_MAX_BLOCK ? SNAKE_MAX_BLOCK : block_len;
    jb->target_depth = target_depth ? target_depth : 2u;
}

/* Insert a decoded block at absolute index `abs`.  Late (already played) or duplicate blocks
 * are ignored.  Returns true if stored. */
static inline bool snake_jb_push(snake_jb_t *jb, uint64_t abs, const float *pcm, uint32_t n) {
    if (jb->primed && abs < jb->play) return false;         /* too late */
    uint32_t s = (uint32_t)(abs % SNAKE_JB_SLOTS);
    if (jb->present[s] && jb->idx[s] == abs) return false;  /* duplicate */
    jb->present[s] = true;
    jb->idx[s] = abs;
    uint32_t c = n < jb->block_len ? n : jb->block_len;
    memcpy(jb->slot[s], pcm, c * sizeof(float));
    if (c < jb->block_len) memset(jb->slot[s] + c, 0, (jb->block_len - c) * sizeof(float));
    if (abs + 1 > jb->hi) jb->hi = abs + 1;
    return true;
}

/* Pop the next block into `out` (block_len samples).  Returns BLOCK (present), GAP (primed but
 * this index is missing → conceal), or UNDERRUN (not yet primed / nothing buffered). */
static inline snake_jb_status_t snake_jb_pop(snake_jb_t *jb, float *out) {
    if (!jb->primed) {
        uint32_t depth = (jb->hi > jb->play) ? (uint32_t)(jb->hi - jb->play) : 0u;
        if (depth < jb->target_depth) return SNAKE_JB_UNDERRUN;
        jb->primed = true;
    }
    uint32_t s = (uint32_t)(jb->play % SNAKE_JB_SLOTS);
    if (jb->present[s] && jb->idx[s] == jb->play) {
        memcpy(out, jb->slot[s], jb->block_len * sizeof(float));
        jb->present[s] = false;
        jb->play++;
        return SNAKE_JB_BLOCK;
    }
    if (jb->play < jb->hi) {          /* a real gap: this index will never arrive in time */
        jb->play++;
        return SNAKE_JB_GAP;
    }
    return SNAKE_JB_UNDERRUN;         /* ran off the end of what we have */
}

/* ── async sample-rate conversion (cubic-Hermite fractional resampler) ───────────────────
 * Resamples an input stream by `ratio` = out_rate/in_rate (drift correction: ratio slightly
 * off 1.0).  Keeps 3 history samples across calls so blocks stitch seamlessly. */
typedef struct {
    double ratio;      /* output samples per input sample                     */
    double phase;      /* fractional read position within the input, [0,1)    */
    float  hist[3];    /* x[-3], x[-2], x[-1] carried across calls            */
    bool   seeded;
} snake_asrc_t;

static inline void snake_asrc_init(snake_asrc_t *a, double ratio) {
    memset(a, 0, sizeof *a);
    a->ratio = ratio > 0.0 ? ratio : 1.0;
}

static inline void snake_asrc_set_ratio(snake_asrc_t *a, double ratio) {
    if (ratio > 0.0) a->ratio = ratio;
}

static inline float snake__hermite(float ym1, float y0, float y1, float y2, float t) {
    /* Catmull-Rom cubic Hermite interpolation at fractional position t in [0,1). */
    float c0 = y0;
    float c1 = 0.5f * (y1 - ym1);
    float c2 = ym1 - 2.5f * y0 + 2.0f * y1 - 0.5f * y2;
    float c3 = 0.5f * (y2 - ym1) + 1.5f * (y0 - y1);
    return ((c3 * t + c2) * t + c1) * t + c0;
}

/* Resample `in`[0..n_in) into `out` (capacity `out_cap`); returns the number of samples written.
 * Advances internal phase/history so the next call continues seamlessly. */
static inline size_t snake_asrc_process(snake_asrc_t *a, const float *in, size_t n_in,
                                        float *out, size_t out_cap) {
    if (!a->seeded) {                 /* seed history with the first sample (avoids a click) */
        a->hist[0] = a->hist[1] = a->hist[2] = n_in ? in[0] : 0.0f;
        a->seeded = true;
    }
    double step = 1.0 / a->ratio;     /* input samples consumed per output sample */
    size_t w = 0;
    double pos = a->phase;            /* read position relative to in[0] */
    while (w < out_cap) {
        double fi = pos;
        long i0 = (long)fi;           /* floor for pos >= 0 */
        if ((size_t)i0 >= n_in) break;
        float t = (float)(fi - (double)i0);
        /* gather the 4 taps, drawing x[<0] from carried history */
        float ym1 = (i0 - 1 >= 0) ? in[i0 - 1] : a->hist[2 + (i0 - 1)];
        float y0  = (i0     >= 0) ? in[i0]     : a->hist[2 + i0];
        float y1  = ((size_t)(i0 + 1) < n_in) ? in[i0 + 1] : in[n_in ? n_in - 1 : 0];
        float y2  = ((size_t)(i0 + 2) < n_in) ? in[i0 + 2] : in[n_in ? n_in - 1 : 0];
        out[w++] = snake__hermite(ym1, y0, y1, y2, t);
        pos += step;
    }
    /* carry phase (fractional remainder past the consumed input) and the last 3 samples */
    a->phase = pos - (double)n_in;
    if (a->phase < 0.0) a->phase = 0.0;
    if (n_in >= 3) {
        a->hist[0] = in[n_in - 3]; a->hist[1] = in[n_in - 2]; a->hist[2] = in[n_in - 1];
    } else if (n_in > 0) {
        for (size_t k = 0; k < n_in; k++) a->hist[3 - n_in + k] = in[k];
    }
    return w;
}

/* ── packet-loss concealment (energy-decaying last-block repeat) ─────────────────────────
 * QSS threads prediction across packets, so concealment is host-side PCM: repeat the last good
 * block scaled by a per-gap decay so a burst of losses fades rather than clicks. */
typedef struct {
    float    last[SNAKE_MAX_BLOCK];
    uint32_t block_len;
    float    gain;      /* current conceal gain (1.0 after a good block)      */
    float    decay;     /* multiplied per consecutive concealed block         */
    bool     have;
} snake_plc_t;

static inline void snake_plc_init(snake_plc_t *p, uint32_t block_len, float decay) {
    memset(p, 0, sizeof *p);
    p->block_len = block_len > SNAKE_MAX_BLOCK ? SNAKE_MAX_BLOCK : block_len;
    p->decay = (decay > 0.0f && decay <= 1.0f) ? decay : 0.5f;
    p->gain = 1.0f;
}

/* Record a good block (resets the conceal gain). */
static inline void snake_plc_good(snake_plc_t *p, const float *pcm, uint32_t n) {
    uint32_t c = n < p->block_len ? n : p->block_len;
    memcpy(p->last, pcm, c * sizeof(float));
    if (c < p->block_len) memset(p->last + c, 0, (p->block_len - c) * sizeof(float));
    p->have = true;
    p->gain = 1.0f;
}

/* Produce one concealed block into `out` (decays each consecutive call; silence if no history). */
static inline void snake_plc_conceal(snake_plc_t *p, float *out) {
    if (!p->have) { memset(out, 0, p->block_len * sizeof(float)); return; }
    p->gain *= p->decay;
    for (uint32_t i = 0; i < p->block_len; i++) out[i] = p->last[i] * p->gain;
}

/* ── cue-mix matrix (per-node aux sends) ────────────────────────────────────────────────
 * N source inputs → M per-node cue buses.  gain[m*N + n] weights source n into node m's mix.
 * All inputs are already sample-aligned by the shared clock, so a cue bus is a weighted sum. */
#ifndef SNAKE_CUE_MAX_NODES
#define SNAKE_CUE_MAX_NODES 32u
#endif

typedef struct {
    uint32_t n_src;
    uint32_t n_node;
    float    gain[SNAKE_CUE_MAX_NODES * SNAKE_CUE_MAX_NODES];
} snake_cuemix_t;

static inline void snake_cuemix_init(snake_cuemix_t *c, uint32_t n_src, uint32_t n_node) {
    memset(c, 0, sizeof *c);
    c->n_src  = n_src  < SNAKE_CUE_MAX_NODES ? n_src  : SNAKE_CUE_MAX_NODES;
    c->n_node = n_node < SNAKE_CUE_MAX_NODES ? n_node : SNAKE_CUE_MAX_NODES;
}

static inline void snake_cuemix_set(snake_cuemix_t *c, uint32_t node, uint32_t src, float g) {
    if (node < c->n_node && src < c->n_src) c->gain[node * c->n_src + src] = g;
}

/* Mix `n_src` input blocks (each `len` samples) into node `node`'s cue bus `out`, with a soft
 * tanh-free limiter (hard clamp to +/-1) to protect the monitor feed. */
static inline void snake_cuemix_bus(const snake_cuemix_t *c, uint32_t node,
                                    const float *const *inputs, uint32_t len, float *out) {
    for (uint32_t i = 0; i < len; i++) {
        float acc = 0.0f;
        for (uint32_t s = 0; s < c->n_src; s++)
            acc += c->gain[node * c->n_src + s] * inputs[s][i];
        if (acc >  1.0f) acc =  1.0f;
        if (acc < -1.0f) acc = -1.0f;
        out[i] = acc;
    }
}
