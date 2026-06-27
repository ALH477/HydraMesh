/* hydra_conv.c -- K=7 rate-1/2 convolutional encoder + soft Viterbi decoder. */
#include "hydra_conv.h"
#include <stdlib.h>

#define G0 0x79u   /* 0171 octal = 1111001b, bit6 = newest tap */
#define G1 0x5Bu   /* 0133 octal = 1011011b                    */
#define MASK7 0x7Fu
#define NSTATE HYDRA_CONV_STATES

static inline int parity7(unsigned v)
{
    v &= MASK7;
    v ^= v >> 4;
    v ^= v >> 2;
    v ^= v >> 1;
    return (int)(v & 1u);
}

/* reg = (b<<6)|state ; outputs from full 7-bit reg ; next = reg>>1 */
size_t hydra_conv_encode(const uint8_t *in_bits, size_t n_msg, uint8_t *out_bits)
{
    unsigned state = 0;
    size_t i, w = 0;
    size_t total = n_msg + HYDRA_CONV_MEM;          /* msg + tail */
    for (i = 0; i < total; ++i) {
        unsigned b = (i < n_msg) ? (in_bits[i] & 1u) : 0u;   /* tail = 0 */
        unsigned reg = (b << 6) | state;
        out_bits[w++] = (uint8_t)parity7(reg & G0);
        out_bits[w++] = (uint8_t)parity7(reg & G1);
        state = reg >> 1;
    }
    return w;                                        /* = 2*(n_msg+MEM) */
}

/* Precompute per (state,bit): outputs o0,o1 and next state. */
typedef struct { uint8_t o0, o1, next; } trellis_edge;

static void build_trellis(trellis_edge edge[NSTATE][2])
{
    unsigned s, b;
    for (s = 0; s < NSTATE; ++s)
        for (b = 0; b < 2; ++b) {
            unsigned reg = (b << 6) | s;
            edge[s][b].o0   = (uint8_t)parity7(reg & G0);
            edge[s][b].o1   = (uint8_t)parity7(reg & G1);
            edge[s][b].next = (uint8_t)(reg >> 1);
        }
}

size_t hydra_conv_decode_soft(const float *soft, size_t n_coded,
                              size_t n_msg, uint8_t *out_bits)
{
    static const float NEG = -1e30f;
    size_t L = n_msg + HYDRA_CONV_MEM;               /* trellis steps */
    trellis_edge edge[NSTATE][2];
    float  *pm, *npm;
    uint8_t *tb_prev, *tb_bit;                       /* [L][NSTATE] */
    size_t  t, s, i;
    int     ok;

    if (n_coded != 2u * L || n_msg == 0) return 0;

    build_trellis(edge);

    pm  = (float *)malloc(NSTATE * sizeof *pm);
    npm = (float *)malloc(NSTATE * sizeof *npm);
    tb_prev = (uint8_t *)malloc(L * NSTATE);
    tb_bit  = (uint8_t *)malloc(L * NSTATE);
    if (!pm || !npm || !tb_prev || !tb_bit) {
        free(pm); free(npm); free(tb_prev); free(tb_bit); return 0;
    }

    for (s = 0; s < NSTATE; ++s) pm[s] = (s == 0) ? 0.0f : NEG;

    for (t = 0; t < L; ++t) {
        float s0 = soft[2*t], s1 = soft[2*t + 1];
        for (s = 0; s < NSTATE; ++s) npm[s] = NEG;
        for (s = 0; s < NSTATE; ++s) {
            float base = pm[s];
            int b;
            if (base <= NEG) continue;
            for (b = 0; b < 2; ++b) {
                const trellis_edge *e = &edge[s][b];
                /* correlation branch metric: expected bit 1 -> +soft, 0 -> -soft */
                float bm = (e->o0 ? s0 : -s0) + (e->o1 ? s1 : -s1);
                float cand = base + bm;
                unsigned ns = e->next;
                if (cand > npm[ns]) {
                    npm[ns] = cand;
                    tb_prev[t * NSTATE + ns] = (uint8_t)s;
                    tb_bit [t * NSTATE + ns] = (uint8_t)b;
                }
            }
        }
        { float *tmp = pm; pm = npm; npm = tmp; }
    }

    /* terminate in state 0 (tail flush guarantees it) and trace back */
    {
        unsigned s_cur = 0;
        /* collect bits in reverse */
        uint8_t *bits = (uint8_t *)malloc(L);
        if (!bits) { ok = 0; }
        else {
            for (t = L; t-- > 0; ) {
                bits[t]   = tb_bit [t * NSTATE + s_cur];
                s_cur     = tb_prev[t * NSTATE + s_cur];
            }
            for (i = 0; i < n_msg; ++i) out_bits[i] = bits[i] & 1u;  /* drop tail */
            free(bits);
            ok = 1;
        }
    }

    free(pm); free(npm); free(tb_prev); free(tb_bit);
    return ok ? n_msg : 0;
}
