/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * demod_fec.h — DCF forward-error-correction adapter (Reed-Solomon over GF(2^8)).
 * Byte-identical to python/MCP/feclab_core.py and codec/src/fec.rs; pinned by
 * Documentation/fec_vectors.json + codec/fec_vectors.gen.h.
 *
 * FEC is an adapter *around* the 17-byte DeModFrame: a systematic RS code appends
 * 2t parity bytes so a lossy medium (RF/SDR, acoustic) can CORRECT a frame, not just
 * detect damage. The frame and its 246-vector certificate are untouched. This is the
 * CERTIFIED half; the IQ waveform that carries the bytes is analog/loopback-tested.
 *
 * Field: GF(2^8), prim 0x11D, generator alpha=2, first consecutive root fcr=0.
 * Systematic: codeword = message ++ parity. Default 2t=16 -> corrects t=8 byte-errors.
 */
#ifndef DCF_DEMOD_FEC_H
#define DCF_DEMOD_FEC_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define DCF_FEC_PRIM    0x11D
#define DCF_FEC_GEN     2
#define DCF_FEC_FCR     0
#define DCF_FEC_NPARITY 16
#define DCF_FEC_MAXN    255

/* ── GF(256) (lazily built per translation unit) ──────────────────────────────*/
static uint8_t dcf__gf_exp[512];
static uint8_t dcf__gf_log[256];
static int     dcf__gf_ready = 0;

static inline void dcf__gf_init(void) {
    if (dcf__gf_ready) return;
    int x = 1;
    for (int i = 0; i < 255; i++) {
        dcf__gf_exp[i] = (uint8_t)x;
        dcf__gf_log[x] = (uint8_t)i;
        x <<= 1;
        if (x & 0x100) x ^= DCF_FEC_PRIM;
    }
    for (int i = 255; i < 512; i++) dcf__gf_exp[i] = dcf__gf_exp[i - 255];
    dcf__gf_ready = 1;
}
static inline uint8_t dcf__gf_mul(uint8_t a, uint8_t b) {
    return (a == 0 || b == 0) ? 0 : dcf__gf_exp[dcf__gf_log[a] + dcf__gf_log[b]];
}
static inline uint8_t dcf__gf_div(uint8_t a, uint8_t b) {
    return (a == 0) ? 0 : dcf__gf_exp[(dcf__gf_log[a] + 255 - dcf__gf_log[b]) % 255];
}
static inline uint8_t dcf__gf_pow(uint8_t a, int p) {
    int e = (((int)dcf__gf_log[a] * p) % 255 + 255) % 255;
    return dcf__gf_exp[e];
}
static inline uint8_t dcf__gf_inv(uint8_t a) { return dcf__gf_exp[255 - dcf__gf_log[a]]; }

/* ── GF polynomials (high-order coefficient first); lengths tracked explicitly ──*/
static inline size_t dcf__pmul(const uint8_t *p, size_t pl, const uint8_t *q, size_t ql,
                               uint8_t *r) {
    size_t rl = pl + ql - 1;
    memset(r, 0, rl);
    for (size_t j = 0; j < ql; j++)
        for (size_t i = 0; i < pl; i++)
            r[i + j] ^= dcf__gf_mul(p[i], q[j]);
    return rl;
}
static inline uint8_t dcf__peval(const uint8_t *p, size_t pl, uint8_t x) {
    uint8_t y = p[0];
    for (size_t i = 1; i < pl; i++) y = (uint8_t)(dcf__gf_mul(y, x) ^ p[i]);
    return y;
}

/* ── encode (systematic) ──────────────────────────────────────────────────────*/
static inline void dcf__rs_genpoly(uint8_t nparity, uint8_t *g, size_t *glen) {
    g[0] = 1;
    size_t L = 1;
    uint8_t tmp[DCF_FEC_MAXN + 2];
    for (uint8_t i = 0; i < nparity; i++) {
        uint8_t factor[2] = { 1, dcf__gf_pow(DCF_FEC_GEN, DCF_FEC_FCR + i) };
        size_t nl = dcf__pmul(g, L, factor, 2, tmp);
        memcpy(g, tmp, nl);
        L = nl;
    }
    *glen = L; /* = nparity + 1 */
}

/* message (mlen bytes) -> code (mlen+nparity bytes). code holds message then parity. */
static inline void dcf_fec_encode(const uint8_t *msg, size_t mlen, uint8_t nparity,
                                  uint8_t *code) {
    dcf__gf_init();
    uint8_t g[DCF_FEC_MAXN + 2];
    size_t glen;
    dcf__rs_genpoly(nparity, g, &glen);
    uint8_t out[DCF_FEC_MAXN + DCF_FEC_NPARITY + 2];
    memcpy(out, msg, mlen);
    memset(out + mlen, 0, nparity);
    for (size_t i = 0; i < mlen; i++) {
        uint8_t coef = out[i];
        if (coef)
            for (size_t j = 1; j < glen; j++) out[i + j] ^= dcf__gf_mul(g[j], coef);
    }
    memcpy(code, msg, mlen);
    memcpy(code + mlen, out + mlen, nparity);
}

/* ── decode (Berlekamp–Massey -> Chien -> Forney) ─────────────────────────────*/
/* syndromes into synd[0..nparity], with a leading 0 (Forney convention). */
static inline int dcf__syndromes(const uint8_t *cw, size_t n, uint8_t nparity, uint8_t *synd) {
    int nonzero = 0;
    synd[0] = 0;
    for (uint8_t i = 0; i < nparity; i++) {
        synd[i + 1] = dcf__peval(cw, n, dcf__gf_pow(DCF_FEC_GEN, DCF_FEC_FCR + i));
        if (synd[i + 1]) nonzero = 1;
    }
    return nonzero;
}

/* error locator via Berlekamp–Massey (mirrors feclab_core._error_locator). synd has a
 * leading 0 at synd[0]; coeffs high-order first. Returns the trimmed length. */
static inline size_t dcf__err_locator(const uint8_t *synd, uint8_t nparity, uint8_t *err_loc) {
    uint8_t old_loc[DCF_FEC_MAXN + 2], a[DCF_FEC_MAXN + 2], b[DCF_FEC_MAXN + 2];
    size_t el = 1, ol = 1;
    err_loc[0] = 1;
    old_loc[0] = 1;
    for (uint8_t i = 0; i < nparity; i++) {
        uint8_t delta = synd[i + 1];                  /* synd_shift = 1 */
        for (size_t j = 1; j < el; j++)
            delta ^= dcf__gf_mul(err_loc[el - 1 - j], synd[i + 1 - j]);
        old_loc[ol++] = 0;                            /* old_loc *= x */
        if (delta != 0) {
            if (ol > el) {
                /* new_loc = scale(old_loc, delta); old_loc = scale(err_loc, 1/delta);
                 * err_loc = new_loc  (lengths swap) */
                for (size_t t = 0; t < ol; t++) a[t] = dcf__gf_mul(old_loc[t], delta);
                uint8_t dinv = dcf__gf_inv(delta);
                for (size_t t = 0; t < el; t++) old_loc[t] = dcf__gf_mul(err_loc[t], dinv);
                size_t new_ol = el;
                memcpy(err_loc, a, ol);
                el = ol;
                ol = new_ol;
            }
            /* err_loc = padd(err_loc, scale(old_loc, delta)) — low-order aligned */
            size_t L = (el > ol) ? el : ol;
            memset(b, 0, L);
            for (size_t t = 0; t < el; t++) b[L - el + t] = err_loc[t];
            for (size_t t = 0; t < ol; t++) b[L - ol + t] ^= dcf__gf_mul(old_loc[t], delta);
            memcpy(err_loc, b, L);
            el = L;
        }
    }
    size_t s = 0;
    while (s < el && err_loc[s] == 0) s++;
    memmove(err_loc, err_loc + s, el - s);
    return el - s;
}

/* Chien search: roots of err_loc (reversed) over positions 0..n-1. */
static inline int dcf__err_positions(const uint8_t *err_loc_rev, size_t el, size_t n,
                                     size_t *pos) {
    size_t errs = el - 1, cnt = 0;
    for (size_t i = 0; i < n; i++) {
        if (dcf__peval(err_loc_rev, el, dcf__gf_pow(DCF_FEC_GEN, (int)i)) == 0) {
            if (cnt < errs) pos[cnt] = n - 1 - i;
            cnt++;
        }
    }
    return (cnt == errs) ? (int)cnt : -1;
}

/* returns mlen on success (code corrected in place), -1 if uncorrectable. */
static inline int dcf_fec_decode(uint8_t *code, size_t n, uint8_t nparity, size_t mlen) {
    dcf__gf_init();
    uint8_t synd[DCF_FEC_NPARITY + 1];
    if (!dcf__syndromes(code, n, nparity, synd)) return (int)mlen; /* clean */

    uint8_t err_loc[DCF_FEC_MAXN + 2];
    size_t el = dcf__err_locator(synd, nparity, err_loc);
    if (el - 1 > (size_t)(nparity / 2)) return -1;

    uint8_t err_loc_rev[DCF_FEC_MAXN + 2];
    for (size_t i = 0; i < el; i++) err_loc_rev[i] = err_loc[el - 1 - i];
    size_t pos[DCF_FEC_NPARITY];
    if (dcf__err_positions(err_loc_rev, el, n, pos) < 0) return -1;
    size_t e = el - 1;

    /* errata locator e_loc = prod (alpha^coef_pos, 1); coef_pos = n-1-pos */
    uint8_t e_loc[DCF_FEC_MAXN + 2], tmp[DCF_FEC_MAXN + 2];
    size_t ell = 1;
    e_loc[0] = 1;
    uint8_t X[DCF_FEC_NPARITY];
    size_t coef_pos[DCF_FEC_NPARITY];
    for (size_t i = 0; i < e; i++) {
        coef_pos[i] = n - 1 - pos[i];
        uint8_t factor[2] = { dcf__gf_pow(DCF_FEC_GEN, (int)coef_pos[i]), 1 };
        size_t nl = dcf__pmul(e_loc, ell, factor, 2, tmp);
        memcpy(e_loc, tmp, nl);
        ell = nl;
        X[i] = dcf__gf_pow(DCF_FEC_GEN, (int)coef_pos[i]);
    }

    /* error evaluator: remainder of (synd_rev * e_loc) mod x^(e+1) = last (e+1) coeffs */
    uint8_t synd_rev[DCF_FEC_NPARITY + 1];
    for (uint8_t i = 0; i <= nparity; i++) synd_rev[i] = synd[nparity - i];
    uint8_t prod[2 * DCF_FEC_MAXN + 2];
    size_t pl = dcf__pmul(synd_rev, (size_t)nparity + 1, e_loc, ell, prod);
    size_t remlen = ell; /* e+1 */
    const uint8_t *rem = prod + (pl - remlen);

    /* Forney */
    for (size_t i = 0; i < e; i++) {
        uint8_t Xi = X[i], Xi_inv = dcf__gf_inv(Xi);
        uint8_t denom = 1;
        for (size_t j = 0; j < e; j++)
            if (j != i) denom = dcf__gf_mul(denom, (uint8_t)(1 ^ dcf__gf_mul(Xi_inv, X[j])));
        uint8_t numer = dcf__peval(rem, remlen, Xi_inv);
        numer = dcf__gf_mul(numer, dcf__gf_pow(Xi, 1 - DCF_FEC_FCR));
        if (denom == 0) return -1;
        code[pos[i]] ^= dcf__gf_div(numer, denom);
    }
    /* verify residual syndrome */
    if (dcf__syndromes(code, n, nparity, synd)) return -1;
    return (int)mlen;
}

/* ── block interleaver (spreads RF burst errors across codewords) ──────────────*/
/* depth codewords of length clen -> out[depth*clen], column-major. */
static inline void dcf_fec_interleave(const uint8_t *cws, size_t depth, size_t clen,
                                      uint8_t *out) {
    for (size_t r = 0; r < depth; r++)
        for (size_t c = 0; c < clen; c++)
            out[c * depth + r] = cws[r * clen + c];
}
static inline void dcf_fec_deinterleave(const uint8_t *in, size_t depth, size_t clen,
                                        uint8_t *cws) {
    for (size_t c = 0; c < clen; c++)
        for (size_t r = 0; r < depth; r++)
            cws[r * clen + c] = in[c * depth + r];
}

/* ── multi-codeword messages (any length: chunk + interleave + protected header) ─*/
#define DCF_FEC_HDR_PARITY 16
#define DCF_FEC_HDR_LEN    (5u + DCF_FEC_HDR_PARITY)   /* 21-byte self-protecting header */
#ifndef DCF_FEC_MSG_MAX
#define DCF_FEC_MSG_MAX    4096u                       /* max message length supported */
#endif
#define DCF_FEC_BLOB_MAX   (DCF_FEC_HDR_LEN + DCF_FEC_MSG_MAX + 64u * 255u)

static inline void dcf__chunking(size_t L, uint8_t np, size_t *nchunks, size_t *k) {
    size_t maxk = 255u - np;
    *nchunks = (L == 0) ? 1u : (L + maxk - 1u) / maxk;
    *k = (L == 0) ? 1u : (L + *nchunks - 1u) / *nchunks;
}

/* msg -> self-describing RS+interleave blob; returns blob length (0 if too large). */
static inline size_t dcf_fec_encode_message(const uint8_t *msg, size_t L, uint8_t np,
                                            uint8_t *out) {
    dcf__gf_init();
    if (L > DCF_FEC_MSG_MAX) return 0;
    size_t nchunks, k;
    dcf__chunking(L, np, &nchunks, &k);
    size_t cwlen = k + np;
    /* header codeword: [len u32 BE | nparity u8] under fixed HDR_PARITY */
    uint8_t hdr[5] = {(uint8_t)(L >> 24), (uint8_t)(L >> 16), (uint8_t)(L >> 8),
                      (uint8_t)L, np};
    dcf_fec_encode(hdr, 5, DCF_FEC_HDR_PARITY, out);            /* 21 bytes */
    /* build codewords from zero-padded blocks, then interleave into out+HDR_LEN */
    static uint8_t cws[64u * 255u];
    static uint8_t block[255u];
    for (size_t c = 0; c < nchunks; c++) {
        size_t off = c * k;
        for (size_t i = 0; i < k; i++) block[i] = (off + i < L) ? msg[off + i] : 0u;
        dcf_fec_encode(block, k, np, cws + c * cwlen);
    }
    dcf_fec_interleave(cws, nchunks, cwlen, out + DCF_FEC_HDR_LEN);
    return DCF_FEC_HDR_LEN + nchunks * cwlen;
}

/* blob -> message into out (>= nchunks*k bytes); returns message length, or -1. */
static inline int dcf_fec_decode_message(const uint8_t *blob, size_t blen, uint8_t *out) {
    dcf__gf_init();
    if (blen < DCF_FEC_HDR_LEN) return -1;
    uint8_t hbuf[DCF_FEC_HDR_LEN];
    memcpy(hbuf, blob, DCF_FEC_HDR_LEN);
    if (dcf_fec_decode(hbuf, DCF_FEC_HDR_LEN, DCF_FEC_HDR_PARITY, 5) < 0) return -1;
    size_t L = ((size_t)hbuf[0] << 24) | ((size_t)hbuf[1] << 16) |
               ((size_t)hbuf[2] << 8) | hbuf[3];
    uint8_t np = hbuf[4];
    if (L > DCF_FEC_MSG_MAX) return -1;
    size_t nchunks, k;
    dcf__chunking(L, np, &nchunks, &k);
    size_t cwlen = k + np;
    if (blen != DCF_FEC_HDR_LEN + nchunks * cwlen) return -1;
    static uint8_t cws[64u * 255u];
    dcf_fec_deinterleave(blob + DCF_FEC_HDR_LEN, nchunks, cwlen, cws);
    for (size_t c = 0; c < nchunks; c++) {
        uint8_t *cw = cws + c * cwlen;
        if (dcf_fec_decode(cw, cwlen, np, k) < 0) return -1;
        memcpy(out + c * k, cw, k);                            /* last block zero-padded */
    }
    return (int)L;
}

#endif /* DCF_DEMOD_FEC_H */
