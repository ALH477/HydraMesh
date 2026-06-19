/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * demod_modulation.h — DCF modulation mapping: the byte<->symbol layer of the
 * Faust-DSP modem, byte-identical to python/MCP/modulationlab_core.py and
 * codec/src/modulation.rs. This is the CERTIFIED half of the modem (the Faust
 * waveform that renders/recovers these symbols is analog and is NOT certified —
 * exactly like DCF-Audio certifies L2 framing but not synthesised audio).
 *
 * Rule: MSB-first bits, zero-padded to a multiple of bits_per_symbol, each group
 * Gray-coded to a symbol index. demodulate(modulate(x), len(x)) == x.
 */
#ifndef DCF_DEMOD_MODULATION_H
#define DCF_DEMOD_MODULATION_H

#include <stddef.h>
#include <stdint.h>

#define DCF_MOD_FSK 0u   /* binary FSK  (1 bit/symbol) */
#define DCF_MOD_OOK 1u   /* on-off keying (1 bit/symbol) */
#define DCF_MOD_PSK 2u   /* QPSK, Gray  (2 bits/symbol) */
#define DCF_MOD_QAM 3u   /* 16-QAM, Gray (4 bits/symbol) */

static const uint8_t DCF_MOD_BITS_PER_SYMBOL[4] = {1, 1, 2, 4};

static inline unsigned dcf_gray(unsigned n) { return n ^ (n >> 1); }
static inline unsigned dcf_ungray(unsigned g) {
    unsigned n = 0;
    while (g) { n ^= g; g >>= 1; }
    return n;
}

/* bytes -> Gray-coded symbol indices. Returns symbol count, or 0 if syms_out is
 * too small (need ceil(8*len / bits_per_symbol)). */
static inline size_t dcf_modulate(uint8_t mod, const uint8_t *data, size_t len,
                                  uint8_t *syms_out, size_t max_syms) {
    unsigned bps = DCF_MOD_BITS_PER_SYMBOL[mod];
    size_t total_bits = len * 8u;
    size_t nsyms = (total_bits + bps - 1u) / bps;
    if (nsyms > max_syms) return 0;
    for (size_t s = 0; s < nsyms; s++) {
        unsigned val = 0;
        for (unsigned i = 0; i < bps; i++) {
            size_t bi = s * bps + i;
            unsigned bit = (bi < total_bits) ? ((data[bi / 8] >> (7u - (bi % 8u))) & 1u) : 0u;
            val = (val << 1) | bit;
        }
        syms_out[s] = (uint8_t)dcf_gray(val);
    }
    return nsyms;
}

/* symbol indices -> bytes (first nbytes recovered). Returns nbytes. */
static inline size_t dcf_demodulate(uint8_t mod, const uint8_t *syms, size_t nsyms,
                                    uint8_t *out, size_t nbytes) {
    unsigned bps = DCF_MOD_BITS_PER_SYMBOL[mod];
    for (size_t b = 0; b < nbytes; b++) {
        unsigned byte = 0;
        for (unsigned k = 0; k < 8u; k++) {
            size_t bi = b * 8u + k;
            size_t s = bi / bps;
            unsigned within = (unsigned)(bi % bps);
            unsigned bit = 0;
            if (s < nsyms) bit = (dcf_ungray(syms[s]) >> (bps - 1u - within)) & 1u;
            byte = (byte << 1) | bit;
        }
        out[b] = (uint8_t)byte;
    }
    return nbytes;
}

#endif /* DCF_DEMOD_MODULATION_H */
