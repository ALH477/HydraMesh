/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * dcf_modem.h — the Faust-DSP modem's waveform layer: render Gray-coded symbols
 * (from codec/demod_modulation.h, the certified mapping) into actual modulated
 * sample snippets, and recover them by matched filtering. This is the *analog*,
 * NON-certified half (mirroring DCF-Audio: the synthesis is loopback-tested, not
 * byte-certified across languages). The waveform designs match codec/faust/dcf_modem.dsp.
 *
 *   FSK : two tones (Bell-202-style mark/space)        OOK : on/off amplitude
 *   PSK : QPSK carrier phase (0/90/180/270)            QAM : 16-QAM I/Q baseband
 *
 * Per-symbol snippet = DCF_MODEM_SPS samples. recover() picks, for each window,
 * the canonical snippet it matches best — exact over an ideal (loopback/file)
 * medium, robust under mild noise on a real medium.
 */
#ifndef DCF_MODEM_H
#define DCF_MODEM_H

#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include "demod_modulation.h"

#ifndef DCF_MODEM_SPS
#define DCF_MODEM_SPS 16        /* samples per symbol */
#endif
#define DCF_MODEM_FC   0.25     /* carrier (cycles/sample) for PSK/QAM */
#define DCF_MODEM_F0   0.1875   /* FSK space tone */
#define DCF_MODEM_F1   0.3125   /* FSK mark tone  */
#define DCF_MODEM_PI   3.14159265358979323846

/* QAM 16 constellation, indexed by symbol = Gray(value); matches modulationlab. */
static inline void dcf_modem_qam_iq(uint8_t sym, double *i, double *q) {
    static const double level[4] = {-3, -1, 3, 1}; /* 2-bit Gray order */
    unsigned v = dcf_ungray(sym);
    *i = level[(v >> 2) & 3];
    *q = level[v & 3];
}

/* Render one symbol into DCF_MODEM_SPS samples. */
static inline void dcf_modem_snippet(uint8_t mod, uint8_t sym, double *out) {
    for (int n = 0; n < DCF_MODEM_SPS; n++) {
        double t = (double)n;
        switch (mod) {
            case DCF_MOD_FSK: {
                double f = sym ? DCF_MODEM_F1 : DCF_MODEM_F0;
                out[n] = sin(2.0 * DCF_MODEM_PI * f * t);
                break;
            }
            case DCF_MOD_OOK:
                out[n] = sym ? sin(2.0 * DCF_MODEM_PI * DCF_MODEM_FC * t) : 0.0;
                break;
            case DCF_MOD_PSK: {
                double phase = (DCF_MODEM_PI / 2.0) * (double)dcf_ungray(sym);
                out[n] = cos(2.0 * DCF_MODEM_PI * DCF_MODEM_FC * t + phase);
                break;
            }
            case DCF_MOD_QAM: {
                double i, q;
                dcf_modem_qam_iq(sym, &i, &q);
                out[n] = (i * cos(2.0 * DCF_MODEM_PI * DCF_MODEM_FC * t)
                          - q * sin(2.0 * DCF_MODEM_PI * DCF_MODEM_FC * t)) / 3.0;
                break;
            }
            default: out[n] = 0.0;
        }
    }
}

/* Render a symbol stream to samples. Returns sample count (= nsyms*SPS) or 0 if too small. */
static inline size_t dcf_modem_render(uint8_t mod, const uint8_t *syms, size_t nsyms,
                                      double *out, size_t max_samples) {
    if (nsyms * (size_t)DCF_MODEM_SPS > max_samples) return 0;
    for (size_t s = 0; s < nsyms; s++)
        dcf_modem_snippet(mod, syms[s], out + s * DCF_MODEM_SPS);
    return nsyms * (size_t)DCF_MODEM_SPS;
}

/* Recover symbols by matched filtering each SPS window against the canonical set. */
static inline size_t dcf_modem_recover(uint8_t mod, const double *samples, size_t nsamples,
                                       uint8_t *syms, size_t max_syms) {
    unsigned nsym_vals = 1u << DCF_MOD_BITS_PER_SYMBOL[mod];
    size_t nsyms = nsamples / (size_t)DCF_MODEM_SPS;
    if (nsyms > max_syms) return 0;
    double cand[DCF_MODEM_SPS];
    for (size_t w = 0; w < nsyms; w++) {
        const double *win = samples + w * DCF_MODEM_SPS;
        double best = 1e300;
        uint8_t best_sym = 0;
        for (unsigned sym = 0; sym < nsym_vals; sym++) {
            dcf_modem_snippet(mod, (uint8_t)sym, cand);
            double err = 0.0;
            for (int n = 0; n < DCF_MODEM_SPS; n++) {
                double d = win[n] - cand[n];
                err += d * d;
            }
            if (err < best) { best = err; best_sym = (uint8_t)sym; }
        }
        syms[w] = best_sym;
    }
    return nsyms;
}

#endif /* DCF_MODEM_H */
