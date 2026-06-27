/* hydra_dsp_ref.c -- portable C reference DSP.
 *
 * Mirrors faust/demod_modem.lib function-for-function so that validating the
 * algorithm here validates exactly what Faust compiles to:
 *
 *   phase01(freq)        -> tx phase accumulator (double, wraps in [0,1))
 *   cpfsk(freq)          -> sin(2*pi*phase)
 *   lo_sin/lo_cos(fc)    -> per-tone quadrature local oscillators
 *   tone_energy(fc,lp)   -> |LPF(x*lo_sin)|^2 + |LPF(x*lo_cos)|^2
 *   tonebank(N,...)      -> N parallel tone_energy detectors
 *
 * The lowpass is a 2nd-order Butterworth (Q = 1/sqrt2), the same magnitude
 * response as Faust's fi.lowpass(2, fc); coefficients are RBJ-cookbook, so the
 * result is response-equivalent though not bit-identical to Faust's internal
 * tf2 form. Everything is double precision (deployment uses -double).
 */
#include "hydra_dsp.h"
#include <math.h>
#include <stdlib.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* ------------------------------- transmitter ------------------------------ */
struct hydra_tx_dsp {
    double sr;
    double phase;     /* normalized [0,1) */
};

hydra_tx_dsp *hydra_tx_dsp_create(double sample_rate)
{
    hydra_tx_dsp *d = (hydra_tx_dsp *)calloc(1, sizeof *d);
    if (!d) return NULL;
    d->sr = sample_rate;
    d->phase = 0.0;
    return d;
}

void hydra_tx_dsp_destroy(hydra_tx_dsp *d) { free(d); }

void hydra_tx_dsp_process(hydra_tx_dsp *d, const float *freq_in, float *audio_out, int n)
{
    int i;
    for (i = 0; i < n; ++i) {
        d->phase += (double)freq_in[i] / d->sr;
        d->phase -= floor(d->phase);                  /* ma.decimal wrap */
        audio_out[i] = (float)sin(2.0 * M_PI * d->phase);
    }
}

/* -------------------------------- receiver -------------------------------- */
/* Raw quadrature down-conversion per tone: I = x*cos, Q = x*sin. No filtering;
 * the matched filter is the per-symbol integrate-and-dump in hydra_modem.c.
 * Mirrors faust/demod_modem.lib tone_iq / tonebank_iq exactly. */
typedef struct {
    double lo_phase;     /* shared phase for the sin & cos LOs */
    double dphi;         /* per-sample phase increment         */
} tone_lo;

struct hydra_rx_dsp {
    double   sr;
    int      n_tones;
    tone_lo *lo;
};

hydra_rx_dsp *hydra_rx_dsp_create(const hydra_profile *p)
{
    int k;
    hydra_rx_dsp *d = (hydra_rx_dsp *)calloc(1, sizeof *d);
    if (!d) return NULL;
    d->sr = p->sample_rate;
    d->n_tones = p->n_tones;
    d->lo = (tone_lo *)calloc((size_t)p->n_tones, sizeof *d->lo);
    if (!d->lo) { free(d); return NULL; }
    for (k = 0; k < p->n_tones; ++k) {
        d->lo[k].lo_phase = 0.0;
        d->lo[k].dphi     = hydra_tone_freq(p, k) / d->sr;
    }
    return d;
}

void hydra_rx_dsp_destroy(hydra_rx_dsp *d)
{
    if (!d) return;
    free(d->lo);
    free(d);
}

void hydra_rx_dsp_process(hydra_rx_dsp *d, const float *audio_in, float *iq_out, int n)
{
    int i, k;
    int N = d->n_tones;
    for (i = 0; i < n; ++i) {
        double x = (double)audio_in[i];
        for (k = 0; k < N; ++k) {
            tone_lo *t = &d->lo[k];
            double ph = 2.0 * M_PI * t->lo_phase;
            iq_out[((size_t)i * N + k) * 2 + 0] = (float)(x * cos(ph));  /* I */
            iq_out[((size_t)i * N + k) * 2 + 1] = (float)(x * sin(ph));  /* Q */
            t->lo_phase += t->dphi;
            t->lo_phase -= floor(t->lo_phase);
        }
    }
}
