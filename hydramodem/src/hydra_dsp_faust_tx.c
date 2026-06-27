/* hydra_dsp_faust_tx.c -- TX half of hydra_dsp.h backed by the Faust-generated
 * one-sample C code. Compiled as its OWN translation unit (the generated file
 * defines class-suffixed symbols plus a few file-scope macros like FAUSTCLASS;
 * keeping each backend in a separate TU avoids any clash between the TX and RX
 * generated files). Build with `make faust`.
 *
 * Generate first (see BUILD.md / FAUST_MODERNIZATION.md):
 *   faust -lang c -os -double -cn hydratx -I faust \
 *         faust/hydramodem_tx.dsp -o build/hydratx.c
 *   (no -ftz: it emits malformed C on Faust >= 2.83; the reference DSP doesn't
 *    flush denormals, so output stays equivalent.)
 *
 * The Faust C glue header provides the UIGlue/MetaGlue typedefs the generated
 * metadata/UI builders reference (we never call them, but they must compile).
 */
#include "hydra_dsp.h"
#include <stdlib.h>

#include <faust/gui/CInterface.h>   /* UIGlue, MetaGlue */
#include "../build/hydratx.c"       /* hydratx + new/init/compute/delete       */

/* Faust's one-sample (`-os`) architecture changed ABI around 2.75:
 *   OLD (<= 2.74): controlhydratx(dsp,ic,fc) once + per-sample
 *                  computehydratx(dsp,&in,&out,ic,fc)
 *   NEW (>= 2.83): per-sample framehydratx(dsp,&in,&out); control folded in,
 *                  no control fn (and the block computehydratx is an empty stub).
 * The generated file #defines FAUST_REAL_CONTROLS only in the OLD ABI, so key
 * off it. See hydramodem/docs/FAUST_MODERNIZATION.md. */
#if defined(FAUST_REAL_CONTROLS)
#  define HYDRA_FAUST_OLD_OS 1
#endif

#define HYDRA_ICTRL_MAX 8
#define HYDRA_FCTRL_MAX 8

struct hydra_tx_dsp {
    hydratx *dsp;
#ifdef HYDRA_FAUST_OLD_OS
    int      ictrl[HYDRA_ICTRL_MAX];
    double   fctrl[HYDRA_FCTRL_MAX];
#endif
};

/* control-rate precompute (no-op on the new ABI; folded into the per-sample fn) */
static inline void hydra_tx_control(hydra_tx_dsp *d)
{
#ifdef HYDRA_FAUST_OLD_OS
    controlhydratx(d->dsp, d->ictrl, d->fctrl);
#else
    (void)d;
#endif
}

/* one audio sample: instantaneous-frequency in -> sample out */
static inline void hydra_tx_step(hydra_tx_dsp *d, FAUSTFLOAT *in, FAUSTFLOAT *out)
{
#ifdef HYDRA_FAUST_OLD_OS
    computehydratx(d->dsp, in, out, d->ictrl, d->fctrl);
#else
    framehydratx(d->dsp, in, out);
#endif
}

hydra_tx_dsp *hydra_tx_dsp_create(double sample_rate)
{
    hydra_tx_dsp *d = (hydra_tx_dsp *)calloc(1, sizeof *d);
    int i;
    if (!d) return NULL;
    d->dsp = newhydratx();
    if (!d->dsp) { free(d); return NULL; }
    inithydratx(d->dsp, (int)sample_rate);
    /* drive the "TX Gain" slider to unity; hydra_modem.c applies tx_gain so the
     * reference and Faust paths produce the same amplitude. */
    d->dsp->fHslider0 = (FAUSTFLOAT)1.0;

    /* WARM-UP: the .dsp's txout applies si.smoo to the gain and an fi.dcblocker,
     * both of which have a multi-thousand-sample startup transient. Run the DSP
     * through a fixed tone and discard the output so those settle to steady
     * state BEFORE the real frame is modulated. This keeps hydra_modem.c
     * backend-agnostic (it need not know the DSP contains smoothers) and makes
     * the Faust path bit-compatible in timing with the flat-gain C reference. */
    {
        const int   warm = 8192;            /* > 4 * si.smoo time constant */
        FAUSTFLOAT  f = (FAUSTFLOAT)(sample_rate / 24.0), o;
        hydra_tx_control(d);
        for (i = 0; i < warm; ++i)
            hydra_tx_step(d, &f, &o);
    }
    return d;
}

void hydra_tx_dsp_destroy(hydra_tx_dsp *d)
{
    if (!d) return;
    if (d->dsp) deletehydratx(d->dsp);
    free(d);
}

void hydra_tx_dsp_process(hydra_tx_dsp *d, const float *freq_in, float *audio_out, int n)
{
    int i;
    /* -os mode: precompute control-rate values (here, the gain) before the
     * per-sample loop (no-op on the new ABI). */
    hydra_tx_control(d);
    for (i = 0; i < n; ++i) {
        FAUSTFLOAT in  = freq_in[i];
        FAUSTFLOAT out = 0.0f;
        hydra_tx_step(d, &in, &out);
        audio_out[i] = out;
    }
}
