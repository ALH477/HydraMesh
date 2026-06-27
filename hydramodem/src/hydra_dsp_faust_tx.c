/* hydra_dsp_faust_tx.c -- TX half of hydra_dsp.h backed by the Faust-generated
 * one-sample C code. Compiled as its OWN translation unit (the generated file
 * defines class-suffixed symbols plus a few file-scope macros like FAUSTCLASS;
 * keeping each backend in a separate TU avoids any clash between the TX and RX
 * generated files). Build with `make faust`.
 *
 * Generate first (see BUILD.md):
 *   faust -lang c -os -double -ftz 2 -cn hydratx -I faust \
 *         faust/hydramodem_tx.dsp -o build/hydratx.c
 *
 * The Faust C glue header provides the UIGlue/MetaGlue typedefs the generated
 * metadata/UI builders reference (we never call them, but they must compile).
 */
#include "hydra_dsp.h"
#include <stdlib.h>

#include <faust/gui/CInterface.h>   /* UIGlue, MetaGlue */
#include "../build/hydratx.c"       /* hydratx + new/init/compute/delete       */

#define HYDRA_ICTRL_MAX 8
#define HYDRA_FCTRL_MAX 8

struct hydra_tx_dsp {
    hydratx *dsp;
    int      ictrl[HYDRA_ICTRL_MAX];
    double   fctrl[HYDRA_FCTRL_MAX];
};

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
        controlhydratx(d->dsp, d->ictrl, d->fctrl);
        for (i = 0; i < warm; ++i)
            computehydratx(d->dsp, &f, &o, d->ictrl, d->fctrl);
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
    /* -os mode: precompute control-rate values (here, the gain) into the
     * control arrays before the per-sample compute loop. */
    controlhydratx(d->dsp, d->ictrl, d->fctrl);
    for (i = 0; i < n; ++i) {
        FAUSTFLOAT in  = freq_in[i];
        FAUSTFLOAT out = 0.0f;
        computehydratx(d->dsp, &in, &out, d->ictrl, d->fctrl);
        audio_out[i] = out;
    }
}
