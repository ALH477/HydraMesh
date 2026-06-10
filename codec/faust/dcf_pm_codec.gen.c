/*
 * dcf_pm_codec.gen.c — committed FFI adapter around the Faust phase-mod synthesis.
 * DeMoD LLC | LGPL-3.0
 *
 * The raw Faust output is dcf_pm_faust.c (regenerated from dcf_pm_codec.dsp and
 * diffed in CI). This file adds minimal glue typedefs so that artifact compiles
 * standalone, then exposes one C-ABI entry point used by both the C SDK
 * (demod_audio.h, codec_id 2 decode) and the Rust crate (audio::pm via build.rs):
 *
 *     void dcf_pm_synth_block_ffi(const uint8_t params8[8], float *out, int n);
 *
 * It resynthesises `n` PCM samples (48 kHz mono) from a decoded 8-byte PM param
 * block. NOTE: PM *synthesis audio* is intentionally NOT byte-certified across
 * languages — only the 8-byte parameter layout is (see DCF_AUDIO_SPEC.md).
 */
#include <math.h>
#include <stdint.h>

#define FAUSTFLOAT float

/* Minimal stand-ins for Faust's CInterface.h: only the members referenced by the
 * generated metadata/UI functions, which we never call (we set zones directly). */
typedef struct {
    void *uiInterface;
    void (*openVerticalBox)(void *, const char *);
    void (*addNumEntry)(void *, const char *, FAUSTFLOAT *, FAUSTFLOAT, FAUSTFLOAT, FAUSTFLOAT, FAUSTFLOAT);
    void (*closeBox)(void *);
} UIGlue;

typedef struct {
    void *metaInterface;
    void (*declare)(void *, const char *, const char *);
} MetaGlue;

#include "dcf_pm_faust.c"   /* verbatim Faust 2.83 output: DcfPmCodec + compute */

/* Param byte indices (must match dcf_pm_pack / PmParams). */
enum { PM_F0_HI = 0, PM_F0_LO, PM_AMP, PM_MOD_INDEX, PM_MOD_RATIO, PM_BRIGHT, PM_ENV, PM_FLAGS };

static float dcf__clampf(float x, float lo, float hi) {
    return x < lo ? lo : x > hi ? hi : x;
}

void dcf_pm_synth_block_ffi(const uint8_t *p, float *out, int n) {
    static DcfPmCodec *dsp = NULL;
    if (!dsp) {
        classInitDcfPmCodec(48000);
        dsp = newDcfPmCodec();
        initDcfPmCodec(dsp, 48000);
    }
    /* Decode the 8-byte param block into the Faust control zones. These mappings
     * are runtime (not certified); they invert the host analysis encoder. */
    uint16_t f0u = (uint16_t)(((uint16_t)p[PM_F0_HI] << 8) | p[PM_F0_LO]);
    dsp->fEntry0 = dcf__clampf(20.0f * powf(2.0f, (float)f0u / 1200.0f), 20.0f, 8000.0f); /* f0 Hz */
    dsp->fEntry4 = (float)p[PM_AMP] / 255.0f;                       /* amp   0..1   */
    dsp->fEntry3 = dcf__clampf((float)p[PM_MOD_INDEX] / 255.0f * 8.0f, 0.0f, 8.0f); /* mod_index */
    dsp->fEntry1 = dcf__clampf((float)p[PM_MOD_RATIO] / 16.0f, 0.25f, 8.0f);        /* mod_ratio */
    dsp->fEntry2 = (float)p[PM_BRIGHT] / 255.0f;                    /* bright 0..1  */
    dsp->fEntry5 = (float)p[PM_ENV] / 255.0f;                       /* env    0..1  */

    FAUSTFLOAT *outs[1] = { out };
    computeDcfPmCodec(dsp, n, NULL, outs);
}
