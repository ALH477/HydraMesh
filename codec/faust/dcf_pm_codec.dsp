// dcf_pm_codec.dsp — DCF-Audio codec_id 2: musical phase-modulation synthesis.
// DeMoD LLC | LGPL-3.0
//
// The decoder side of the PM codec: a compact 8-byte parameter block (see
// dcf_pm_pack / PmParams) drives a ratio-locked phase-modulation oscillator that
// resynthesises a playable timbre.  This .dsp is the readable spec; the committed
// dcf_pm_codec.gen.c is its Faust -lang c output, wrapped by a small FFI adapter.
//
// Controls are set once per 20 ms block by the host from the decoded params.
// (Audio output is intentionally NOT byte-certified across languages — only the
//  8-byte parameter layout is.  See DCF_AUDIO_SPEC.md.)
import("stdfaust.lib");

f0       = nentry("f0",        220.0, 20.0, 8000.0, 0.001);   // fundamental, Hz
amp      = nentry("amp",         0.5,  0.0,    1.0, 0.0001);  // output amplitude
modIndex = nentry("mod_index",   1.0,  0.0,    8.0, 0.0001);  // PM depth
modRatio = nentry("mod_ratio",   1.0,  0.25,   8.0, 0.0001);  // carrier:modulator
bright   = nentry("bright",      0.0,  0.0,    1.0, 0.0001);  // brightness / feedback
env      = nentry("env",         1.0,  0.0,    1.0, 0.0001);  // amplitude envelope

modulator = os.osc(f0 * modRatio) * modIndex;
carrier   = os.oscp(f0, modulator * (1.0 + bright));
process   = carrier * amp * env;
