// SPDX-License-Identifier: LGPL-3.0-only
// ============================================================================
// DCF RF Modulator — Faust I/Q baseband synthesis for the SDR modem.
//
// Companion to dcf_modem.dsp (the *audio* passband modem): this file renders the
// same certified symbols (codec/demod_modulation.h byte<->symbol Gray map) to
// **complex baseband I/Q** for an SDR sink (.cf32 / SoapySDR). Each `process` has
// TWO outputs: I(t), Q(t). As with all DCF waveform layers, the symbol mapping +
// the Reed-Solomon FEC are byte-certified; the I/Q waveform here is analog and
// loopback-tested (the functional reference is python/modem/iq.py). See
// Documentation/DCF_SDR_SPEC.md.
//
//   FSK/GFSK : CPFSK, bit -> -/+ deviation, continuous phase
//   OOK/ASK  : amplitude-shift on I
//   PSK      : QPSK constellation point at (pi/2)*value
//   QAM      : 16-QAM I/Q levels {-3,-1,1,3} (Gray), normalised by 3
// ============================================================================

declare name      "DCF RF Modulator";
declare author    "DeMoD LLC";
declare license   "LGPL-3.0-only";
declare version   "0.3.0";

import("stdfaust.lib");

fdev = 0.25;                       // FSK frequency deviation (cycles/sample)

// Variable-frequency phase accumulator (radians): integrates per-sample frequency.
phase(f) = f : (+ ~ _) : *(2.0 * ma.PI);

// FSK / GFSK baseband: bit in {0,1} -> -/+fdev, continuous-phase -> (I, Q).
fsk_iq(bit) = cos(p), sin(p) with { p = phase(select2(bit, 0.0 - fdev, fdev)); };

// OOK / ASK baseband: amplitude on I (Q = 0).
ook_iq(bit) = bit, 0.0;

// PSK (QPSK) baseband: value in {0..3} -> unit-circle point at (pi/2)*value.
psk_iq(value) = cos(a), sin(a) with { a = (ma.PI / 2.0) * value; };

// QAM (16) baseband: host supplies the Gray-decoded i,q levels in {-3,-1,1,3}.
qam_iq(i, q) = i / 3.0, q / 3.0;

// Default process renders binary CPFSK (the GFSK/FSK data path), the most common
// SDR mode; the host/codegen selects the scheme per modulation_id.
process = fsk_iq;
