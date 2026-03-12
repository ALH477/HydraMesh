// ═══════════════════════════════════════════════════════════════════════════════
// DeMoD DCF Acoustic Modem - Handshakeless Over-Air Communication
// ═══════════════════════════════════════════════════════════════════════════════
//
// Audio-frequency FSK modem implementing the DCF mathematical formalization.
// Operates in the 300-3400 Hz telephone band for speaker-to-microphone
// communication between two computers over open air.
//
// Signal Design (Bell 202 inspired):
//   Mark  (bit 1): 1200 Hz      Space (bit 0): 2200 Hz
//   Baud rate:     300 symbols/sec
//   Bandwidth:     ~1000 Hz occupied within 700-2700 Hz
//
// Mathematical Mapping:
//   M_xi  (S4.2)  Modulation operator     -> fsk_modulate
//   T_a   (S4.1)  Translation operator    -> channel propagation (air)
//   |F|^2 (S4.4)  Magnitude spectrum      -> bandpass energy detectors
//   S_xx  (S6.1)  Power spectral density  -> exponential PSD averaging
//   F_H x F_P     (S7.4) Tensor product   -> independent header/payload detect
//
// License: LGPL-3.0 | Patent Pending
// Based on a secure protocol validated by the United States Air Force.
// Originally designed for DeMoD Guitars by Asher, founder of DeMoD LLC.
//
// ═══════════════════════════════════════════════════════════════════════════════

declare name        "DeMoD DCF Acoustic Modem";
declare author      "Asher - DeMoD LLC";
declare copyright   "(c) 2025 DeMoD LLC";
declare license     "LGPL-3.0 | Patent Pending";
declare version     "2.0.0";
declare description "Acoustic FSK modem for handshakeless over-air DCF communication. Speaker-to-mic dial-up.";

import("stdfaust.lib");


// ═══════════════════════════════════════════════════════════════════════════════
// Constants
// ═══════════════════════════════════════════════════════════════════════════════

// Bell 202 dial-up frequencies
MARK_DEFAULT  = 1200.0;    // Hz - bit 1
SPACE_DEFAULT = 2200.0;    // Hz - bit 0
BAUD_RATE     = 300.0;     // symbols per second

// Bandpass filter parameters
BPF_ORDER     = 4;         // 4th order Butterworth
BPF_BW        = 200.0;     // Hz bandwidth per tone detector

// PSD smoothing (S6.1)
PSD_FAST      = 0.02;      // fast attack for bit-level tracking
PSD_SLOW      = 0.001;     // slow decay for noise floor estimation

// Pade tanh saturator (DeMoD standard)
// tanh(x) ~ x(15 + x^2) / (15 + 6x^2)
pade_tanh(x) = x * (15.0 + x*x) / (15.0 + 6.0*x*x);


// ═══════════════════════════════════════════════════════════════════════════════
// UI Parameters
// ═══════════════════════════════════════════════════════════════════════════════

// Mode: 0 = TX (transmit), 1 = RX (receive)
mode = nentry("mode", 0, 0, 1, 1);

// Current bit to transmit (set by Python each symbol period)
// 0 = space tone, 1 = mark tone, 2 = silence (inter-frame gap)
tx_bit = nentry("tx_bit", 2, 0, 2, 1);

// Carrier frequencies (adjustable for different acoustic environments)
mark_freq  = hslider("mark_freq",  MARK_DEFAULT,  400, 4000, 1) : si.smoo;
space_freq = hslider("space_freq", SPACE_DEFAULT, 800, 6000, 1) : si.smoo;

// TX output gain
tx_gain_db = hslider("tx_gain_db", -3.0, -40.0, 0.0, 0.1) : si.smoo;
tx_gain    = tx_gain_db : ba.db2linear;

// Detection threshold
detect_thresh = hslider("detect_thresh", 8.0, 1.0, 50.0, 0.1) : si.smoo;


// ═══════════════════════════════════════════════════════════════════════════════
// S4.2 - FSK Modulation Operator M_xi
// ═══════════════════════════════════════════════════════════════════════════════
//
// M_xi f(x) = e^{2*pi*i*<x, xi>} * f(x)
//
// For FSK, xi alternates between mark and space frequencies depending
// on the current bit value. The modulation operator produces a pure
// sinusoid at the selected frequency.
//
// Phase continuity between frequency transitions prevents spectral
// splatter and is achieved by using a single continuous-phase oscillator.
// ═══════════════════════════════════════════════════════════════════════════════

fsk_modulate = output * tx_gain
with {
    // Select frequency based on current bit
    // tx_bit: 0=space, 1=mark, 2=silence
    target_freq = ba.if(tx_bit > 1.5, 0.0,
                  ba.if(tx_bit > 0.5, mark_freq, space_freq));

    // Continuous-phase oscillator
    // Phase accumulates continuously even across frequency changes
    // This prevents clicks and spectral artifacts at bit transitions
    phase_inc = target_freq / ma.SR;
    phase = (+ (phase_inc)) ~ (_ <: _ , (_ >= 1.0) : - );
    osc = sin(phase * 2.0 * ma.PI);

    // Gate: silence when tx_bit == 2
    gate = tx_bit < 1.5;
    output = osc * gate;
};


// ═══════════════════════════════════════════════════════════════════════════════
// S4.4 / S8 - Magnitude Spectrum |F_N[k]|^2 via Bandpass Energy Detection
// ═══════════════════════════════════════════════════════════════════════════════
//
// |F{f}(w_k)|^2 = energy at frequency w_k
//
// Implemented as bandpass filter centered at w_k, followed by squaring
// (magnitude squared) and smoothing (time-averaging).
//
// This is mathematically equivalent to a sliding-window Goertzel bin:
// both compute the spectral energy at a single frequency. The bandpass
// approach is more robust for acoustic channels because the filter
// bandwidth accommodates Doppler shift and oscillator drift.
//
// The magnitude-squared step is where phase is discarded:
//   |e^{-2*pi*i*a*w} * F{f}(w)|^2 = |F{f}(w)|^2
// This is the commutation identity from S4.3 in action.
// ═══════════════════════════════════════════════════════════════════════════════

// Bandpass energy detector at frequency fc with bandwidth bw
// Returns |F{f}(fc)|^2 - the magnitude-squared spectral energy
// Phase information is discarded (handshakeless property)
bp_energy(fc, bw) = _ : bandpass : squared : smoother
with {
    // 4th-order Butterworth bandpass
    bandpass = fi.bandpass(BPF_ORDER, fc - bw*0.5, fc + bw*0.5);

    // |.|^2 - discard phase, keep magnitude (S4.4)
    squared = _ <: * ;

    // Exponential smoothing - implements time-averaging toward S_xx
    smoother = si.smooth(1.0 - PSD_FAST);
};


// ═══════════════════════════════════════════════════════════════════════════════
// S6.1 - Power Spectral Density Estimator S_xx(w)
// ═══════════════════════════════════════════════════════════════════════════════
//
// S_xx(w) = lim_{T->inf} (1/2T) * E[|F{x_T(t)}|^2]
//
// The noise floor estimator uses very slow smoothing (PSD_SLOW) to track
// the long-term average energy at an off-carrier frequency. This
// represents E[|F{noise}|^2] - the noise component of S_xx.
//
// Detection confidence = carrier S_xx / noise S_xx (SNR estimate)
// ═══════════════════════════════════════════════════════════════════════════════

// Noise floor PSD at an off-carrier frequency
// Uses slow exponential averaging to estimate E[|noise|^2]
noise_psd_estimator = bp_energy(3500.0, 300.0)
                    : si.smooth(1.0 - PSD_SLOW)
                    : max(1e-10);  // prevent division by zero


// ═══════════════════════════════════════════════════════════════════════════════
// S7 - Header Detection and Payload Extraction (Tensor Product)
// ═══════════════════════════════════════════════════════════════════════════════
//
// F_{H x P} = F_H (x) F_P
//
// Header detection: mark_energy + space_energy vs noise floor
//   If the total FSK carrier energy exceeds the noise floor by the
//   detection threshold, a DCF frame is present.
//   This is handshakeless: no preamble, no sync - just spectral energy.
//
// Bit demodulation: mark_energy vs space_energy
//   After detection, the dominant tone determines the current bit.
//   This operates on the payload subband independently of detection.
// ═══════════════════════════════════════════════════════════════════════════════

fsk_demodulate(sig) = sig <: (detection_confidence, soft_bit, demod_audio)
with {
    // S4.4: Compute |F{f}|^2 at mark and space frequencies
    mark_energy  = sig : bp_energy(mark_freq, BPF_BW);
    space_energy = sig : bp_energy(space_freq, BPF_BW);

    // S6.1: Noise floor PSD
    noise_floor = sig : noise_psd_estimator;

    // S7.3: Detection by threshold
    // Total carrier energy / noise energy = SNR
    carrier_total = mark_energy + space_energy;
    snr = carrier_total / noise_floor;

    // Confidence: SNR normalized to [0, 1] via Pade tanh (DeMoD standard)
    detection_confidence = pade_tanh((snr - 1.0) / detect_thresh)
                         : max(0.0) : min(1.0);

    // S7.4: Bit decision (tensor product - independent of detection)
    // mark_energy > space_energy -> bit 1, else bit 0
    // Normalized to [-1, +1] for soft decision
    energy_diff = mark_energy - space_energy;
    energy_sum  = mark_energy + space_energy : max(1e-10);
    soft_bit    = energy_diff / energy_sum;

    // Demodulated audio (baseband reconstruction for monitoring)
    demod_audio = sig : fi.bandpass(BPF_ORDER,
                        mark_freq - BPF_BW, space_freq + BPF_BW)
                      : pade_tanh
                      : fi.dcblocker;
};


// ═══════════════════════════════════════════════════════════════════════════════
// Main Process
// ═══════════════════════════════════════════════════════════════════════════════
//
// Input:  1 channel (microphone audio)
// Output: 3 channels
//   [0] audio_out   - TX: modulated FSK signal / RX: demodulated baseband
//   [1] confidence  - detection confidence [0, 1]
//   [2] soft_bit    - demodulated bit decision [-1, +1]
// ═══════════════════════════════════════════════════════════════════════════════

process = _ : router
with {
    // TX mode: ignore input, generate FSK from tx_bit parameter
    tx_out = fsk_modulate, 0.0, 0.0;

    // RX mode: demodulate input signal
    rx_out(sig) = sig : fsk_demodulate;

    // Mode router
    router(sig) = ba.if(mode < 0.5, tx_out, rx_out(sig));
};
