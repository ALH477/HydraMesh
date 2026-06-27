//============================== hydramodem_test.dsp ==========================
// Pure-Faust offline self-test (no C required). Generates an alternating-tone
// symbol pattern, modulates it (CPFSK), feeds the result straight into the
// tonebank, and outputs the N detector energies. Use it to eyeball that the
// correct detector lights up per symbol:
//
//   faust2plot faust/hydramodem_test.dsp && ./hydramodem_test -n 480 > det.txt
//   # or render to wav:
//   faust2jaqt / faust2alsa ... then inspect outputs 0..N-1
//
// Output 0 = energy of tone 0, output 1 = energy of tone 1. They should swap
// every symbol period (48 samples at the defaults).
//=============================================================================
declare name    "hydramodem_test";
declare version "0.1.0";
declare author  "DeMoD LLC";

import("stdfaust.lib");
m = library("demod_modem.lib");

N       = 2;
base    = 2000.0;
spacing = 1000.0;
baud    = 1000.0;
lpCut   = baud * 0.5;

symbolLen = int(float(ma.SR) / baud);          // samples per symbol

// Current symbol index: alternate 0,1,0,1,... one new symbol every symbolLen.
sym  = int(ba.time / symbolLen) % N;
freq = base + float(sym) * spacing;

process = m.cpfsk(freq) : m.tonebank(N, base, spacing, lpCut);
