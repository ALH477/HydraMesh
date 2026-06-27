# HydraModem — Faust toolchain pinning & modernization plan

HydraModem's DSP is **authored in Faust** — `faust/hydramodem_tx.dsp`,
`faust/hydramodem_rx.dsp`, and `faust/demod_modem.lib` are the *normative*
definition of the CPFSK modulator and the quadrature down-conversion bank. There
are two backends behind `src/hydra_dsp.h`:

- **`hydra_dsp_ref.c`** — portable C, the default `make` build, a 1:1 hand-port of
  `demod_modem.lib`. This is the certified/ground-truth path and what the test
  suite and the hardware cable runs exercise.
- **`hydra_dsp_faust_{tx,rx}.c`** — thin adapters over `faust -lang c -os` output,
  the deployment path (`make faust`). Byte-identical to the reference in loopback.

## Current state — version-tolerant (Faust 2.72 – 2.85)

The Faust backend builds and passes `make faust-check` (clean loopback, full AWGN
sweep, timing recovery) on **Faust 2.72.14, 2.83.1, and 2.85.5** from one source
(see "Modernization → Faust 2.85 — DONE" below). The hermetic
`nix build .#hydramodem-faust` uses a **pinned Faust 2.72.14** (via the
`nixpkgs-faust` input, nixos-24.05) purely for a *reproducible, binary-cached*
build — not because newer Faust is unsupported.

Build it hermetically or interactively:

```sh
nix build .#hydramodem-faust          # builds + runs faust-check in the sandbox
nix develop .#hydramodem-faust        # FAUST/FAUST_INC exported; then:
cd hydramodem && make faust-check     # compiled Faust DSP, loopback must pass
```

(Exact Faust **2.70.3** is not in any cached nixpkgs and would be a from-source
LLVM build; 2.72.14 is the reproducible, cache-backed stand-in with the same ABI.)

### Verified equivalence (Faust backend vs C reference)

The compiled-Faust backend is not just "builds" — it is equivalent to the certified
C reference:

- **Loopback:** `make faust-check` passes the full clean/AWGN/clock-offset suite.
- **Cross-decode:** a frame modulated by the Faust backend decodes on the reference
  RX and vice versa — 8/8 both ways.
- **Waveform:** the shared warm-up tone is byte-identical; modulated frames carry the
  same tones (2000/3000 Hz) and differ only in carrier *phase*, which the
  non-coherent integrate-and-dump detector ignores by design.
- **Over the cable:** the Faust backend ran A→B on the two-interface rig at
  **200/200 frames, PER 0.00%** — identical to the reference run.

## What broke on Faust >= 2.83 (historical — now handled)

Before the modernization below, Faust **>= 2.83** broke the backend in two
independent ways (observed on 2.83.1 and 2.85.5):

1. **`-ftz 2` codegen bug.** The denormal-flush idiom emits malformed C — the
   `int64_t` bit-cast of the sample is mis-parenthesized:
   ```c
   dsp->fRec1[0] = ((*((int64_t*(&fTemp1) & 9218868437227405312) ? fTemp1 : 0.0);
   ```
   which does not compile.

2. **`-os` ABI change.** 2.72 generates the signature the adapter calls:
   ```c
   void controlhydratx(hydratx*, int* iControl, double* fControl);
   void computehydratx(hydratx*, FAUSTFLOAT* in, FAUSTFLOAT* out,
                       int* iControl, double* fControl);
   ```
   2.85 changed `-os` to the unified `compute(dsp, count, inputs, outputs)` form
   (control folded in), so `hydra_dsp_faust_{tx,rx}.c`'s 5-arg calls no longer
   match.

## Modernization → Faust 2.85 — DONE

The backend is now **version-tolerant across Faust 2.72 – 2.85** from a single
source. Verified green (`make faust-check`, full clean/AWGN/clock-offset suite)
on **2.72.14, 2.83.1, and 2.85.5**.

What changed:

1. **Version-tolerant adapters.** `hydra_dsp_faust_{tx,rx}.c` pick the call
   convention at compile time off a macro the generated file emits only in the
   old ABI:
   ```c
   #if defined(FAUST_REAL_CONTROLS)      /* <= 2.74 */
       controlhydratx(dsp, ic, fc);  computehydratx(dsp, &in, &out, ic, fc);
   #else                                 /* >= 2.83 */
       framehydratx(dsp, &in, &out);     /* control folded in; no control fn */
   #endif
   ```
   The control arrays and the `control*()` call exist only on the old path; the
   TX warm-up (settling `si.smoo` / `fi.dcblocker`) is preserved on both.
2. **Dropped `-ftz 2`.** The Makefile no longer passes it — it produced the
   malformed bit-cast on >= 2.83, and the C reference doesn't flush denormals
   (it is the numeric reference), so removing it keeps TX/RX byte-equivalent and
   builds cleanly on every version. (No supported per-version denormal flag is
   needed at 48 kHz voice-band.)
3. **Re-validated.** Loopback green on all three Faust versions; the Faust TX is
   tone-for-tone identical to the C reference and cross-decodes with it both ways
   (see "Verified equivalence" above). The C reference remains the certified path.

### Remaining (optional) follow-ups

- **Bump / drop the pin.** `nixpkgs-faust` (2.72.14) is now kept only for a
  *reproducible, binary-cached* hermetic build — no longer a necessity. It can be
  bumped to a nixpkgs shipping a newer Faust, or dropped in favour of the default
  `nixpkgs`, whenever a cached modern Faust is preferred.
- If a future Faust changes the `-os` ABI again, extend the `#if` with the next
  discriminator macro; the seam is already in place.
