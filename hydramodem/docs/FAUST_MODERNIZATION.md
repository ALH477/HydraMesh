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

## Current state — pinned to Faust 2.72.14

The Faust backend builds and passes `make faust-check` (clean loopback, full AWGN
sweep, timing recovery) on **Faust 2.72.14**, pinned via the `nixpkgs-faust`
input (nixos-24.05) in the repo `flake.nix`. This is the nearest binary-cached
release to the **2.70.x** the adapters were written against (`BUILD.md` says
"2.70.x or newer"); the 2.70–2.74 series share the same `-os` (one-sample) ABI.

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

## Why not Faust 2.85 (the version on current dev machines)

Faust **>= 2.85** breaks the backend in two independent ways. Both were observed
with Faust 2.85.5:

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

## Modernization plan → Faust 2.85

Target: the Faust backend builds and stays byte-identical to the C reference on
current Faust, without losing the 2.72 path until the new one is proven.

1. **Adapt to the new `-os` ABI.** Update `hydra_dsp_faust_{tx,rx}.c` to the
   2.85 `compute(dsp, count, inputs, outputs)` shape, folding the control-rate
   precompute (formerly `control<class>()` + separate `iControl/fControl`) into
   the new calling convention. Keep the TX warm-up that lets `si.smoo` /
   `fi.dcblocker` settle before the preamble.
2. **Resolve `-ftz`.** Check whether a 2.85 point release fixes the bitcast; if
   not, drop `-ftz 2` (the reference DSP does not flush denormals and is the
   numeric reference anyway) or substitute a supported denormal flag, and confirm
   no audible/loopback difference.
3. **Re-validate.** `make faust-check` green on 2.85, and a byte-diff of Faust vs
   reference output over the loopback vectors. The C reference remains the
   certified path; the Faust build must match it, not the other way around.
4. **Make the adapter version-tolerant** (optional): detect the `-os` signature
   at build time (a configure probe or a small `#if` on `FAUSTVERSION`) so one
   adapter supports both the 2.70–2.74 and >=2.85 ABIs.
5. **Bump the pin.** Once the adapter supports 2.85, move `nixpkgs-faust` to a
   nixpkgs that ships it (or drop the pin and use the default `nixpkgs`), and
   delete this interim note's "pinned to 2.72.14" framing.

Until step 5 lands, the pin is the supported configuration and the C reference is
the default everywhere else.
