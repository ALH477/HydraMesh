# DCF backlog — deferred, non-critical items

The legitimacy program closed every compile-breaker and crasher (**C1–C9**) and gave every
advertised language a certified-or-CI-gated wire codec. The items below are **HIGH / MEDIUM
severity but non-blocking** — tracked here and deliberately deferred. Specifics and line
references live in [`DCF_CODE_REVIEW.md`](DCF_CODE_REVIEW.md).

## HIGH

- **C SDK — UDP transport truncates payloads > 512 B.** The receive path uses a fixed small
  buffer; large frames are silently cut. Needs a length-prefixed or MTU-sized buffer.
- **C SDK — connection-pool race.** Health/eviction interaction has a window flagged in the
  review; audit under TSan and tighten the locking.
- **Lisp SDK — HIGH fixes F2 / F3 / F5 / F6 / F7 / F9: DONE.** Folded from the hotfix into
  `lisp/src/hydramesh.lisp` (v0.3.x); `hydramesh-hotfix.lisp` is now a thin compatibility shim.

## MEDIUM (portability)

- MSVC build guards; `CLOCK_MONOTONIC` fallback; signed `recv` length checks; async-signal-safe
  crash handler; log-before-init ordering; `strcasecmp` portability.

## Release / infrastructure follow-ups

- Make `.github/workflows/wire-certify.yml` a **required status check** on `main` (a GitHub
  branch-protection setting, not a file change).
- `flake.nix`: replace the placeholder hashes (`sha256-0000…` `vendorHash` / `cargoHash` /
  `npmDepsHash`) with real values and add `meta.license` to every derivation so `nix build`
  works end-to-end. (The `c_sdk` → `C_SDK` path case is fixed.)
- **B3** regression tests for the already-fixed crashers **C4/C5/C6** under ASan/TSan, so they
  cannot silently return.
- Flip **Haskell / Kotlin / Swift / Lisp** from the Design tier to Certified once their
  `certify-<lang>` jobs run green on a runner with the toolchain.
