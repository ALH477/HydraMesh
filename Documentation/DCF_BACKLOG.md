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
- **Lisp SDK — HIGH fixes still living only in `lisp/hydramesh-hotfix.lisp`** (the criticals
  C7–C9 are folded into source; these are not yet):
  - **F2** RTT pong restamping — a pong stamped the responder's clock, so cross-process RTT was
    garbage. Pong must echo the ping's timestamp.
  - **F3** `stop-udp-endpoint` shutdown-hang — it joined a receiver parked in `socket-receive`
    *before* closing the socket. Close first.
  - **F5** `load-config` used `GETF` on the alist `cl-json` returns, so every config key was
    silently ignored and defaults always used.
  - **F6** `save-state`/`restore-state` peer-persistence shape.
  - **F7** `dcf-db-insert` input hardening (string | octet-vector | JSON-encoded).
  - **F9** `dcf-benchmark` `0`-is-truthy RTT guard.
  Until folded, load `hydramesh-hotfix.lisp` after the SDK to pick these up.

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
