# DCF backlog — deferred, non-critical items

The legitimacy program closed every compile-breaker and crasher (**C1–C9**) and gave every
advertised language a certified wire codec. The hardening pass (2026-07) then closed the
HIGH C SDK items and completed certification. The items below are what remains open.
Specifics and line references live in [`DCF_CODE_REVIEW.md`](DCF_CODE_REVIEW.md).

## DONE (2026-07 hardening pass)

- **C SDK — UDP receive truncation: FIXED.** All UDP receive paths now use MTU-sized buffers
  with `MSG_TRUNC` fail-closed detection (`plugins/{DOOM_,dcf_}udp_transport.c`,
  `node/dcfnode.c`); `dcf_proto_serialize` gained a capacity parameter so fixed stack buffers
  can't overflow. Serial/namedpipe/unixsocket length-handling and unchecked-return bugs fixed.
- **C SDK — connection-pool + logging races: FIXED.** Lock-guarded `drain`/`get_stats`, atomic
  per-field stats, bounded graceful destroy, once-guarded log init. New `test_regressions`
  (C4/H2/C6) + `test_streamdb_regress` (C5) run under **ASan and TSan** in the `c-sdk-unit` CI
  job (the unit suite had never run in CI). TSan's first StreamDB run found and fixed a real
  race on the auto-flush `running` flag.
- **MEDIUM portability: FIXED** — `dcf_once` (pthread_once / InitOnceExecuteOnce), async-signal-
  safe crash handler (M4, already in source; pre-warm + short-write loop added), `strcasecmp`
  shim (M6), signed `recv` checks (M3). MSVC guards and `CLOCK_MONOTONIC` remain untested on
  those platforms but are coded.
- **Certification: Haskell / Kotlin / Swift / Lisp are Certified.** All four have ungated
  `certify-<lang>` CI jobs. Lisp was upgraded from an embedded vector subset to certifying the
  full 109 encode + 137 syndrome vectors (and the FEC set) by reading the canonical JSON via a
  small in-tree reader — still no Quicklisp.
- **Nix fake hashes: NONE REMAIN.** Go `vendorHash = null` (stdlib-only), nodejs rewritten
  without a lockfile hash, streamdb is C (not Rust). The earlier "fill the fakeHash" item is
  obsolete.

## Still open

- **Header→impl gap: mostly closed.** The six declaration-only modules are quarantined in
  `C_SDK/include/experimental/`; `dcf_interface.h`/`dcf_plugin_manager.h` moved there too.
  Reviving a real plugin *loader* (v2 `ITransport`) remains future work — today
  `DCF_BUILD_PLUGINS=ON` only compile-checks the v1 plugins.
- **Aspirational Nix SDK packages** still need their proto-gen build steps repaired.
- **`dcf-haskell`/`dcf-kotlin`/`dcf-swift` Nix derivations + devShell toolchains** do not exist
  (certification doesn't need them; only local pre-verification would).

## Release / infrastructure follow-ups

- Make `.github/workflows/wire-certify.yml` a **required status check** on `main` (a GitHub
  branch-protection setting, not a file change).
- Keep `.github/LOCAL_CI_RESULTS.md` refreshed at HEAD via `make ci-local` after cert changes.
