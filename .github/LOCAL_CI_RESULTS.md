# Local CI results — `wire-certify.yml` run off-platform

The Wire Certification workflow (`.github/workflows/wire-certify.yml`) was executed
**locally**, job-for-job, instead of on GitHub Actions. Toolchains not installed on
the host were supplied hermetically with Nix (`nix shell nixpkgs#…`), exactly the
versions CI would provision. This file is a dated attestation, not a substitute for
the hosted run.

- **Date:** 2026-06-18
- **Commit:** `394731f`
- **Host toolchains:** Python 3.13, Rust 1.92 (cargo), GCC 15.2, Go 1.25, Perl 5.42, Faust 2.83
- **Nix:** Determinate Nix 2.33 (supplied node, lua5.4, sbcl, jdk, kotlin, ghc)

## Result: 15 PASS · 1 SKIP · 0 FAIL

| Job | Status | How |
|-----|--------|-----|
| certify-python | ✅ PASS | host python3 (wire laws, sdk/MCP self-test, superpack + modulation regen+diff) |
| certify-rust | ✅ PASS | host cargo (`cargo test --tests`: certify, superpack, modulation, audio, game, text) |
| certify-c | ✅ PASS | host gcc (wire/superpack/modulation/audio/game/text) + cmake node ctests (proto, modulation, modem) |
| certify-cpp | ✅ PASS | host g++ (certify + certify_superpack) |
| certify-go | ✅ PASS | host go (`go vet`, `go test ./...`, `go test -race ./node/`) |
| certify-perl | ✅ PASS | host perl (`make test`: wire + superpack) |
| certify-audio | ✅ PASS | host python+gcc (regen+diff vectors, C audio cert) |
| certify-game | ✅ PASS | host python+gcc (regen+diff vectors, C game cert) |
| certify-text | ✅ PASS | host python+gcc (regen+diff vectors, C text cert) |
| certify-node | ✅ PASS | nix nodejs (certify.js + certify_superpack.js) |
| certify-lua | ✅ PASS | nix lua5.4 (selftest, jam smoke, dcf_superpack) |
| certify-lisp | ✅ PASS | nix sbcl (`--load lisp/src/wire.lisp`, self-certs wire + superpack) |
| certify-java | ✅ PASS | nix jdk (Certify + SuperPackCertify) |
| certify-kotlin | ✅ PASS | nix kotlin+jdk (Frame+SuperPack+Certify → run) |
| certify-haskell | ✅ PASS | nix ghcWithPackages (Certify.hs: wire + syndrome + superpack) |
| certify-swift | ⏭️ SKIP | the Nix Swift-on-Linux wrapper omits the `swift-test` subcommand; this cert needs the `swift-actions` runner. Not a code failure. |

## Note

Running the workflow locally also surfaced a real bug in the workflow itself: the
`certify-c` job invoked `ctest` on `modulation_certify` without building that
target (it would have reported *Not Run* on GitHub). Fixed in the same workflow file.
