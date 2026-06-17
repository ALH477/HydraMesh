# Releasing

**Versioning.** Single monorepo SemVer, currently `0.x` (pre-1.0). `1.0.0` is reserved for when
the full advertised language set is green in CI. The per-component manifests (`codec/Cargo.toml`,
`rust/Cargo.toml`, `client/src-tauri/Cargo.toml`, `flake.nix`, and the per-binding manifests)
track the same umbrella version.

## Cutting a release

1. Ensure `wire-certify.yml` is green on `main` (all `certify-*` jobs).
2. Update `CHANGELOG.md`: move the `[Unreleased]` items under a new
   `## [X.Y.Z] - YYYY-MM-DD` heading, and refresh the compare/tag links at the bottom.
3. Bump the version string in `codec/Cargo.toml`, `rust/Cargo.toml`,
   `client/src-tauri/Cargo.toml`, `flake.nix`, and the per-binding manifests
   (`JS/nodejs/package.json`, `haskell/dcf-wire.cabal`, `perl/lib/DCF/Frame.pm`,
   `kotlin/build.gradle.kts`, …).
4. Tag and push:
   ```sh
   git tag -a vX.Y.Z -m "DCF vX.Y.Z"
   git push origin vX.Y.Z
   ```
5. Create the GitHub release from the tag, pasting the `CHANGELOG.md` section as the notes.

## First release

**v0.3.0** — every advertised language has a certified-or-CI-gated `DeModFrame` wire codec, and
the critical bugs C1–C9 are closed. See `CHANGELOG.md`.
