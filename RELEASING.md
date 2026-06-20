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
   `client/src-tauri/Cargo.toml`, `flake.nix`, `python/pyproject.toml`, and the
   per-binding manifests (`JS/nodejs/package.json`, `haskell/dcf-wire.cabal`,
   `perl/lib/DCF/Frame.pm`, `kotlin/build.gradle.kts`, …).
4. Tag and push:
   ```sh
   git tag -a vX.Y.Z -m "DCF vX.Y.Z"
   git push origin vX.Y.Z
   ```
5. Create the GitHub release from the tag, pasting the `CHANGELOG.md` section as the notes.

## Publishing to language registries

These push to public registries (irreversible) and need credentials — they are **not**
run by CI. Publish from a green `main` at the tagged version; verify each package
*installs and runs* before uploading.

### Python — `demod-dcf` → PyPI (`python/`)

```sh
cd python
python3 -m build                       # dist/demod_dcf-<ver>-py3-none-any.whl + sdist
python3 -m twine check dist/*
python3 -m venv /tmp/v && /tmp/v/bin/pip install dist/demod_dcf-*.whl
/tmp/v/bin/dcf-sdr tx --text "DCF!" --mod gfsk --iq /tmp/d.cf32
/tmp/v/bin/dcf-sdr rx --iq /tmp/d.cf32 --mod gfsk        # must recover "DCF!"
python3 -m twine upload dist/*                            # needs a PyPI token
```

The SDR modem + reference codecs ship *under* the `dcf` package (`dcf.modem`,
`dcf.MCP`) so their sibling-relative `sys.path` resolution works the same from a source
checkout and an installed wheel — don't flatten them to top-level.

### Node.js — `@demod/dcf-wire` → npm (`JS/nodejs/`)

```sh
cd JS/nodejs
npm test                       # certify frame + superpack + fec vs the golden vectors
npm pack --dry-run             # confirm the tarball = src/{frame,superpack,fec}.js
npm publish --access public    # scoped pkg must be public; needs `npm login`
```

When you add a new certified module, **add it to the `files` allow-list** in
`package.json` (this is how `fec.js` was once shipped missing).

### Rust — crates.io (`rust/`, `codec/`)

`codec/` is intentionally `publish = false` (carries committed Faust-generated C,
consumed by path). `rust/` (`dcf-rust-sdk`) has crates.io-ready metadata but a path dep
on `codec`, so publishing the SDK needs `codec` published or vendored first. Dry-run:
`cd rust && cargo package --allow-dirty --no-verify`.

## First release

**v0.3.0** — every advertised language has a certified-or-CI-gated `DeModFrame` wire codec, and
the critical bugs C1–C9 are closed. See `CHANGELOG.md`.
