# Changelog

All notable changes to this project are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the project follows
[Semantic Versioning](https://semver.org/spec/v2.0.0.html). The project is pre-1.0; `1.0.0` is
reserved for when the full advertised language set is green in CI.

## [Unreleased]

## [0.3.0] - 2026-06-17

First tagged release — the repository legitimacy program. Every advertised language now has a
golden-vector-certified (or CI-gated) `DeModFrame` wire codec, every known critical bug is
closed (C1–C9), and the documentation advertises only what CI backs.

### Added
- **Wire-codec bindings across the whole language set.** From-scratch 17-byte `DeModFrame`
  codecs, each byte-identical to the Python reference and certified against all 246 golden
  vectors, with a `certify-<lang>` CI job in `wire-certify.yml` and `SPDX` headers:
  - **Certified** (verified locally + CI): **Go** (`go/dcf/`), **Java** (`java/com/demod/dcf/`),
    **Node.js** (`JS/nodejs/`), **Perl** (`perl/`), **C++** (`cpp/include/dcf/`) — joining the
    existing C / Rust / Python / Lua.
  - **Design** (written + CI-gated): **Haskell** (`haskell/`, made to compile + a cabal cert),
    **Kotlin** (`kotlin/`, a Gradle module with a UDP `DcfNode.kt`), **Swift** (`swift/`, a real
    SwiftPM package), and a dependency-free Lisp `lisp/src/wire.lisp`.
- DCF-Text: a certified text adapter over `DeModFrame` (`python/MCP/textlab_core.py`).
- Multi-agent client integration: `matrix-bridge/a2a.py` (`config`/`send`/`recv`), a per-agent
  MCP config generator, an OpenClaw ClawHub skill, a non-MCP CLI bridge, and a shared HTTP mesh
  service (see `Documentation/AGENT_CLIENTS.md`).
- Governance/licensing docs: `LICENSING.md`, `SECURITY.md`, `AUTHORS`, this `CHANGELOG.md`,
  `Documentation/DCF_BACKLOG.md`, and `RELEASING.md`.
- `SPDX-License-Identifier: LGPL-3.0-only` headers across the certified core (`codec/`, `C_SDK/`,
  `python/MCP/`, `rust/`) and every new binding.
- README **Language status** table (Certified / Design / Experimental) that tracks CI reality,
  plus a wire-certify CI badge.

### Changed
- Truth-in-advertising pass on `README.md`, `CLAUDE.md`, `C_SDK/README.md`: replaced
  "11 production-ready bindings" / "5.2.0" with the tiered status; flagged Dijkstra / TUI / AUTO
  / AI-topology as planned; scoped the DCF-Audio "byte-certified" claim to L2 framing + PCM-diag
  bytes + PM param layout (Opus output and PM synthesis audio are **not** byte-certified);
  replaced non-compiling C client examples.
- `Documentation/Specs/export_compliance.markdown`: license label GPL-3.0 → LGPL-3.0.
- Reconciled component versions to `0.3.0` (`codec/`, `rust/`, `client/`, `flake.nix`).
- Fixed the `flake.nix` C-SDK source path case (`c_sdk` → `C_SDK`).
- `mesh_mcp.py`: configurable HTTP transport port (`DCF_MCP_HTTP_PORT`); default 8765 preserved.
- Expanded `.gitignore` (C/JVM/Node/Lisp/Swift/CMake build artifacts).

### Removed
- An offensive line in `lisp/README.md` acknowledgements.
- Non-functional stubs replaced by real codecs: `swift/package.swift` (empty, mis-cased) and the
  Kotlin gRPC sketch `Networking.kt`.
- `C_SDK/c_sdk_fixes.patch` — redundant; its C1–C6 fixes are already applied in source.

### Fixed
- Lisp wire-codec criticals folded from `lisp/hydramesh-hotfix.lisp` into source
  (`lisp/src/hydramesh.lisp`): **C7** the vacuous `crc16-ccitt` (a shadowing inner `let*` made it
  return `#xFFFF` for all input, so wire validity was vacuous), **C8** the `dcf-stop`
  use-after-free, and **C9** the `collect-streamdb-results` `Result*` linked-list ABI; plus a
  load-time self-cert and the `certify-lisp` CI job so the CRC can never silently regress.
- Haskell `FrameSpec.hs` no longer fails to compile (ambiguous `seq`, missing `xor`/`Bits`
  imports, and an `Integral`-less `hi`/`lo` signature).

[Unreleased]: https://github.com/ALH477/HydraMesh/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/ALH477/HydraMesh/releases/tag/v0.3.0
