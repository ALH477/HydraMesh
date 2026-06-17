# Changelog

All notable changes to this project are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the project aims to follow
[Semantic Versioning](https://semver.org/spec/v2.0.0.html) once tagged releases begin.

## [Unreleased]

### Added
- `LICENSING.md`, `SECURITY.md`, `AUTHORS`, and this `CHANGELOG.md` (repo legitimacy program,
  Stage A).
- DCF-Text: a certified text adapter over `DeModFrame` (`python/MCP/textlab_core.py`,
  `Documentation/text_vectors.json`) shared by the agent-to-agent mesh.
- Multi-agent client integration: `matrix-bridge/a2a.py` (`config`/`send`/`recv`),
  per-agent MCP config generator, an OpenClaw ClawHub skill
  (`matrix-bridge/agents/openclaw/`), a non-MCP CLI bridge, and a shared HTTP mesh service;
  see `Documentation/AGENT_CLIENTS.md`.

### Changed
- `Documentation/Specs/export_compliance.markdown`: corrected the document license label from
  GPL-3.0 to LGPL-3.0 (the framework is LGPL-3.0; GPL-3.0 is scoped to `C_SDK/examples/DOOM/`).
- Expanded `.gitignore` to cover C/JVM/Node/Lisp/Swift build artifacts and editor/OS cruft.
- `mesh_mcp.py`: the HTTP transport port is now configurable (`DCF_MCP_HTTP_PORT` / `http [PORT]`
  / `--port`), default 8765 preserved.

### Removed
- An offensive line in `lisp/README.md` acknowledgements.

### Fixed
- Lisp wire-codec criticals folded from `lisp/hydramesh-hotfix.lisp` into source
  (`lisp/src/hydramesh.lisp`): **C7** the vacuous `crc16-ccitt` (a shadowing inner `let*`
  discarded every iteration, so it returned `#xFFFF` for all input and wire validity was
  vacuous), **C8** the `dcf-stop` use-after-free (it freed StreamDB then called `save-state`
  on the dangling pointer), and **C9** the `collect-streamdb-results` ABI (it walked a
  pointer array, but `streamdb_prefix_search` returns a `Result*` linked list). Added a
  dependency-free `lisp/src/wire.lisp`, a load-time self-cert in `hydramesh.lisp`, and the
  `certify-lisp` CI job so the CRC can never silently regress.
