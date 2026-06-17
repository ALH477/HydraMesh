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
- (tracked) Lisp wire-codec criticals are being folded from `lisp/hydramesh-hotfix.lisp` into
  source; see the repo legitimacy program.
