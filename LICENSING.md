# Licensing

This repository is **multi-licensed by scope**. When in doubt, the `SPDX-License-Identifier`
header at the top of a file is authoritative for that file.

## The library — LGPL-3.0-only

All linkable library code — the wire codec (`codec/`, `C_SDK/`), the SDKs (`rust/`, `python/`,
`lisp/`, and the other language bindings), and the tooling (`matrix-bridge/`, `client/`) — is
licensed under the **GNU Lesser General Public License v3.0 only** (`LGPL-3.0-only`). The full
text is in [`LICENSE`](LICENSE).

LGPL-3.0 lets you link DCF into proprietary applications, provided changes *to DCF itself* remain
under the LGPL and users can relink. This is the default for the whole tree.

## The DOOM example — GPL-3.0

The DOOM integration example under [`C_SDK/examples/DOOM/`](C_SDK/examples/DOOM/) is licensed
**GPL-3.0**, because it links GPL-licensed game code. This is the *only* GPL-scoped part of the
repository; it is an example, not part of the linkable library, and does not affect the license
of anything else.

## The Lua framework — dual-licensed

The Lua DCF-Audio binding ([`lua/`](lua/), see [`lua/LICENSING.md`](lua/LICENSING.md)) is
**dual-licensed**: `LGPL-3.0-only` for open-source use, or a commercial license available from
DeMoD LLC on request. Dual-licensing is currently **scoped to Lua only**. DeMoD LLC is the sole
copyright holder and may extend dual-licensing to other components in the future; until then,
the rest of the tree is LGPL-3.0-only.

## Export compliance

DCF is **encryption-free by design** to remain outside EAR/ITAR licensing requirements; see
[`Documentation/Specs/export_compliance.markdown`](Documentation/Specs/export_compliance.markdown).
Do not add cryptography to the core wire path.

## SPDX headers

Every source file should carry an SPDX header matching its scope:

- Library / SDK / tooling: `SPDX-License-Identifier: LGPL-3.0-only`
- `C_SDK/examples/DOOM/`: `SPDX-License-Identifier: GPL-3.0-only`

Copyright © DeMoD LLC.
