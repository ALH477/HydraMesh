# Licensing

This repository is **multi-licensed by scope**. When in doubt, the `SPDX-License-Identifier`
header at the top of a file is authoritative for that file. Files without one are covered by
[`REUSE.toml`](REUSE.toml), which records the holder and licence of every file in the tree
(`reuse lint` checks it in CI); the licence texts are in [`LICENSES/`](LICENSES/).

**Copyright.** DeMoD LLC holds the copyright in this repository, except for the contributions
credited in [`AUTHORS`](AUTHORS), whose authors keep the copyright in them (today: the Spanish
README, `README.es-ES.md`, contributed by webbrain-one under `LGPL-3.0-only`). Third-party
material keeps its own holders and licences; it is listed in the sections below.

## The library — LGPL-3.0-only

All linkable library code — the wire codec (`codec/`, `C_SDK/`), the SDKs (`rust/`, `python/`,
`lisp/`, and the other language bindings), and the tooling (`matrix-bridge/`, `client/`) — is
licensed under the **GNU Lesser General Public License v3.0 only** (`LGPL-3.0-only`). The full
text is in [`LICENSE`](LICENSE).

LGPL-3.0 lets you link DCF into proprietary applications, provided changes *to DCF itself* remain
under the LGPL and users can relink. This is the default for the whole tree.

### Why there are two licence texts

LGPLv3 is not a standalone licence. Its own first paragraph says it "incorporates the terms and
conditions of version 3 of the GNU General Public License, supplemented by the additional
permissions listed below" — so the LGPLv3 text alone (7.6 kB of *additional permissions*) is
incomplete without the GPLv3 it sits on top of. Both therefore ship here:

| File | Text | Role |
|---|---|---|
| [`LICENSE`](LICENSE) | GNU **Lesser** General Public License v3.0 | the licence this project grants |
| [`COPYING`](COPYING) | GNU General Public License v3.0 | the base terms LGPLv3 incorporates by reference |

`COPYING` does **not** mean any part of the library is GPL-licensed — the library is
`LGPL-3.0-only`, and the only GPL-scoped file in the repo is the DOOM example's README below. The FSF ships
these as `COPYING` + `COPYING.LESSER`; this repo keeps the LGPL text at `LICENSE` instead, because
that path is referenced by `CPACK_RESOURCE_FILE_LICENSE`, the per-language manifests and the
READMEs. Both files are verbatim FSF texts and must not be edited.

## The DOOM example — GPL-3.0

[`C_SDK/examples/DOOM/`](C_SDK/examples/DOOM/) is scoped **GPL-3.0**. It currently holds only
a README (`README.markdown`) describing a DOOM networking mod; no DOOM engine code and no mod
source is in this tree. The GPL scope is for the mod that README describes, which would link
id Software's GPL-licensed DOOM source (linuxdoom-1.10). This is the *only* GPL-scoped part of
the repository; it is an example, not part of the linkable library, and does not affect the
license of anything else.

## The Lua framework — dual-licensed

The Lua DCF-Audio binding ([`lua/`](lua/), see [`lua/LICENSING.md`](lua/LICENSING.md)) is
**dual-licensed**: `LGPL-3.0-only` for open-source use, or a commercial license available from
DeMoD LLC on request. Dual-licensing is currently **scoped to Lua only**. DeMoD LLC holds the
copyright in the Lua framework, and may extend dual-licensing to other components whose
copyright it holds (everything except the contributions credited in [`AUTHORS`](AUTHORS)); until
then, the rest of the tree is LGPL-3.0-only.

## DCF-JANUS — GPL-3.0 boundary (subprocess only)

The DCF `janus:` transport ([`python/dcf/transport.py`](python/dcf/transport.py),
[`Documentation/DCF_JANUS_SPEC.md`](Documentation/DCF_JANUS_SPEC.md)) interoperates with the
NATO STANAG-4748 standard by invoking the **GPL-3.0** janus-c reference (`janus-tx`/`janus-rx`)
as a **separate subprocess** — mere aggregation, exactly like the existing `pw-play`/`ffmpeg`
calls. janus-c is **never vendored or linked** into this `LGPL-3.0-only` tree; it is an
**optional, user-installed GPL dependency** (built by a standalone Nix derivation,
`nix build .#janus-c`, kept out of every LGPL package's closure). The transport raises (and
its tests skip) when janus-c is absent, so the LGPL library never depends on GPL code.

## DCF-Snake / quanta — GPL-3.0 boundary (subprocess only)

The DCF-Snake record plane ([`Documentation/DCF_SNAKE_SPEC.md`](Documentation/DCF_SNAKE_SPEC.md))
carries the DeMoD **quanta** codec by invoking the **GPL-3.0-only** (dual-licensed
`GPL-3.0-only OR DeMoD-Commercial`) `quanta-stream` / `quanta-stream-decode` binaries
(from the separate DeMoD `quanta` repository) as **separate subprocesses** — mere aggregation, exactly
like the existing janus-c and `pw-play`/`ffmpeg` calls. quanta is **never vendored or linked**
into this `LGPL-3.0-only` tree; it is an **optional, standalone GPL dependency** (built by its
own Nix derivation, `nix build .#quanta`, kept out of every LGPL package's closure —
`flake.nix`). The mixer/spoke nodes shell out to the `quanta-stream`/`quanta-stream-decode`
binaries at runtime (`$QUANTA_STREAM`/`$QUANTA_STREAM_DECODE`), so the LGPL library never
depends on GPL code.

## Exsecutor — GPL-3.0-or-later + §7 exceptions, vendored in the *other* direction

[Exsecutor](https://github.com/ALH477/exsecutor) (`exsc` compiler; same author,
DeMoD LLC) implements the DeModFrame wire codec as a conformance entry
(`entry23`) and certifies it against `Documentation/golden_vectors.json`. See
[`Documentation/DCF_EXSECUTOR.md`](Documentation/DCF_EXSECUTOR.md) for the full
binding writeup.

Exsecutor is licensed **GPL-3.0-or-later** with §7 additional permissions
(`LICENSE.EXCEPTION`, v2.0): **Exception A** covers *compiler output* — "You have
permission to propagate Compiler Output under terms of your choosing," modelled on
the GCC Runtime Library Exception, the FAUST notice, and the Bison parser
exception — so anything `exsc` compiles (including the entry-23 codec) carries no
GPL propagation obligation of its own. **Exception B** is a per-file exception
covering designated runtime files linked into a compiled program.

### The relicensed codec — an exception to the exception

The codec itself now lives in this tree, at [`exsecutor/`](exsecutor/):
`demodframe.exsc` (the `DeModFrame` declaration), `codex.exsc` (the codec),
`probatio.exsc` (the driver) and `expecta.py` (the comparator). **Those files are
`LGPL-3.0-only`**, by an explicit additional grant recorded in each file's header.

DeMoD LLC is the **sole copyright holder of Exsecutor**, and so of these files,
which originate there; a sole copyright holder may license their own work under
more than one licence. (In Punctim, DeMoD LLC holds the copyright in everything
except the contributions credited in [`AUTHORS`](AUTHORS), none of which touches
`exsecutor/`. The headers of those files say "sole copyright holder of both
Exsecutor and Punctim" and are left exactly as granted; the Punctim half of that
phrase is broader than the facts, but the grant rests only on DeMoD LLC's
copyright in the files themselves.) So
this is dual-licensing of specific files, not a conversion: the Exsecutor
originals (`tests/conformance/entry23/…`) remain `GPL-3.0-or-later`, and **no
other Exsecutor source is relicensed by implication.**

A new grant was needed because neither existing exception reaches this case.
Exception A covers compiler **output**; Exception B covers designated **runtime
files** linked into a compiled program. Compiler *input* source — which is what
`codex.exsc` is — falls under neither. Hence a third, deliberately narrow
carve-out, scoped to exactly the files in `exsecutor/`: **an exception to the
exception.**

### The compiler stays outside

`exsc` itself is **not** relicensed and is **never linked**. It is a
`GPL-3.0-or-later` compiler invoked as a **separate process** by
`exsecutor/certify.sh`, built from the pinned upstream flake as `nix build .#exsc`
and kept out of every LGPL closure — precisely the janus-c and quanta boundary
above. Exception A independently guarantees that whatever `exsc` emits carries no
GPL obligation, so both the input (by the grant above) and the output (by
Exception A) are unencumbered.

Data still crosses in the other direction too: Exsecutor vendors this repo's
`golden_vectors.json` and `WIRE_QUANTUM_SPEC.md` as read-only reference material
with sha256 provenance recorded upstream.

The `certify-exsecutor` CI job (`.github/workflows/wire-certify.yml`) therefore
does three things: it certifies the **in-tree** codec against this repo's live
`Documentation/golden_vectors.json` (246/246, the gate), it warns if the in-tree
files drift from the Exsecutor originals, and it runs the upstream suite for the
mutation and purity checks this repo does not reproduce.

## HydraModem — LGPL-3.0-only

The [`hydramodem/`](hydramodem/) directory is a self-contained acoustic M-FSK modem that carries
the 17-byte `DeModFrame` *opaquely* (a transport beneath the wire quantum). It originated as a
standalone Apache-2.0 release; on integration into this monorepo DeMoD LLC — which holds the
copyright in it — **relicensed it to `LGPL-3.0-only`**, consistent with the rest of the tree
(`hydramodem/LICENSE`, `hydramodem/NOTICE`). Repo-specific glue under `hydramodem/dcf-tools/`
carries the standard `LGPL-3.0-only` SPDX header.

## Faust-generated code

The Faust DSP sources (`codec/faust/*.dsp`, `hydramodem/faust/`, `python/modem/*.dsp`) are
DeMoD LLC's, `LGPL-3.0-only`. The Faust compiler inlines code from the Faust standard
libraries (GRAME and contributors, not vendored here) into the C it generates, and records
each library's licence in the generated metadata. In the committed outputs
(`codec/faust/dcf_pm_faust.c`, `codec/faust/dcf_rf_modulator.gen.c`) that is `maths.lib`,
"LGPL with exception": LGPL-2.1-or-later plus GRAME's exception, which lets the compiled code
be distributed under a licence of the distributor's choosing. Those files are therefore
`LGPL-3.0-only` like their sources. What the HydraModem Faust backend pulls in is recorded in
[`hydramodem/NOTICE`](hydramodem/NOTICE).

## StreamDB — LGPL-2.1-or-later

The embedded database the Lisp SDK loads ([`lisp/streamdb/`](lisp/streamdb/): `streamdb.c`,
`streamdb.h`, `libstreamdb_wrapper.c`, `libstreamdb_wrapper.h`) is DeMoD LLC's and is licensed
**LGPL-2.1-or-later**, as its file headers state (text: [`LICENSES/LGPL-2.1-or-later.txt`](LICENSES/LGPL-2.1-or-later.txt)).
It is not GPLv3, which older README text said.

## Training data — an Apache-2.0 option on three files

[`TRAINING_DATA_LICENSE`](TRAINING_DATA_LICENSE) additionally offers **Apache-2.0** for
exactly the three JSONL files it names — `punctim_wire.jsonl`, `punctim_adapter.jsonl` and
`punctim_certify.jsonl` — "specifically for machine learning model training purposes". Those
three are therefore `LGPL-3.0-only OR Apache-2.0`. The other JSONL files at the root
(`punctim_unified.jsonl` and the `*_qwen.jsonl` variants) are not named in it and are
`LGPL-3.0-only` like the rest of the tree.

## The GUI review page — embedded third-party code and fonts

[`GUI/Punctim-Comms-Review.html`](GUI/Punctim-Comms-Review.html) is a single-file bundle. Its
own markup, styles and scripts are DeMoD LLC's (`LGPL-3.0-only`); it also embeds, verbatim,
React and ReactDOM 18.3.1 and @babel/standalone 7.29.0 (MIT, the latter bundling packages under
MIT, ISC, BSD-2-Clause and CC-BY-4.0), and woff2 subsets of the Inter and JetBrains Mono fonts
(SIL OFL 1.1). Holders, licences and the required notices are in
[`GUI/THIRD_PARTY_NOTICES.md`](GUI/THIRD_PARTY_NOTICES.md).

## Export compliance

DCF is **encryption-free by design** to remain outside EAR/ITAR licensing requirements; see
[`Documentation/Specs/export_compliance.markdown`](Documentation/Specs/export_compliance.markdown).
Do not add cryptography to the core wire path.

## Minecraft — `minecraft/` (datapack, Paper plugin, Fabric mod, sidecar)

Everything under `minecraft/` and `python/dcf/minecraft/` is `LGPL-3.0-only` like the rest of
the linkable tree, and the plugin/mod jars embed only `com.demod.dcf.*` code. They are compiled
**against** third-party APIs that are never vendored or redistributed here: the Paper API
(MIT; the Paper *server* it loads into carries GPL-3.0 lineage, which LGPL-3.0 code is
compatible with either way), Fabric Loader / Fabric API (Apache-2.0), and Mojang's Minecraft
itself (proprietary, EULA — never copied, downloaded only by the user's own launcher/server).
The datapack is data (`.mcfunction` text) generated by our LGPL generator. The optional
Mineflayer bot (`minecraft/tools/bot`) is an `npm install` at the user's option, not vendored,
and runs as a separate process — the janus-c/quanta boundary again.

## SPDX headers

Every source file should carry an SPDX header matching its scope:

- Library / SDK / tooling: `SPDX-License-Identifier: LGPL-3.0-only`
- `C_SDK/examples/DOOM/`: `SPDX-License-Identifier: GPL-3.0-only`
- `lisp/streamdb/`: `SPDX-License-Identifier: LGPL-2.1-or-later`

A file that cannot carry a header (generated code, golden vectors, binaries, JSON, anything a
test hashes) is annotated in [`REUSE.toml`](REUSE.toml) instead; never add header lines to it.

Copyright © DeMoD LLC.
