# web/bridge/custos/ — provenance

`custos.gen.c` is **generated**. Do not edit it. It is the C11 library unit
the Exsecutor compiler emits from three Exsecutor source files:

| | |
|---|---|
| Exsecutor repository | `github.com/ALH477/exsecutor` |
| Commit | `5569b55dd904ef198864c59271a23d563f189319` |
| Sources, in unit order | `tests/conformance/entry23_demodframe_golden_vectors.exsc` (the `DeModFrame` declaration), `tests/conformance/entry23/codex.exsc` (the codec certified by Exsecutor spec §14 entry 23 against the 246 golden vectors), `examples/custos/custos.exsc` (the datagram gate) |
| Command | `exsc aedifica --hospes x86_64-linux --emitte c <the three files> -o custos.gen.c` |
| Output | 31,948 bytes, sha256 `35a0cd8413bbbb5a6bdf7980d27e27ddd148a8c12d8123febc4a6d1f6585acaa` |

`regen.sh` re-emits the file from an Exsecutor checkout and compares the result
with this copy. Exsecutor's own `make reproduce` covers byte-identical emission
across different directories, locales, time zones and hostnames. Its
`examples/custos/proba_c.sh` checks anchors, double emission and five mutants.

The unit defines `exs_admitte`, `exs_lege` and `exs_redundantia_sarcinae`.
It imports only `exsrt_abortus`, which `shim.c` supplies. `build.rs` compiles
both files with the `cc` crate; `src/gate.rs` is the only caller.

## Licence

`custos.gen.c` is compiler output. Exsecutor's `LICENSE.EXCEPTION`, Exception A,
lets compiler output be propagated under terms of the recipient's choosing. It
ships here under this repository's LGPL-3.0-only, alongside `shim.c`. This is
the same reading `LICENSING.md` already records ("Exception A covers compiler
OUTPUT").

No Exsecutor *source* is copied into this directory. The three `.exsc` inputs
stay upstream under GPL-3.0-or-later. That is why the relicensing grant that
covers `exsecutor/` (compiler-input source) is not needed here.

To regenerate the file in-tree, or to edit the gate in this repository,
`custos.exsc` would need the same explicit grant `exsecutor/codex.exsc`
carries. Only the copyright holder can give that grant.
