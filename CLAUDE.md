# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

A polyglot monorepo for **DCF / HydraMesh** (DeMoD Communication Framework) — a
handshakeless, encryption-free, export-control-compliant mesh protocol. The same
protocol is implemented as bindings/SDKs across C, C++, Rust, Go, Python, Perl,
Java/Kotlin, Swift, Node.js, Haskell, and Common Lisp, each in its own top-level
directory.

The gravitational center of current work is narrow: the **wire quantum** and its
cross-language certification, now extended with a certified **audio** layer.
Read these first — they are normative:
- `Documentation/WIRE_QUANTUM_SPEC.md` — the 17-byte `DeModFrame` wire format.
- `Documentation/DCF_AUDIO_SPEC.md` — collaborative audio as an adapter over it.
- `Documentation/DCF_GAME_SPEC.md` — multiplayer game state/events as an adapter
  over it (same fragmentation scheme as audio, on `DATA` frames).
- `Documentation/DCF_CODE_REVIEW.md` — frank, module-by-module status (consult
  before trusting any module's surface area).

## The wire quantum (the one invariant)

One wire format: the 17-byte `DeModFrame` (version nibble = 1). Layout:
`sync(0xD3) | flags[ver|type] | seq | src | dst | payload(4B) | ts24 | crc16`.
Valid iff sync byte + version nibble + CRC-16/CCITT-FALSE over bytes `[0..14]`.
Everything else on the wire is an **adapter** over this quantum. CRC anchors:
`CRC("123456789")=0x29B1`, `CRC(0^15)=0x4EC3`.

Reference codecs (must stay byte-identical):

| Lang | File | Entry points |
|------|------|--------------|
| C | `codec/demod_frame.h` | `dcf_frame_encode/decode`, `dcf_crc16` |
| Rust | `codec/frame.rs`, `codec/src/lib.rs` | `Frame::encode/decode` |
| Python | `python/MCP/wirelab_core.py` | `encode`/`decode`/`crc16_ccitt`/`syndrome` |
| Lua | `GUI/wirelab.lua` | `encode`/`decode`/`crc16` (self-certs on load) |
| Haskell | `haskell/src/DCF/Transport/FrameSpec.hs` | `encodeFrame`/`decodeFrame` |
| Lisp | `lisp/src/hydramesh.lisp` (+ `lisp/hydramesh-hotfix.lisp`) | `encode-dcf-frame` |

### Certification is the contract

`Documentation/golden_vectors.json` is a 246-vector certificate (109 encode +
137 syndrome). Matching all 246 ≡ agreeing with the reference on the entire
input space. When you touch any codec, regenerate and diff:

```sh
python3 python/MCP/verify_laws.py /tmp/gv.json     # regenerate + verify laws
python3 python/MCP/certify_sdk.py --selftest
cd codec && cargo test --test certify              # Rust
```

CI: `.github/workflows/wire-certify.yml` runs Python/C/Rust certs on push/PR to
`main` and diffs regenerated vs committed vectors.

> Caveat: `GUI/MCP/*` are **dangling symlinks** into `python/MCP/` (broken in a
> fresh clone/CI) — use `python/MCP/` paths. `C_SDK/tests/test_wire_certify.c` is
> pre-existingly broken (calls `dcf_frame_crc`; the header exports `dcf_crc16`).

## DCF-Audio (collaborative audio over the wire)

A 20 ms codec block is an **adapter** over `DeModFrame`, serialised into
`1 + ceil(payload_len/4)` ordinary `CTRL` (type 3) frames. The L2 framing is
**codec-agnostic and byte-certified across C/Rust/Python**; `codec_id` lives in
the descriptor, so adding codecs never changes the vectors.

- `seq = packet_id[15:5] | frag_idx[4:0]`; `frag_idx 0` = `[len, frag_total, codec_id, flags]`; payload ≤ **124 B/block**.
- Codecs: Opus (id 0), PCM-diag (id 1, byte-certified), Faust phase-mod (id 2).
- L2 references: `codec/demod_audio.h` (C), `codec/src/audio.rs` (Rust),
  `python/MCP/audiolab_core.py` (Python), and **`lua/dcf_audio.lua`** (Lua, the
  self-certifying `LGPL-3.0-only` dual-licensed binding — see `lua/LICENSING.md`; CLI
  `lua/dcf_jam.lua`, panel `GUI/audiolab.lua`, frequency rendezvous via the `dst`
  channel). PM synth: `codec/faust/dcf_pm_codec.dsp` → committed `dcf_pm_codec.gen.c`.
- Vectors: `Documentation/audio_vectors.json`, `pm_param_vectors.json` (+ copies
  in `python/MCP/`), and `codec/audio_vectors.gen.h` (dependency-free C test).

```sh
python3 python/MCP/gen_audio_vectors.py /tmp/av.json          # regen + verify laws
cd codec && cargo test --test certify_audio                   # Rust
gcc -std=c11 -I codec C_SDK/tests/test_audio_certify.c -lm -o /tmp/ac && /tmp/ac  # C
cd codec && cargo run --example jam_loopback -- --codec pcm   # 2-peer jam demo (--loss 0.05 for PLC)
```

Opus is behind `--features opus` / `-DDCF_AUDIO_OPUS` (needs libopus); PM behind
`--features pm` / `-DDCF_AUDIO_PM`. Opus output and PM *synthesis audio* are
**not** byte-certified — only L2 framing, PCM-diag bytes, and the PM param layout
are. Rust SDK hookup: `DcfNode::send_audio_dcf` + `reassemble_audio_payload`
(`rust/src/lib.rs`).

## DCF-Game (multiplayer game state/events over the wire)

A second adapter over `DeModFrame`, structurally identical to DCF-Audio but on
`DATA` (type 0) frames instead of `CTRL`, so the two never collide. One game
message (state snapshot, input frame, or opaque event) is serialised into
`1 + ceil(payload_len/4)` ordinary `DATA` frames. The L2 framing is
**message-type-agnostic and byte-certified across C/Rust/Python**; `msg_type_id`
lives in the descriptor, so adding message types never changes the vectors.

- `seq = packet_id[15:5] | frag_idx[4:0]`; `frag_idx 0` = `[len, frag_total, msg_type_id, flags]`; payload ≤ **124 B/message**.
- Message types: SNAPSHOT (id 0, 14 B certified), INPUT (id 1, 6 B certified),
  EVENT (id 2, opaque), JOIN (id 3, certified). Descriptor `flags`: bit0 RELIABLE,
  bit1 ORDERED, bit2 END_TICK — these pick the transport path, not the L2 bytes.
- L2 references: `codec/demod_game.h` (C), `codec/src/game.rs` (Rust),
  `python/MCP/gamelab_core.py` (Python). Spec: `Documentation/DCF_GAME_SPEC.md`.
- Vectors: `Documentation/game_vectors.json` (+ identical `python/MCP/` copy) and
  `codec/game_vectors.gen.h` (dependency-free C test).
- Topology: direct P2P / LAN first (mDNS + `add_peer`, frequency-channel
  rendezvous via `dst`); multi-hop mesh forwarding is a future, additive extension.

```sh
python3 python/MCP/gen_game_vectors.py /tmp/gv.json           # regen + verify laws
cd codec && cargo test --test certify_game                    # Rust
gcc -std=c11 -I codec C_SDK/tests/test_game_certify.c -lm -o /tmp/gc && /tmp/gc  # C
```

SDK hookup: `DcfNode::send_game_dcf` + `reassemble_game_payload` and
`MessageHandler::handle_game` (`rust/src/lib.rs`). Client: a **Game** tab (a 2-D
dot-arena demo) alongside Jam/Messages in `client/`.

## Comms client (`client/`) — Tauri 2 end-user app

A cross-platform communications client (Rust core + Vue UI) on `dcf-rust-sdk` + the
`codec/` crate + cpal: Connect | Peers/Mesh | Messages | Jam (audio) | Wire inspector,
over the frequency-channel rendezvous. Real-time audio is **per-source** (reassembly
keyed by frame `src_id`). It records each participant as a **bit-exact Opus track** on a
sample-accurate timeline (`client/src-tauri/src/sync.rs`, unit-tested) and muxes a
multitrack `master.mka` + `mix.flac` via host **ffmpeg** (`recorder.rs`).

```sh
nix develop .#comms        # toolchain: rust + node + webkit + alsa + opus + cargo-tauri
cd client && npm install && cargo tauri dev
cd client/src-tauri && cargo test --features audio   # sync/channel/recorder (+ ffprobe)
cargo check --no-default-features                     # headless (no audio system libs)
```

The `audio` feature gates cpal/Opus/ogg so the mesh+messaging+wire client builds headless.
SDK additions for the client: `list_peers_detailed`/per-peer RTT, `run_ping_scheduler`,
`send_audio_dcf(.., channel)` (`rust/src/lib.rs`).

## C SDK (`C_SDK/`) — compiled spine vs. quarantine

Only four modules compile and ship (the sound part): `dcf_platform`, `dcf_error`,
`dcf_ringbuf`, `dcf_connpool` (`DCF_SOURCES` in `C_SDK/CMakeLists.txt`). Public
headers declare more than the build delivers; `include/experimental/`,
`plugins/experimental/`, `tests/legacy/` are quarantined (don't build).

```sh
cd C_SDK && mkdir build && cd build && cmake .. && make && ctest
cd C_SDK && make            # minimal/embedded (no CMake)
```

## Per-language build & test

| Dir | Build | Test |
|-----|-------|------|
| `codec/` (Rust wire+audio) | `cargo build` (`--features opus,pm`) | `cargo test --test certify --test certify_audio` |
| `rust/` (gRPC SDK) | `cargo build` | `cargo test` |
| `python/` | `pip install -r python/requirements.txt` | `pytest python/tests/` |
| `python/modem/` (FSK acoustic modem) | — | `python3 main.py --help` (uses `faust_jit.py`) |
| `go/` | `go build ./...` | `go test ./...` |
| `lisp/` | load `lisp/src/hydramesh.lisp` then `lisp/hydramesh-hotfix.lisp` in SBCL | hotfix prints `:CERTIFIED` |
| `Documentation/` (Sphinx) | `cd Documentation && pip install -r requirements.txt && make html` | — |

Lisp: before loading, remove `:cl-json-schema` from the `ql:quickload` list and
`defpackage` in `hydramesh.lisp` (not in Quicklisp). StreamDB (`lisp/streamdb/`,
Rust via CFFI) is Lisp-SDK-only.

## Nix / Docker

`flake.nix` exposes per-language packages (`dcf-c`, `dcf-cpp`, `dcf-go`,
`streamdb`, `dcf-python`, `dcf-rust`, `dcf-nodejs`, `dcf-perl`, `dcf-docs`) and a
default devShell (`nix develop`) with all toolchains. Top-level `Dockerfile`,
`install_deps.sh`, and `*-edit-gen.sh` bootstrap environments.

## Conventions when changing the wire/audio path

- A change to any codec is a change to all of them. The certificate is the
  contract: regenerate the golden vectors, run the certs, keep the `Documentation/`
  and `python/MCP/` copies identical. CI fails on drift.
- New transports/codecs are adapters over `DeModFrame`, not new wire formats.
- The protocol is deliberately **encryption-free** for EAR/ITAR export compliance
  (`Documentation/Specs/export_compliance.markdown`). Do not add encryption to the
  core wire path.
- License is **LGPL-3.0** for the linkable library; GPL-3.0 is scoped to the DOOM
  example only. (The repo has historical README inconsistencies — see the code
  review.)
