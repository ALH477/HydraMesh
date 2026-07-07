# DCF-Snake — Synchronized Studio Audio Snake over cat5e

**Version 1** · DeMoD LLC · This document is normative. Companion to
[`WIRE_QUANTUM_SPEC.md`](WIRE_QUANTUM_SPEC.md), [`DCF_AUDIO_SPEC.md`](DCF_AUDIO_SPEC.md)
and the quanta codec (`docs/SPEC.md`, Appendix S — QSS streaming).

DCF-Snake is a **software digital audio snake**: a *star* of source ("spoke") nodes
streaming to one central **"mixer"** hub over dedicated **cat5e**, for studio use. It has
**two planes on two physical wires per node**:

- **Record plane (DCF-Snake):** compressed, sample-accurate multitrack **capture** to the
  mixer, carrying the DeMoD **quanta** codec (QSS streaming container). Latency-tolerant
  (quanta's ≥64 ms encode floor is unfit for live tracking but ideal for recording, where
  it uses *less* bandwidth than PCM). The mixer is the live analog of the offline
  multitrack recorder (`client/src-tauri/src/recorder.rs`).
- **Cue plane (DCF-Cue):** a **bidirectional** uncompressed-PCM monitor loop on a *second*
  wire — sources send tiny (~1 ms) PCM blocks up, the mixer sums a **per-node cue mix**
  (aux sends) and returns each performer their own low-latency monitor feed (~4–6 ms
  round-trip). No atom pursuit, so it escapes the quanta latency wall.

Both planes are **adapters over the 17-byte `DeModFrame` quantum** — no new wire format, the
246-vector wire certificate is untouched. Both lock to one **BEACON (type 2) grandmaster
media clock** broadcast by the mixer, and ride a **raw-L2-Ethernet transport** (a custom
EtherType per plane, no IP/UDP). The wire is plaintext — deploy behind WireGuard or
operator crypto beneath the link (`DCF_SECURITY_EXPOSURE.md`).

## Topology

```
 spoke0 ═╗  wire A (quanta record, EtherType 0x88B5)  →  ┌──────────────┐
 spoke1 ═╬══════════════════════════════════════════════►│              │
 spoke2 ═╝                                                │    MIXER     │  grandmaster clock
         ◄── wire B (PCM cue return, 0x88B6) ────────────┤ (hub / sink) │  per-src reassemble +
 spoke0 ═╗  wire B (PCM cue up, 0x88B6)  ───────────────►│              │  decode + align + ASRC
 spoke1 ═╬══════════════════════════════════════════════►│              │  + sum → record sink;
 spoke2 ═╝                                                └──────┬───────┘  per-node cue → return
              BEACON(2) media clock  ◄───────────────────────────┘  (broadcast on BOTH wires)
```

The single sink **is** the clock master — no election (unlike DCF-Mesh).

## Layered model

| Layer | What | Certified? |
|-------|------|-----------|
| **L1** codec | quanta QSS (record) / raw PCM (cue). QSS opaque to L2; a `mode_id` hint lives in the descriptor. | PCM byte-count: derived. quanta QSS audio & the mixer ASRC/PLC/cue-mix: **no** (float, like Opus/PM). |
| **L2** framing | `packetize`/reassemble: codec bytes ⇄ DeModFrame frames. Record 5:11 on CTRL(3); cue 9:7 on CTRL(3); clock 5:11 on BEACON(2). | **Yes** — `snake_vectors.json`, `monitor_vectors.json` |
| **L3** clock + jitter + PLC | BEACON media clock, PI servo, per-source ASRC, hop/block jitter buffer, host-side PCM PLC. | clock payload + `unwrap_pid`: **yes**. servo/ASRC/PLC timing: runtime. |
| **L4** transport + mixer | raw-L2 Ethernet (SuperPack-batched), the C spoke/mixer nodes, cue-mix engine. | Loopback/integration-tested. |

## L2 framing — record plane (normative)

One self-delimiting quanta QSS commit-hop packet → one DCF-Snake *message* → `1 + frag_total`
frames, every one a valid **CTRL (type 3)** `DeModFrame`.

```
 seq (u16)      = stream_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
 frag_idx == 0  descriptor : payload = [len_hi, len_lo, mode_id, flags]
 frag_idx 1..N  data       : payload = qss[(k-1)*4 .. +4]   (last frame zero-padded)
 frag_total     = ceil(len / 4)              (<= 2047  =>  len <= 8188 bytes / message)
 timestamp_us   = 24-bit frame ts (per-frame; NOT the media clock — see below)
 src / dst      = node id / rendezvous channel (crc16(passphrase), 0xFFFF = broadcast)
```

`mode_id` = quanta `LIVE=0` (64 ms hop) / `NEAR=1` (128 ms) / `RELAXED=2` (256 ms). Descriptor
`flags`: bit0 `MORE`, bit1 `ANCHOR` (this QSS packet re-anchors — low prediction dependency),
bit2 `END`. `mode_id`/`flags` are **opaque to L2**, so adding modes never changes the vectors.
`stream_id` is a rolling 5-bit slot; the **real monotone clock is the QSS `hop_index`** carried
inside the decoded stream, never `stream_id` or the 24-bit frame ts.

| mode | QSS pkt | data frags | frames/pkt | SuperPacks/pkt | wire expansion |
|------|---------|-----------|-----------|----------------|----------------|
| live (64 ms) | ~784 B | 196 | 197 | ~99 | ~4.27× |
| near (128 ms) | ~1328 B | 332 | 333 | ~167 | ~4.26× |
| relaxed (256 ms) | ~2048 B | 512 | 513 | ~257 | ~4.25× |

**Reassembly.** Buffer by `stream_id`; gather the descriptor + all data fragments; concatenate
and truncate to `len`. Duplicates ignored; any missing fragment ⇒ message lost ⇒ L3 PLC.
Because CTRL(3) also carries DCF-Audio (an 11:5 split), a node runs **exactly one reassembler
per `dst` channel** — never multiplex DCF-Audio and DCF-Snake on one `dst`. On a dedicated snake
wire this is automatic.

## L2 framing — cue plane (normative)

One tiny PCM block → one DCF-Cue *block* → `1 + frag_total` **CTRL (type 3)** frames on wire B.

```
 seq (u16)      = block_seq[15:7] (9 bits, 0..511) | frag_idx[6:0] (7 bits, 0..127)
 frag_idx == 0  descriptor : payload = [block_samples, format, channels, flags]
 frag_idx 1..N  data       : payload = pcm[(k-1)*4 .. +4]   (last frame zero-padded)
 length         = block_samples * channels * bytes_per_sample(format)   (derived, self-describing)
 frag_total     = ceil(length / 4)           (<= 127  =>  length <= 508 bytes / block)
```

`format` = `S16=0` (2 B) / `S24=1` (3 B) / `F32=2` (4 B). `flags`: bit0 `CUE_RETURN` (downlink
mixer→node vs uplink), bit1 `END`. The block geometry is self-describing — no explicit length
field. `block_seq` is a rolling 9-bit counter (wraps ~512 ms at 1 ms blocks) the mixer unwraps
with `unwrap_pid` (mod 512). A 1 ms block at 48 kHz is 48 samples: mono S16 = 96 B / 24 frames,
stereo F32 = 384 B / 96 frames — all within the 508 B / 127-frag rail.

**Cue-loop latency budget** (48-sample blocks): capture 1 ms · encode 0 · raw-L2 up <0.5 ms ·
mixer sum ~1 block · raw-L2 down <0.5 ms · playout 1 ms → **~4–5 ms round-trip** (≈3 ms at
24-sample blocks).

## BEACON grandmaster media clock (normative)

The mixer periodically broadcasts a 16-byte media clock in **BEACON (type 2)** frames on both
wires, fragmented by the same 5:11 fragmenter (descriptor `[0, 16, clk_ver=1, flags]` + 4 data
frames = 5 frames/tick, ~50–100 ms interval):

```
 [0..7]   gm_sample_count  u64 BE   grandmaster capture-sample index (monotonic, never wraps)
 [8..11]  nominal_rate_mHz u32 BE   nominal sample rate in milli-Hz (48000000 = 48 kHz)
 [12..13] tx_seq           u16 BE   beacon sequence (PLL discipline / loss detect)
 [14..15] epoch            u16 BE   session epoch (bumps on rate change / mixer restart)
```

**Absolute time lives only in `gm_sample_count`**; the frame's 24-bit `timestamp_us` (which
wraps ~16.7 s) carries the mixer TX instant (T1) for delay/skew estimation at RTT scale.
Alignment keys on `gm_sample_count` / QSS `hop_index`, never the frame ts.

## Hybrid clock discipline

- **Spoke PI servo.** Each source disciplines its capture clock to the grandmaster: from BEACON
  `gm_sample_count` + `nominal_rate` and its own sample counter (corrected by one-way delay
  `owd = rtt/2`, reusing the existing mesh PING/PONG RTT), it drives `ppm = Kp·err + Ki·∫err`.
- **Mixer ASRC fallback.** For sources that can't lock (no rate control) or drift out of range,
  the mixer runs a per-source **async sample-rate converter** (fractional polyphase/Farrow
  resampler) driven by the measured skew, bringing every source onto the grandmaster output
  clock. The ASRC waveform is **not byte-certified** (float DSP, like Opus/PM synthesis).

## `unwrap_pid` (certified timeline primitive)

Ported verbatim from `client/src-tauri/src/sync.rs`: unwrap a rolling packet_id to a monotonic
absolute index (forward progress dominates; a small backward delta = reorder → saturating
step-back). Byte-certified for the mixer's alignment (record: QSS `hop_index`; cue: `block_seq`
mod 512).

## Raw-L2 Ethernet transport

`AF_PACKET`/`SOCK_RAW`, bound to a named interface, per-plane EtherType (record `0x88B5`, cue
`0x88B6` — IEEE local-experimental range, no clash with real AVB `0x22F0`). `SO_TIMESTAMPING`
feeds the clock servo low-jitter arrival times. The Ethernet payload is a batch of **32-byte
SuperPacks** (`SUPERPACK_SPEC.md`); **jumbo frames (9000 MTU) recommended** so a whole
live-mode QSS packet rides one Ethernet frame (loss-atomic). Requires `CAP_NET_RAW`
(`setcap cap_net_raw+ep`). A privilege-free loopback double certifies the framing in CI.

## Mixer DSP

Per source: raw-L2 recv → SuperPack unpack → DCF-Snake reassemble → `quanta-stream-decode`
subprocess → PCM → jitter buffer (keyed on QSS `hop_index`) → `unwrap_pid`/`TrackTimeline`
align + ASRC drift-correct → sum-mix → record sink. In parallel, the cue engine sums a per-node
aux-send mix matrix (all uplinks are already sample-aligned) → returns each node its cue feed.
The jitter buffer, ASRC, host-side PCM PLC, and cue-mix are **not byte-certified** (float DSP).

## quanta integration (license boundary)

The encoder (`quanta-stream`) and decoder (`quanta-stream-decode`) run as **subprocesses**
(like `JanusTransport`→janus-c), discovered via `$QUANTA_STREAM` / `$QUANTA_STREAM_DECODE` then
PATH, with a graceful skip when absent. quanta is **GPL-3.0-only OR DeMoD-Commercial** and is
kept **entirely out of the LGPL closure** (mere aggregation, never linked) — shipped as a
standalone `nix build .#quanta`.

## Certification

| Artifact | Role |
|----------|------|
| `Documentation/snake_vectors.json` (= `python/MCP/`) | record framing + reassembly + clock + unwrap |
| `Documentation/monitor_vectors.json` (= `python/MCP/`) | cue PCM framing + reassembly |
| `codec/snake_vectors.gen.h` / `monitor_vectors.gen.h` | dependency-free C headers |
| `python/MCP/snakelab_core.py` / `monitorlab_core.py` | Python L2 references |
| `python/MCP/gen_snake_vectors.py` / `gen_monitor_vectors.py` | executable laws + vector generators |
| `codec/demod_snake.h` / `demod_monitor.h` (C) · `codec/src/snake.rs` / `monitor.rs` (Rust) | references |

```sh
python3 python/MCP/gen_snake_vectors.py   /tmp/sv.json    # regen record + verify laws
python3 python/MCP/gen_monitor_vectors.py /tmp/mv.json    # regen cue + verify laws
gcc -std=c11 -I codec C_SDK/tests/test_snake_certify.c   -lm -o /tmp/sc && /tmp/sc
gcc -std=c11 -I codec C_SDK/tests/test_monitor_certify.c -lm -o /tmp/mc && /tmp/mc
cd codec && cargo test --test certify_snake --test certify_monitor
```

CI: the `certify-snake` job in `.github/workflows/wire-certify.yml` regenerates + diffs both
vector sets and runs the C + Rust certs. The cert path is pure LGPL and never touches quanta.

## Deployment notes

- **Two cat5e runs per node** (record + cue), each on its own EtherType; a dedicated L2
  broadcast domain (no routing). Enable jumbo frames on the switch; some managed switches
  filter unknown EtherTypes.
- `CAP_NET_RAW` for the raw sockets. Plaintext wire — deploy inside WireGuard/operator crypto
  beneath the link.
- True sub-sample word-clock (NIC PTP/PHC hardware timestamping) is a future extension; v1 uses
  software timestamps + mixer ASRC (ample for capture alignment).
