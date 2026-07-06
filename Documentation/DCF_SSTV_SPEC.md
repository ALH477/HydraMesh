# DCF-SSTV — Slow-Scan Television (Still-Image Transport) over the DeModFrame Wire

**Version 1** · DeMoD LLC · This document is normative. Companion to
[`WIRE_QUANTUM_SPEC.md`](WIRE_QUANTUM_SPEC.md) and a sibling of
[`DCF_TEXT_SPEC.md`](DCF_TEXT_SPEC.md), [`DCF_GAME_SPEC.md`](DCF_GAME_SPEC.md) and
[`DCF_AUDIO_SPEC.md`](DCF_AUDIO_SPEC.md).

DCF-SSTV carries a **still image** — the digital, DCF-native reimagining of ham
"slow-scan television" — across the mesh, and in particular across the narrow
[HydraModem](../hydramodem/README.md) acoustic link. It introduces **no new wire
format**: one image is an **adapter** over the 17-byte `DeModFrame` quantum,
serialised into a burst of ordinary `DATA` (type 0) frames. The framing is
**content-agnostic and byte-deterministic across C, Rust, Python, Go, and Node.js**,
pinned by a finite certificate exactly like the wire quantum itself.

Why "slow-scan": at HydraModem's default conv profile the link is roughly **8–12
application bytes/s**, so a few-KB thumbnail is *minutes* of air time. The receiver
renders it **top-down as fragments arrive** (a progressive scan), which is the
slow-scan experience — just carried as certified digital frames rather than an analog
FM waveform.

Audio rides `CTRL` (type 3); **text, game, and sstv all ride `DATA` (type 0)** but
split the `seq` field differently — sstv gives the fragment index **11 bits** (an
image is larger than a chat line or per-tick game state), text 10, game 5. See
[Relationship to the other DATA adapters](#relationship-to-the-other-data-adapters).

## Layered model

| Layer | What | Certified? |
|-------|------|-----------|
| **L1** image | One image blob (≤ 8192 B per image_id) — opaque bytes; a `format_id` names the encoding. | n/a (opaque app bytes) |
| **L2** framing | `packetize` / reassemble: image bytes ⇄ `DeModFrame` `DATA` frames. | **Yes** — `sstv_vectors.json` |
| **L3** delivery | Reassembly with dup-suppression + out-of-order tolerance; progressive-prefix render; optional ARQ via the `RELIABLE` flag. | Runtime (unit-tested) |
| **L4** transport + apps | Frames over the mesh / HydraModem; `sstv_send`/`sstv_recv`, `sstv_demo.py`. | App |

The key invariant: **the `format_id` and descriptor flags are opaque to L2, so they
never change the L2 vectors** — the framing bytes are invariant to the image
encoding and the flag choice, so adding image formats never re-opens the certificate.

## L2 framing (normative)

One image → `1 + frag_total` frames. Every frame is a fully valid `DeModFrame`
(version = 1, type = `DATA` = 0) and still passes the 246-vector wire certificate.

```
 seq (u16)      = image_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
 frag_idx == 0  descriptor : payload = [len_hi, len_lo, format_id, flags]  (length big-endian u16)
 frag_idx 1..N  data       : payload = bytes[(k-1)*4 .. +4]                 (last frame zero-padded)
 frag_total     = ceil(len / 4)
 timestamp_us   = 24-bit send time, identical across an image's frames
 src_id         = local node id ; dst_id = rendezvous channel (0xFFFF = broadcast)
```

Bounds (from the 11-bit fragment field):

| Quantity | Value |
|----------|-------|
| Max data fragments | 2047 |
| **Max image payload** | **8188 bytes / image_id** |
| Max `image_id` | 31 (5-bit) |
| Frames per image | `1 + ceil(len/4)` (1–2048) |

The descriptor carries the **true byte length** (big-endian u16) so the receiver can
strip the zero-padding on the final data fragment. Images larger than 8188 B are
chained by the application across successive `image_id`s (the `MORE` flag marks a
continuation); the adapter never re-fragments beyond the 8188 B quantum.

**Format id** (`payload[2]` of the descriptor) is an **opaque hint** for the decoder —
it does **not** change the L2 bytes and is not parsed by L2 (exactly as DCF-Audio's
`codec_id` and DCF-Game's `msg_type_id`). Geometry (width/height) lives in the
image's own leading bytes, not in L2.

| Id | Name | Meaning |
|----|------|---------|
| 0 | `RAW` | Raw application bytes, app-defined geometry. |
| 1 | `JPEG` | JPEG octet stream. |
| 2 | `PNG` | PNG octet stream. |
| 3 | `WEBP` | WebP octet stream. |
| 4 | `RGB565` | `width*height*2` little-endian RGB565, geometry app-side. |

Ids 5–255 reserved; adding one never changes the vectors.

**Descriptor flags** (`payload[3]` of the descriptor) request delivery behaviour;
they do **not** change the L2 bytes:

| Bit | Name | Meaning |
|-----|------|---------|
| 0 | `MORE` (0x01) | Image continues in the next `image_id` (a chained large image). |
| 1 | `KEYFRAME` (0x02) | Start of a fresh image (vs a continuation). |
| 2 | `RELIABLE` (0x04) | Ask the receiver to ACK (the DCF-SSTV reliability layer). |

Remaining bits reserved. Because the flags are opaque to L2, a `KEYFRAME|RELIABLE`
image and a bare image of the same bytes produce **byte-identical** framing.

**Reassembly.** Buffer by `image_id`; gather the descriptor and every data fragment
`1..frag_total`; concatenate and truncate to `len`; emit `(image_id, timestamp_us,
src, dst, format_id, image_bytes, flags)`. Duplicate fragments are ignored (idempotent
— replaying a fragment never double-emits). Out-of-order delivery is tolerated. A
`finalize` call reports every still-incomplete image as **lost** (ascending
`image_id`) and clears state.

**Progressive render (L3, not certified).** The Python reference reassembler
optionally emits `("progress", image_id, prefix_len)` whenever the largest *contiguous*
received byte-prefix grows, so a viewer can paint the image top-down as fragments
trickle in. Progress events are a convenience — the byte certificate uses the
non-progress path, so the ports need not implement it to interoperate.

## Relationship to the other DATA adapters

Text, game, and sstv **share `DATA` (type 0)** and the same descriptor-then-data
fragmentation scheme. There is **no in-band tag** distinguishing them — they differ
only by `seq` split (text 6+10, game 11+5, sstv 5+11), which is not self-describing.
They are therefore separated by **deployment, not by the bytes**: a node feeds a
channel's frames to the reassembler for the one service it runs on that channel. Do
**not** multiplex sstv with text or game on the same `dst` channel expecting a single
node to demultiplex them in-band. Audio is unambiguous against all three (it rides
`CTRL`).

## Addressing & channel rendezvous

SSTV frames ride the existing DCF transport. Peers rendezvous on a **channel** (the
`DeModFrame` `dst_id`): `dst_id = 0xFFFF` is broadcast; otherwise the channel is
derived from a shared passphrase via the certified CRC-16
(`channel_id(name) = crc16(name)`, the same hash the rest of the repo uses). A node
accepts a frame iff `dst_id == my_channel` or `dst_id == 0xFFFF`, and multiplexes
correspondents by `src_id`.

## HydraModem carriage

DCF-SSTV frames cross a real acoustic link with the HydraModem tools
(`hydramodem/dcf-tools/`), which transport the 17-byte frame **opaquely** — the wire
certificate is untouched:

```sh
cd hydramodem && dcf-tools/build.sh
dcf-tools/build/sstv_send photo.jpg out.wav --conv --image-id 3 --format jpeg   # image -> M-FSK audio
dcf-tools/build/sstv_recv out.wav got.jpg  --conv                               # audio -> byte-exact image
```

A hardware-free, in-process demo (with the progressive scan render) is
`python/dcf/sstv_demo.py --image photo.jpg --transport loopback`; add
`--transport hydra` to drive the real acoustic link (`frame_tx`/`frame_rx` on PATH).

## Certification

Mirrors the wire quantum and DCF-Text: a Python reference emits golden vectors; the
ports diff against them byte-for-byte.

```sh
# regenerate + verify the laws, then diff against the committed vectors
python3 python/MCP/gen_sstv_vectors.py /tmp/sstv_vectors.json
diff /tmp/sstv_vectors.json   Documentation/sstv_vectors.json   # must be empty
diff /tmp/sstv_vectors.json   python/MCP/sstv_vectors.json       # must be empty

cd codec && cargo test --test certify_sstv                       # Rust
gcc -std=c11 -I codec C_SDK/tests/test_sstv_certify.c -lm -o /tmp/sc && /tmp/sc   # C
cd go && go test ./sstv/                                          # Go
node JS/nodejs/test/certify_sstv.js                              # Node.js
```

`Documentation/sstv_vectors.json` (with an identical `python/MCP/` copy) holds **9
framing + 4 reassembly** cases (the reassembly cases exercise in-order, reordered,
duplicate, and fragment-drop-lost delivery); `codec/sstv_vectors.gen.h` is the
dependency-free C vector dump (payloads bounded to 128 B — the 8188 B rail is certified
by Python + Rust). CI runs the Python/C/Rust certs and diffs regenerated vs. committed
vectors (`.github/workflows/wire-certify.yml`, job `certify-sstv`); the Go cert runs
under `certify-go`, the Node cert under `certify-node`.

**Anchor.** `exampleImage`: `payload = ffd8ffe000104a46494600010100` (14 bytes, a JPEG
SOI/APP0 stub), `image_id = 10`, `ts_us = 66051`, `src = 0x00A1`, `dst = 0x06FD`
(`channel_id("sstv")`), `format_id = 1` (JPEG), `flags = 0x06` → 1 descriptor + 4 data
frames, first frame `d310500000a106fd000e010601020388a3`.

## Reference implementations

| Lang | File | Entry points |
|------|------|--------------|
| C | `codec/demod_sstv.h` | `dcf_sstv_packetize` / `dcf_sstv_reasm_push`, `dcf_sstv_channel_id` |
| Rust | `codec/src/sstv.rs` | `packetize` / `SstvReassembler`, `channel_id` |
| Python | `python/MCP/sstvlab_core.py` | `packetize` / `SstvReassembler`, `channel_id` |
| Go | `go/sstv/sstv.go` | `Packetize` / `SstvReassembler`, `ChannelID` |
| Node.js | `JS/nodejs/src/sstv.js` | `packetize` / `SstvReassembler`, `channelId` |

`python/MCP/sstvlab_core.py` is the canonical source of truth (the generator
`gen_sstv_vectors.py` emits the laws + vectors from it).

## Theorem

L2 framing is a fixed bit-placement adapter over the certified `DeModFrame`. Matching
the framing + reassembly vectors pins the C, Rust, Go, and Node implementations to the
Python reference on the entire input space. The descriptor `format_id` and flags are
opaque to L2, so the framing vectors are invariant to the image encoding and the
flag choice — adding formats or transports never re-opens the certificate, and the
246-vector wire certificate is untouched.

## Security & export posture

Like the rest of DCF, the SSTV wire is **plaintext** — image bytes, `src`/`dst`, and
timing are fully readable by any on-path observer (the protocol is deliberately
encryption-free for export compliance). Deploy behind WireGuard or operator-supplied,
export-compliant crypto **beneath** the transport; never add encryption to the codec.
See [`DCF_SECURITY_EXPOSURE.md`](DCF_SECURITY_EXPOSURE.md).
