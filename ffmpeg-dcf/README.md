# DCF-Audio → FFmpeg (`dcf` demuxer + `dcf-rec`)

A **native FFmpeg demuxer** for the DeMoD DCF-Audio wire, so stock FFmpeg can
record audio carried over `DeModFrame`s into any container/codec it supports:

```sh
nix build .#dcf-ffmpeg                              # patched ffmpeg (builds the demuxer in)
ffmpeg -f dcf -i capture.dcf out.flac              # transcode to anything
ffmpeg -f dcf -i capture.dcf -map 0:a:0 peerA.wav  # pull one peer's track
dcf-rec capture.dcf -o master.mka                  # CLI front door, format by extension
```

## `dcf-rec` — the CLI front door

One ergonomic command (Python; reuses the certified `python/MCP` + `python/dcf`
modules) over the `dcf` demuxer. Bare `dcf-rec INPUT ...` is shorthand for
`dcf-rec rec INPUT ...`.

```sh
# record (type by extension; multi-peer -> downmix, except .mka = multitrack master)
dcf-rec capture.dcf -o out.flac
dcf-rec capture.dcf                       # no -o  -> capture.flac (mixdown)
dcf-rec capture.dcf -o master.mka         # one bit-exact track per peer
cat capture.dcf | dcf-rec - -o out.wav    # stdin;  -o - + --format = stdout

# inspect: peers, codecs, duration, packet loss, channels
dcf-rec info capture.dcf                   # table   (--json for scripts)

# per-peer stems (DAW): one file per peer, named by src id
dcf-rec split capture.dcf --out-dir stems/ --format wav
#   stems/peer-0x00a1.wav, stems/peer-0x0042.wav, ...

# record live off the wire (passive sniffer; Ctrl-C to stop)
dcf-rec listen --bind :7100 -o jam.flac
dcf-rec listen --bind :7100 --forward host:7000 -o jam.flac   # transparent inline tap
```

Each peer (frame `src`) becomes one stream, tagged `title="peer 0x<id>"` so
players, `ffprobe`, and `.mka` tracks name it. `info` durations/loss come from a
direct frame scan (accurate), not ffprobe's bitrate estimate.

This is **not a new wire format**. DCF-Audio is an *adapter* over the certified
17-byte wire quantum; the demuxer reuses the certified C reassembler + frame codec
(`codec/demod_audio.h`, `codec/demod_frame.h`) verbatim, so the bytes it hands
FFmpeg are provably the same ones `Documentation/audio_vectors.json` and the
246-vector wire certificate pin. See `Documentation/DCF_AUDIO_SPEC.md`.

## The `.dcf` frame-dump format

A flat concatenation of **17-byte `DeModFrame`s** — exactly the AUDIO adapter
frames that ride on the wire (one `DeModFrame` per UDP `MSG_AUDIO` payload). It is
self-synchronising: every record starts with sync `0xD3` and ends with a
CRC-16/CCITT-FALSE over bytes `[0..14]`, so the demuxer can probe and resync.

Produce one with either:

```sh
# deterministic PCM-diag test tone (+ per-source ground-truth .u8 sidecars)
python3 python/MCP/gen_audio_dump.py capture.dcf --src 1 --src 7 --blocks 50

# or capture live AUDIO frames off the plaintext wire
python3 python/dcf_node.py wiretap --bind 0.0.0.0:7100 --forward host:7000 --dump capture.dcf
```

## Codec-id → FFmpeg mapping

Each frame `src` id becomes one audio stream. The descriptor's `codec_id` maps
onto existing FFmpeg decoders, so **no custom `AVCodec` is needed** — only the
demuxer:

| `codec_id` | DCF codec | FFmpeg stream | Notes |
|---|---|---|---|
| 0 | Opus, 48 kHz mono | `AV_CODEC_ID_OPUS` (+ `OpusHead`) | `-c:a copy` is bit-exact |
| 1 | PCM-diag, u8 @ 6 kHz | `AV_CODEC_ID_PCM_U8` | the bytes *are* unsigned-8 PCM |
| 2 | Faust phase-mod | `AV_CODEC_ID_PCM_F32LE` @ 48 kHz | **synthesised inside the demuxer** via `dcf_pm_synth_block_ffi` |

Packet PTS = the source's unwrapped 11-bit `packet_id` × the 20 ms block length
(in `1/sample_rate` units), so dropped blocks surface as honest PTS gaps.

## Files

- `dcfdec.c` — the demuxer (`ff_dcf_demuxer`, name `"dcf"`). Dropped into
  `libavformat/` at build time alongside the certified headers and the PM synth
  object (`codec/faust/dcf_pm_codec.gen.c`).
- `dcf-rec` — thin CLI wrapper; selects ffmpeg args by output extension.
- Build/registration lives in `flake.nix` (`packages.dcf-ffmpeg`): it copies the
  sources into the FFmpeg tree, inserts `extern const FFInputFormat
  ff_dcf_demuxer;` into `libavformat/allformats.c` (which `configure`'s
  `find_things_extern` scans), and appends the `OBJS-$(CONFIG_DCF_DEMUXER)` line.

## Version pin

Written against the FFmpeg the flake's `nixpkgs` provides (**8.x / libavformat
62**, the `FFInputFormat` demuxer API). FFmpeg's internal demuxer API is not
stable across majors; if the pin moves to a new FFmpeg major, re-check the
`FFInputFormat` struct and `ff_alloc_extradata` / `avpriv_set_pts_info` in
`libavformat/{demux.h,internal.h}`.
