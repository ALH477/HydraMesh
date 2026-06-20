# DCF-Radio — live HLS stream + DVR over the mesh

DCF-Radio turns live DCF-Audio into a **digital radio**: a continuous stream per
frequency channel that anyone can tune into in a browser/VLC, with **DVR rewind** —
scrub back into the past while the live edge keeps advancing. It is an *adapter
consumer*, not a new wire format: it reuses the certified frame `decode` (to route
by channel), the native `dcf` ffmpeg demuxer (codec-agnostic decode/mix), and host
FFmpeg's HLS muxer. No wire/cert changes.

## Model

- **A station = one frequency channel** (the frame `dst`). Every peer talking on
  that channel is mixed into one live program. `dst = 0xFFFF` (broadcast) is the
  station `CH65535`.
- **Retention = rolling HLS window + raw `.dcf` archive.** The HLS playlist keeps a
  bounded, seekable DVR window (`--dvr`, default 6 h) for instant rewind; in
  parallel every frame is appended to `radio-CH<dst>.dcf` — the *full* history,
  re-renderable any time with `dcf-rec rec`.
- **Two ingress modes** feed the same engine:
  - **wiretap** — `dcf-rec stream` / `dcf-radio`: a passive (or inline `--forward`)
    UDP tap. Works in front of any node.
  - **AUTO/master node** — `dcf_node.py start --radio`: the node serves a radio of
    the audio it receives (via its `on_audio` tee).

```
 MSG_AUDIO frames ─▶ ingest ─▶ RadioStation(per dst) ─▶ HLS DVR playlist ─▶ HTTP
   (wiretap | node)              └─ radio-CH<dst>.dcf  (raw archive, full past)
```

Each HLS stream is **AAC in MPEG-TS** by default (max player reach) — or Opus in
fMP4 with `--codec opus` — with `#EXT-X-PROGRAM-DATE-TIME` for a stable DVR
timeline. Players: Safari/iOS open the `.m3u8` natively; the built-in `/` page uses
hls.js for other browsers; VLC/ffmpeg open the playlist URL directly.

**No dead air.** A per-station writer thread paces a continuous 20 ms frame stream
into ffmpeg: real audio when peers talk, **codec-correct silence when they don't**,
so the live edge tracks wall-clock and the DVR window never has holes. Ingest is
decoupled (a bounded queue + a 4 MB socket buffer), so capture never blocks on the
encoder. The raw `.dcf` archive stores **only real audio** (compact) and rotates at
`--archive-max` MB.

## Endpoints

| Path | What |
|---|---|
| `/` | player (station list, tune-in, DVR scrub) |
| `/radio/CH<n>.m3u8` | the live HLS stream (seekable DVR window) |
| `/radio/CH<n>.live.mp3` | continuous low-latency MP3 (Icecast-style, no rewind) |
| `/stations.json` | now-playing: per-peer `{id,name,talking,blocks}`, counters |
| `/healthz` | `{status, stations, uptime_s}` |
| `/metrics` | Prometheus: `dcf_radio_frames_total`, `_silence_frames_total`, `_dropped_frames_total`, `_active_peers`, `_stations` |

## Usage

```sh
# wiretap radio: a station per channel seen, served on :8000, 6 h rewind
dcf-radio --bind 0.0.0.0:7100 --http 127.0.0.1:8000 --dvr 6h --archive ./radio
#   open http://127.0.0.1:8000/        (player + station list)
#   http://127.0.0.1:8000/radio/CH1420.m3u8   (tune in / rewind)

# inline transparent tap in front of a node
dcf-radio --bind 0.0.0.0:7100 --forward node:7777 --http :8000

# pin a single channel; friendly names; loudness-normalized Opus
dcf-radio --bind :7100 --channel 1420
dcf-radio --bind :7100 --names 0x00a1=Hermes,1420=MainStage --loudnorm --codec opus

# observe
curl localhost:8000/healthz ; curl localhost:8000/metrics

# container service (HLS on :8000, UDP tap on :7100)
nix build .#docker-dcf-radio

# from the comms client: the Radio tab has a "Broadcast" toggle and "Open player ↗"

# AUTO/master node serving its own radio
python3 python/dcf_node.py start --mode master --node-id 1 \
        --radio 127.0.0.1:8000 --radio-dvr 6h --radio-archive ./radio

# replay the full past from the archive (beyond the live DVR window)
dcf-rec rec ./radio/radio-CH1420.dcf -o show.flac
```

`dcf-radio` is an alias for `dcf-rec stream`; both ship in the flake
(`nix build .#dcf-radio`) and the default devShell.

## Notes & limits

- **Latency** is HLS-typical (~4–10 s); fine for radio, not for live conversation.
- **Dynamic mixing**: when a new peer joins a live channel the encoder relaunches
  with a wider `amix` (a brief discontinuity). The raw `.dcf` archive is always
  exact, so a re-render mixes everyone cleanly.
- **Security**: the DCF wire is plaintext by design. Keep the HTTP server on
  localhost / a trusted net (default `--http 127.0.0.1:8000`) and run the UDP side
  under WireGuard — never add crypto to the codec. See `DCF_SECURITY_EXPOSURE.md`.
- Codec is transcoded to AAC for the stream (not bit-exact); use `dcf-rec` on the
  archive for bit-exact Opus. Opus-in-fMP4 HLS is an easy follow-on.
