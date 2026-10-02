# DCF-WASM — the browser comms client

A single self-contained `index.html` that runs the DCF protocol **in the browser
as WebAssembly** and joins the mesh through a thin WebSocket↔UDP bridge. It is
**not a new wire format or transport** — it is the certified `dcf-wire-codec`
compiled to `wasm32` plus the redesigned comms UI, speaking the same bare-frame
dialect as the Node.js / Python text nodes.

## Architecture

```
 ┌─ index.html (one file) ───────────────┐        ┌─ dcf-ws-bridge ─┐      DCF mesh
 │  comms UI (client/src/App.vue)         │  WS    │  stateless      │  UDP (bare
 │  web/src/wasm-ipc.ts ──▶ dcf_codec.wasm│◀──────▶│  WS↔UDP relay   │◀────▶ frames +
 │  Web Audio (Jam)   (certified codec)   │ binary │  + peer list    │      SuperPacks)
 └────────────────────────────────────────┘ frames └─────────────────┘
```

- **The browser owns the protocol** (encode / packetize / reassemble / channel)
  in WASM (`codec-wasm/`, a wasm-bindgen surface over `codec/`). Only opaque
  datagrams cross the socket.
- **The bridge is dumb, but it gates** (`web/bridge/`, `dcf-ws-bridge`): one UDP
  socket per WS connection, a peer list set by JSON control messages, broadcast
  out / relay in. It never decodes, routes or reassembles. It does check that
  each datagram, in either direction, is something the bare dialect carries.
  See *Security* below.

## Bridge protocol (browser ↔ `dcf-ws-bridge`)

- **Control** — WS *text* frame, JSON: `{"op":"addpeer","host":"…","port":N}`,
  `{"op":"clear"}`.
- **Data out** — WS *binary* frame = one UDP payload: a 17-byte DeModFrame or a
  32-byte SuperPack. The bridge `send_to`s it to every peer. Anything else is
  dropped and counted.
- **Data in** — each inbound UDP datagram from a known peer that passes the same
  gate is delivered as a WS *binary* frame. The browser unpacks SuperPacks and reassembles, deriving `src`
  from the frame.

## Dialect & routing (what the browser emits/accepts)

Bare frames batched into 32-byte SuperPacks (pair consecutive frames; lone
trailing frame raw), rendezvous on the frame `dst` — identical to
`JS/nodejs/src/node.js`. Inbound routing:

| Frame type | Channel | Adapter |
|-----------|---------|---------|
| `CTRL` (3) | active | DCF-Audio (per-`src` `AudioReassembler`) |
| `DATA` (0) | active | DCF-Text (per-`src` `TextReassembler`) — Messages |
| `DATA` (0) | `active ^ 1` | DCF-Game (per-`src` `GameReassembler`) — Arena |

Text and game both ride `DATA(0)` with no in-band tag (the documented foot-gun),
so the browser puts game on a derived sub-channel `active ^ 1` to keep the two
from colliding. A peer that wants to interoperate with the browser's Arena must
use the same convention; Messages interoperate on the active channel directly
(e.g. with `node JS/nodejs/src/node.js`).

## Capabilities (host-only features off in the browser)

`api.capabilities = { recording, radio, opus }`. The web build sets all three
`false`: multitrack **recording** and **Radio** HLS need a host ffmpeg pipeline,
and **Opus** is not in the WASM build. **Jam** runs **PCM-diag** end-to-end in
WASM via Web Audio (needs a secure context — serve over http(s)/localhost, not
`file://`, for mic access). Messages, Arena, and the Wire inspector work fully,
including from `file://` once the bridge admits that origin (`--allow-origin null`; see *Security*). The shared `App.vue` hides the disabled bits.

## Build & certify

```sh
nix develop .#wasm
cd web && npm install && npm run build      # → web/dist/index.html (wasm base64-inlined)
npm run certify                              # WASM byte-identical to the golden vectors
cargo run --manifest-path bridge/Cargo.toml -- --listen 127.0.0.1:7000
```

`web/certify/certify_wasm.mjs` checks the wasm-bindgen surface against the
committed adapter / superpack / fec / roundtrip vectors and the CRC anchors
(`crc("123456789")=0x29B1`, `crc(0^15)=0x4EC3`). The WASM links the same
`dcf-wire-codec` that `cd codec && cargo test --test certify` enforces against
the full 246-vector wire certificate, so byte-identity of the thin wrapper is
what this job pins. Drift fails CI like the native certs.

## Security

The DCF wire is plaintext by design (export compliance). The bridge adds no
crypto — **run it behind WireGuard** or an operator-supplied tunnel, exactly as
for any DCF node. See `DCF_SECURITY_EXPOSURE.md`.

The bridge is a UDP socket on the user's machine, and browsers do not apply the
same-origin rule to WebSockets. Before the two controls below, any web page the
user visited could open `ws://127.0.0.1:7000`, `addpeer` any `host:port`, and send
arbitrary bytes there: a general UDP sender for every website.

- **The gate.** Each datagram, both directions, is admitted only if it is a valid
  17-byte DeModFrame (sync, version 1, CRC) or a valid 32-byte SuperPack (sync,
  sflags `0x15`, joint CRC, both cores version 1). This is `DCF_MEDIUM_SPEC.md`'s
  `udp_bare` decode plus its frame gate. The gate's code is Exsecutor's
  `examples/custos`. The Exsecutor compiler builds it as one unit with the
  DeModFrame codec that passes Exsecutor's own run of the 246-vector certificate,
  then emits it as C (`web/bridge/custos/`, provenance in `PROVENANCE.md`).
  - **No authority.** It is pure under Exsecutor's capability rules: no
    `poscit`, no capability parameter. The compiler refuses a variant that
    draws ambient authority, with `EXS-E0421`.
  - **Certified.** `web/bridge/tests/certify_gate.rs` holds it to the committed
    golden, SuperPack and medium vectors, and to `dcf-wire-codec`'s
    `bare_decode` + `gate` over 14,735 further datagrams.
- **The origin check.** A WebSocket handshake is accepted only with no `Origin`
  (a non-browser client), from a loopback page (`http(s)` on `localhost`,
  `127.0.0.1` or `[::1]`), or from an origin named with `--allow-origin`.
  - **`null` is not loopback.** A `file://` page sends `Origin: null`, and so
    does a sandboxed iframe on any site. Admitting it reopens the hole, so it is
    opt-in (`--allow-origin null`).

Still open: `addpeer` resolves and accepts any `host:port`, so an admitted page
can still direct valid frames at any address. WebSocket TLS (`wss://`) under
the browser is fine and recommended; the plaintext DCF payload sits beneath it.

## Not yet (follow-ups)

- Opus Jam via the browser WebCodecs `AudioEncoder/Decoder` (gated by
  `capabilities.opus`).
- Hermetic Nix packages (`dcf-codec-wasm`, `dcf-web`, `dcf-ws-bridge`) need
  committed `Cargo.lock`s + vendor hashes; today the `.#wasm` dev shell drives
  the documented build.
- Per-peer RTT in the roster (the relay is connectionless; add PING/PONG in JS).
