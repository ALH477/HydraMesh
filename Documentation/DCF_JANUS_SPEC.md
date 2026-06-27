# DCF-JANUS — STANAG-4748 transport for the DCF wire frame

DCF-JANUS carries the 17-byte `DeModFrame` over a **JANUS (NATO STANAG 4748)** acoustic
link. JANUS is the first internationally-ratified digital underwater acoustic comms
standard: frequency-hopping binary FSK (FH-BFSK, 13 orthogonal tone pairs), a convolutional
code + interleaver, a 64-bit baseline packet (→144 chips) after a 32-bit BFSK preamble, and a
variable-length **cargo** field for arbitrary data.

Like UDP/audio/SDR, JANUS is a **transport beneath the wire quantum** — it carries the frame
opaquely, so the 246-vector wire certificate is untouched. The frame rides as JANUS **cargo**.
The point is **interoperability with a standard**: a DCF mesh can exchange frames with real
JANUS gear (AUVs, naval/sensor nodes), not just other DCF nodes.

## Prior art (honest framing)
JANUS is an established standard with a **GPL-3.0 reference implementation** (CMRE; the
community fork used here is [`mission-systems-pty-ltd/janus-c`](https://github.com/mission-systems-pty-ltd/janus-c)).
This is *interop with a standard*, not a novel modem — that is the value.

## Architecture

`JanusTransport` (`python/dcf/transport.py`) subclasses `_DirMedium` exactly like
`AudioTransport`: TX writes one WAV per frame into `out_dir`, RX tails `in_dir` and decodes.
The two codec hooks **shell out to the GPL janus-c reference** as a separate process — the
encoder/decoder make the waveform STANAG-4748-compliant *by construction*:

- `_encode_file(frame, path)` → `janus-tx --stream-driver wav --stream-driver-args <path>
  --pset-id <id> --stream-fs <fs> --packet-cargo <frame.hex()>`
- `_decode_file(path)` → `janus-rx ... --verbose 1`, parse the recovered cargo, hex-decode → 17 bytes.

Notes:
- The frame is **hex-encoded** as cargo (34 ASCII chars) so arbitrary binary survives a CLI
  argument; cargo is null-padded by JANUS to a multiple of 8 bytes and trimmed back on decode.
- janus-c loads its **cargo codec plugins via `dlopen`** by bare name, so the plugins dir must
  be on `LD_LIBRARY_PATH`. The transport sets this automatically from the binary location
  (`../share/janus/plugins`), overridable via `$JANUS_PLUGINS`.
- Default profile: **parameter set 1** — the *Initial JANUS band* (center 11520 Hz, bandwidth
  4160 Hz) rendered at 48 kHz. Other sets (e.g. 2 = 1200 Hz lower-band) select via `pset_id`.
- Binaries are discovered via `$JANUS_TX`/`$JANUS_RX` or `PATH`; the parameter-set CSV via
  `$JANUS_PSET` or `../share/janus/etc/parameter_sets.csv`.

### Frame ↔ JANUS mapping
| DCF | JANUS |
|---|---|
| 17-byte `DeModFrame` (opaque) | packet **cargo** (hex-encoded, variable-length) |
| `class_id` / `app_type` (optional) | baseline packet class/app fields |
| `src`/`dst` (inside the frame) | not parsed by JANUS — DCF routes on them above the transport |
JANUS is one-shot (no ARQ); DCF's mesh dedup/forwarding handles delivery and loops.

## Licensing boundary (important)
janus-c is **GPL-3.0**; the DCF library is **LGPL-3.0**. DCF-JANUS therefore **never vendors or
links** janus-c — it invokes `janus-tx`/`janus-rx` as a **separate subprocess** (mere
aggregation, exactly like the existing `pw-play`/`ffmpeg` calls), so the LGPL library is
unaffected. janus-c is an **optional, user-installed GPL dependency**; the transport raises a
clear error (and the tests skip) when it's absent. Provided via a *separate* Nix derivation
kept out of every LGPL package's closure. See [`LICENSING.md`](../LICENSING.md).

## Build & run

```sh
nix build .#janus-c        # GPL reference: janus-tx / janus-rx (+ parameter sets, plugins)
nix develop .#janus        # the above on PATH, ready for the transport/tests

# DCF-over-JANUS round-trip (17-byte frame -> JANUS cargo WAV -> frame, byte-exact):
cd python && python3 -m unittest tests.test_transport -k janus -v
python3 -m unittest tests.test_bridge -k janus -v          # multi-hop janus -> sdr

# Compose a node with a JANUS link (a shared dir = the acoustic medium for loopback):
python3 python/dcf/bridge.py -t 'janus:in=/tmp/jrx,out=/tmp/jtx,pset=1' --text SOS --seconds 5

# Spot-check standard compliance with the bare reference:
janus-rx --stream-driver wav --stream-driver-args <tx.wav> --pset-id 1 --stream-fs 48000 --verbose 1
```

Without the janus-c binaries the `janus:` transport is unavailable (raises at construction)
and the JANUS tests skip — so CI stays green without the GPL dependency.

## Future
- **Live ALSA** PER campaign over real interfaces (`janus-c` has an ALSA stream driver; JANUS
  is underwater-band but runs over cable/air) — reuse the HydraModem `field-test.sh` method.
- A clean-room **LGPL native** PHY (`python/modem/janus.py`) to drop the GPL dependency.
- A **Faust** FH-BFSK JANUS PHY (build on the HydraModem Faust backend) — hedge any "first" claim.
