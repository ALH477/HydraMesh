# DCF-Sense — configurable sensor telemetry over the DCF wire frame

DCF-Sense is a thin telemetry layer for many cheap sensor nodes reporting to a gateway
over a wired, audio-band link (HydraModem) — built for greenhouses (temp, humidity, soil
moisture, CO₂, light/PAR, pH, EC, …) but medium- and platform-agnostic. It is an **adapter +
media-access layer over the 17-byte `DeModFrame`**: it never changes the frame, so the
246-vector wire certificate is untouched.

Status: **Phase 1 done** (reading schema, TDMA/dedicated MAC, node + gateway, loopback demo,
tests) **+ the HydraModem transport** (Phase 2 start): DCF-Sense runs end-to-end over the real
modem at PER 0%. Remaining Phase 2–3 (FDMA/CSMA, mesh topology, MCU firmware, field test) are
planned.

## Reading schema (the efficient default)

**One reading = one bare `DeModFrame`** — zero adapter overhead:

| field | source |
|---|---|
| node id | the frame's `src_id` (u16) |
| gateway channel | the frame's `dst` |
| reading | the 4-byte payload: `[ sensor_type:u8 | value:i16 BE (scaled) | flags:u8 ]` |

Each sensor type has a fixed scale, so `physical = raw_i16 / SCALE[type]` (e.g. temp at
0.01 °C). Out-of-range values clamp and set `FLAG_CLAMPED`. Registry + codec:
`python/dcf/sense/schema.py` (`encode_reading` / `decode_reading`). A node reporting K
sensors sends K bare frames in its slot.

Efficiency options (Phase 2, layered on top, no schema change): pack two readings into one
datagram with **SuperPack**, or send a multi-sensor atomic bundle as an opaque **DCF-Game
`EVENT`** message (already-certified L2 fragmentation, no new vectors).

## Media access (DCF-MAC)

HydraModem is a PHY with no media access; a shared line needs collision avoidance. The MAC is
a thin scheduler *above* the transport (`python/dcf/sense/mac.py`), selected by config:

- **`dedicated`** — each node its own line into a multi-input gateway; no contention.
- **`tdma`** — a superframe of `num_slots` slots of `slot_dur` s; a node transmits only inside
  its (guarded) slot, so a shared line never collides. `slot = node_id % num_slots`. The
  `epoch` (superframe t0) is learned from the gateway `BEACON` in deployment; HydraModem's
  ±3000 ppm timing recovery absorbs the in-slot clock skew. **Best for many battery nodes.**
- **`fdma`** (Phase 2) — per-node HydraModem tone profile; nodes transmit concurrently, the
  gateway runs one decoder per channel. **`csma`** (Phase 2) — listen-before-talk + backoff.

## Topology

- **Star** (shared bus or dedicated lines) → nodes → one gateway.
- **Mesh** (Phase 3) → the DCF self-healing mesh (AUTO/master, RTT routing, failover) relays
  across a large/obstructed greenhouse — reuses `python/dcf/mesh_runtime.py`.

All selected by one config (`python/dcf/sense/config.py`: `topology`, `mac`, per-node sensors
+ cadence, gateway egress).

## Node & gateway

- **Node** (`node.py`): `read(node_id) -> {sensor: value}` → `encode_reading` → `transport.send`
  in the node's MAC slot, then sleep (low duty cycle). Runs over any DCF transport; core logic
  is portable so the same design ports to an MCU (Phase 3).
- **Gateway** (`gateway.py`): receives frames (HydraModem streaming RX is already per-`src_id`)
  → `decode_reading` → egress (callback / CSV; MQTT/sqlite later).

## Run it

```sh
# End-to-end demo over the in-process loopback medium (deterministic):
python3 python/dcf/sense/demo.py --nodes 4 --mac tdma --cycles 3 --csv /tmp/sense.csv

# End-to-end over REAL HydraModem (each reading -> M-FSK audio -> decoded back), via a
# shared dir = shared bus. Build the modem tools first:
hydramodem/dcf-tools/build.sh
python3 python/dcf/sense/demo.py --nodes 3 --mac tdma --cycles 2 --transport hydra

# Unit tests
cd python && python3 -m unittest tests.test_sense_schema tests.test_sense_mac -v
```

**HydraModem transport (Phase 2, shipped):** `HydraTransport` (`python/dcf/transport.py`,
factory `hydra:in=,out=,fec=`) drives HydraModem as a subprocess PHY via the single-frame
`frame_tx`/`frame_rx` tools (`hydramodem/dcf-tools/`). DCF-Sense over it is verified end-to-end
at PER 0% (file medium). A CFFI binding to `libhydramodem` is the next step (lower latency, and
needed for FDMA's concurrent decoders). Over the wire, reuse the HydraModem
`field-test.sh`/`duplex-test.sh` methodology to measure per-node PER and throughput.

## Wired-medium notes
- **Shared bus**: sum multiple line-outs into one line-in with a passive summing network and
  manage levels (clipping degrades decode — keep peaks ≈ −6 dBFS, per the HydraModem field
  tests).
- **Levels/clocks**: each interface has its own crystal; TDMA guard time + HydraModem timing
  recovery handle the skew.

## Honest framing
DCF-Sense reuses standard ideas (TDMA/FDMA media access, scaled-integer telemetry). The
contribution is the *integration*: efficient sensor telemetry as an adapter over the certified
DCF frame, carried by HydraModem, with a configurable MAC/topology — not a novel modem or MAC.
For wireless-at-distance sensors, RF (DCF-SDR/LoRa) is the better PHY than acoustic HydraModem.
