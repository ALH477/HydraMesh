# DCF-Sense — configurable sensor telemetry over the DCF wire frame

DCF-Sense is a thin telemetry layer for many cheap sensor nodes reporting to a gateway
over a wired, audio-band link (HydraModem) — built for greenhouses (temp, humidity, soil
moisture, CO₂, light/PAR, pH, EC, …) but medium- and platform-agnostic. It is an **adapter +
media-access layer over the 17-byte `DeModFrame`**: it never changes the frame, so the
246-vector wire certificate is untouched.

Status: **Phases 1–2 done.** Schema, MAC (`tdma`/`dedicated`/`csma`/`fdma`), node + gateway,
config-driven `build_network`, energy/throughput model, the HydraModem transport (subprocess
`HydraTransport` + in-process ctypes `HydraCffiTransport`), FDMA per-node tone channels, mesh
relay via `dcf.bridge.Bridge`, and a portable C MCU node skeleton — all tested (DCF-Sense over
the real modem at PER 0%; a C node's reading decodes in the Python gateway). **Remaining: the
physical cabled multi-node field run** (needs hardware — runbook below) and MCU firmware ports.

## Cost, energy & throughput (measured model)

`python/dcf/sense/model.py` computes capacity/energy from the *measured* HydraModem airtime
(default conv profile: **396 ms / 17-byte frame = 1 reading**):

- **Capacity:** ~2.5 readings/s per channel → **~134 nodes per channel** at a 60 s report
  interval (TDMA); FDMA channels multiply that (e.g. 8 ch ≈ 500+ nodes). Worst-case latency =
  the superframe (≈ the report interval).
- **Duty cycle:** ~0.66% (a node is active 396 ms every 60 s).
- **Energy:** ~28 mJ/reading, dominated by the node SoC being awake — the line drive itself is
  **sub-milliwatt** (≈0.1 mW into a line input). For comparison: LoRa ≈ 35 mJ + a ~$6 radio;
  RS-485 ≈ 0.1 mJ but needs a transceiver IC.
- **Honest takeaway:** the wins are **near-zero PHY BOM** (a software modem + an audio output;
  no radio, no license, no spectrum) and **EMI robustness** on a noisy wired bus — *not* raw
  energy. With **PoE+ powered nodes (see cable, below) energy isn't the constraint**; capacity
  and cost are. For battery + wireless-at-distance, use RF (DCF-SDR), not acoustic.

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
- **`fdma`** — each node on a distinct HydraModem tone channel (`base_freq = base0 +
  k·spacing`, integer-cycle so tones stay orthogonal); nodes transmit concurrently and the
  gateway runs one decoder per channel. Verified: a ch-0 signal is *rejected* by a ch-1 decoder.
  Capacity multiplies by the channel count (see the model). `Fdma.profile_of(channel)` feeds the
  HydraModem transport's `base_freq/tone_spacing/baud/n_tones`.
- **`csma`** — opportunistic + exponential backoff (real carrier-sense needs the live medium).

## Topology

- **Star** (shared bus or dedicated lines) → nodes → one gateway.
- **Mesh** → a DCF-Sense reading is just a `DeModFrame`, so the existing **`dcf.bridge.Bridge`**
  relays it multi-hop across media (flood/egress + dedup) — verified end-to-end (a reading
  crosses a 2-hop relay and decodes at the far gateway). For self-healing roles/failover,
  layer `python/dcf/mesh_runtime.py` underneath.

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

# Cost/energy/throughput model (capacity, J/reading, vs LoRa/RS-485):
python3 python/dcf/sense/model.py --fec conv --interval 60 --channels 1 --node-mw 70

# Config-driven: one SenseConfig -> gateway + nodes (build_network), any transport.
# Unit tests
cd python && python3 -m unittest tests.test_sense_schema tests.test_sense_mac \
                                 tests.test_sense_model tests.test_sense_network -v
```

**Config-driven builder** (`network.build_network`): a `SenseConfig` (topology / MAC /
per-node sensors + cadence / egress) plus a `read` callback and a transport factory
(`loopback_factory()` or `hydra_factory(dir)`) yields a runnable `Network` — verified end-to-end
over both loopback and real HydraModem. MAC modes: `tdma`, `dedicated`, `csma` (FDMA pending).

**HydraModem transport (Phase 2, shipped):** `HydraTransport` (`python/dcf/transport.py`,
factory `hydra:in=,out=,fec=`) drives HydraModem as a subprocess PHY via the single-frame
`frame_tx`/`frame_rx` tools (`hydramodem/dcf-tools/`). DCF-Sense over it is verified end-to-end
at PER 0% (file medium). A CFFI binding to `libhydramodem` is the next step (lower latency, and
needed for FDMA's concurrent decoders). Over the wire, reuse the HydraModem
`field-test.sh`/`duplex-test.sh` methodology to measure per-node PER and throughput.

## Wired-medium notes
- **The cable (target deployment):** a shielded combo cable — CAT5e + a 1/4" instrument
  conductor in an XLR-style connector — carrying **PoE+ power *and* the analog telemetry line**
  on one run. So nodes are *powered* (energy is not the constraint) and the HydraModem signal
  rides the instrument conductor; the CAT5e shield + twisted pairs reject the greenhouse's EMI.
  (A PSU instead of PoE+ works for bench/fixed nodes.)
- **Shared bus**: sum multiple line-outs into one line-in with a passive summing network and
  manage levels (clipping degrades decode — keep peaks ≈ −6 dBFS, per the HydraModem field
  tests).
- **Levels/clocks**: each interface has its own crystal; TDMA guard time + HydraModem timing
  recovery handle the skew.

## Platform-agnostic node (C / MCU)

`hydramodem/dcf-tools/sense_node.c` is a portable C reference node: read sensor → pack a
DCF-Sense reading into a `DeModFrame` (the repo wire codec) → HydraModem TX → emit samples.
An MCU port only implements three hooks — `sensor_read` (ADC/I2C), `sample_sink` (DAC/line-out),
`node_sleep` (low-power) — everything else is portable. On the host it writes a WAV per reading;
because it builds the frame with the same codec, **a C node's WAV decodes in the Python gateway**
(verified). The C core targets MCUs (HydraModem cross-compiles to RISC-V); flashable firmware
ports are the remaining hardware work.

## Field runbook (cabled multi-node, needs hardware)

1. Build: `hydramodem/dcf-tools/build.sh`; `export HYDRA_TX/HYDRA_RX` (or `HYDRAMODEM_LIB` for
   the in-process codec).
2. Wire each node's line-out (instrument conductor of the PoE+ combo cable) into the gateway
   interface's line-in. **Shared bus:** passive summing network; keep the summed peak ≈ −6 dBFS.
   **FDMA:** assign each node a channel (`Fdma.profile_of`), gateway runs N decoders.
3. Pick the MAC in config: `tdma` (many nodes, one channel) or `fdma` (concurrent, N channels).
4. Run the gateway over `HydraCffiTransport` (or `HydraTransport`) on the line-in device; run
   nodes (SBC: `build_network` + a node loop; MCU: `sense_node` port) on their line-out devices.
5. Measure **per-node PER and throughput** over ≥100 readings/node, reusing the HydraModem
   `field-test.sh`/`duplex-test.sh` methodology (level → single-frame proof → campaign). Pass:
   PER < 1% with conv FEC at nominal level.

## Honest framing
DCF-Sense reuses standard ideas (TDMA/FDMA media access, scaled-integer telemetry). The
contribution is the *integration*: efficient sensor telemetry as an adapter over the certified
DCF frame, carried by HydraModem, with a configurable MAC/topology — not a novel modem or MAC.
For wireless-at-distance sensors, RF (DCF-SDR/LoRa) is the better PHY than acoustic HydraModem.
