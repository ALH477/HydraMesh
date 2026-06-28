# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense energy + throughput model.

Grounds the "cost-effective / energy-efficient" question in numbers, from the *measured*
HydraModem frame airtime (default profile: 48 kHz, 1000 baud, 2-FSK). One DCF-Sense
reading = one 17-byte DeModFrame, so airtime/frame == airtime/reading.

Deployment assumption (the user's PoE+ XLR-form cable): nodes are POWERED, so the
headline metrics are **bus capacity** and **cost**, not battery energy — but we report
energy/reading too (it's low) for completeness. Comparisons to LoRa / RS-485 use the
documented constants below; treat them as order-of-magnitude, not lab-calibrated.

  python3 python/dcf/sense/model.py --fec conv --interval 60 --channels 1 --node-mw 70
"""
import argparse

# Measured HydraModem airtime per 17-byte frame (this repo, default profile) — see
# `frame_tx` WAV length / 48 kHz. Includes ~40 ms lead+tail guard silence.
AIRTIME_S = {"none": 0.232, "conv": 0.396, "rep3": 0.536}

WIRE_BYTES = 17        # one DeModFrame
READING_BYTES = 3      # useful sensor payload per reading (sensor_type u8 + value i16)

# Reference figures for the comparison table (documented assumptions).
LORA = {  # SF7/BW125, 14 dBm class
    "airtime_s": 0.046,     # ~46 ms for a short (~12 B) packet
    "tx_mw": 120.0,         # ~14 dBm draw incl. PA inefficiency
    "overhead_mj": 30.0,    # radio ramp + MCU awake per packet (typical)
    "node_bom_usd": 6.0,    # LoRa module
    "license": "ISM, duty-cycle limited (EU)",
    "range": "100s of m–km (RF)",
}
RS485 = {
    "baud": 115200,
    "xcvr_mw": 5.0,
    "node_bom_usd": 0.5,    # transceiver IC
    "license": "none (wired)",
    "range": "~1 km wired bus",
}


def model(fec="conv", interval_s=60.0, channels=1, node_mw=70.0,
          line_vrms=1.0, line_ohm=10000.0, guard_s=0.05):
    air = AIRTIME_S[fec]
    slot = air + guard_s
    readings_per_s_ch = 1.0 / air
    # capacity: a node reporting every `interval_s` needs the superframe <= interval.
    nodes_per_channel = max(1, int(interval_s // slot))
    total_nodes = nodes_per_channel * channels
    superframe_s = nodes_per_channel * slot
    duty = air / interval_s
    # energy/reading: SoC awake during airtime dominates; line drive is negligible.
    line_mw = (line_vrms ** 2) / line_ohm * 1000.0
    e_reading_mj = (node_mw + line_mw) * air
    # LoRa per-packet energy (TX + overhead), RS-485 per-reading energy.
    lora_mj = LORA["tx_mw"] * LORA["airtime_s"] + LORA["overhead_mj"]
    rs485_air = (WIRE_BYTES * 10) / RS485["baud"]          # 10 bits/byte (8N1)
    rs485_mj = (node_mw + RS485["xcvr_mw"]) * rs485_air
    return {
        "fec": fec, "airtime_s": air, "slot_s": slot,
        "readings_per_s_per_channel": readings_per_s_ch,
        "wire_bps": WIRE_BYTES * 8 / air, "goodput_Bps": READING_BYTES / air,
        "nodes_per_channel": nodes_per_channel, "channels": channels,
        "total_nodes": total_nodes, "superframe_s": superframe_s,
        "report_interval_s": interval_s, "latency_worstcase_s": superframe_s,
        "duty_cycle": duty, "line_mw": line_mw,
        "energy_per_reading_mj": e_reading_mj,
        "lora_energy_per_reading_mj": lora_mj, "rs485_energy_per_reading_mj": rs485_mj,
        "rs485_airtime_s": rs485_air,
    }


def _report(m, node_mw):
    print(f"DCF-Sense model — HydraModem {m['fec']} profile (measured)")
    print(f"  airtime/reading      {m['airtime_s']*1000:6.0f} ms   "
          f"(1 reading = 1 bare 17-byte frame)")
    print(f"  per-channel rate     {m['readings_per_s_per_channel']:6.2f} readings/s   "
          f"({m['wire_bps']:.0f} bit/s wire, {m['goodput_Bps']:.1f} useful B/s)")
    print(f"  CAPACITY @ {m['report_interval_s']:.0f}s report interval, "
          f"{m['channels']} channel(s) (FDMA):")
    print(f"     {m['nodes_per_channel']} nodes/channel  ->  {m['total_nodes']} nodes total")
    print(f"     superframe {m['superframe_s']:.1f}s = worst-case latency")
    print(f"  per-node duty cycle  {m['duty_cycle']*100:.3f}%   "
          f"(node awake {m['airtime_s']*1000:.0f} ms every {m['report_interval_s']:.0f}s)")
    print(f"  energy/reading       {m['energy_per_reading_mj']:6.1f} mJ   "
          f"(node {node_mw:.0f} mW awake; line drive {m['line_mw']:.3f} mW — negligible)")
    print()
    print("  vs alternatives (per reading; order-of-magnitude, see assumptions):")
    row = "     {:<12}{:>14}{:>16}   {:<26}"
    print(row.format("PHY", "energy/reading", "node BOM", "license"))
    print(row.format("HydraModem", f"{m['energy_per_reading_mj']:.1f} mJ",
                     "~$0 (audio out)", "none (wired)"))
    print(row.format("LoRa SF7", f"{m['lora_energy_per_reading_mj']:.1f} mJ",
                     f"~${LORA['node_bom_usd']:.0f}", LORA["license"]))
    print(row.format("RS-485", f"{m['rs485_energy_per_reading_mj']:.2f} mJ",
                     f"~${RS485['node_bom_usd']:.1f}", RS485["license"]))
    print()
    print("  Takeaways:")
    print("   - Cost: near-zero PHY BOM (software modem + an audio output); no radio/license.")
    print("   - Energy: TX is sub-watt and dominated by SoC awake time, not the line; with")
    print("     PoE+ powered nodes it isn't the constraint. RS-485 is similar energy + faster;")
    print("     audio-line's edge is no transceiver IC, one DCF stack across media, and the")
    print("     ability to ride media (transformer/AC-coupled/power-line) RS-485 cannot.")
    print("   - Capacity scales with FDMA channels; for wireless/range, use RF (DCF-SDR).")


def main(argv=None):
    ap = argparse.ArgumentParser(prog="dcf-sense-model")
    ap.add_argument("--fec", choices=list(AIRTIME_S), default="conv")
    ap.add_argument("--interval", type=float, default=60.0, help="report interval (s)")
    ap.add_argument("--channels", type=int, default=1, help="FDMA channels")
    ap.add_argument("--node-mw", type=float, default=70.0, help="node active power (mW)")
    a = ap.parse_args(argv)
    _report(model(a.fec, a.interval, a.channels, a.node_mw), a.node_mw)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
