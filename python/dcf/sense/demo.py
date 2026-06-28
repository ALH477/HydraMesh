# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense end-to-end demo: N simulated greenhouse sensor nodes report to one gateway
over a shared medium, governed by a configurable MAC. Defaults to the in-process
loopback medium (deterministic); the same node/gateway run unchanged over HydraModem or
any DCF transport.

  python3 python/dcf/sense/demo.py --nodes 4 --mac tdma --cycles 3
"""
import argparse
import os
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "MCP"))

from dcf import transport as T          # noqa: E402
from dcf.sense import schema, mac as macmod   # noqa: E402
from dcf.sense.node import SensorNode    # noqa: E402
from dcf.sense.gateway import Gateway    # noqa: E402

SENSORS = ["temp", "humidity", "soil_moisture", "co2"]


def synth_read(node_id):
    """Deterministic synthetic greenhouse readings (no RNG, so the demo is repeatable)."""
    n = node_id & 0x0F                       # small per-node index for plausible ranges
    return {
        "temp":          22.0 + 0.5 * n + 1.5 * ((n * 3) % 5) / 5.0,
        "humidity":      55.0 + (n * 7) % 20,
        "soil_moisture": 30.0 + (n * 11) % 25,
        "co2":           450 + 60 * ((n + 1) % 4),
    }


def main(argv=None):
    ap = argparse.ArgumentParser(prog="dcf-sense-demo")
    ap.add_argument("--nodes", type=int, default=4)
    ap.add_argument("--mac", choices=["tdma", "dedicated"], default="tdma")
    ap.add_argument("--cycles", type=int, default=3)
    ap.add_argument("--slot-dur", type=float, default=1.0)
    ap.add_argument("--csv", default=None, help="write decoded readings to this CSV")
    a = ap.parse_args(argv)

    mac = macmod.make_mac({"mode": a.mac, "num_slots": a.nodes,
                           "slot_dur": a.slot_dur, "guard": 0.05})

    medium = T.LoopbackMedium()
    gw = Gateway(T.LoopbackTransport("gw", medium), csv_path=a.csv).start()

    nodes = []
    for i in range(1, a.nodes + 1):
        nt = T.LoopbackTransport(f"node{i}", medium)
        nt.start(lambda f, m: None)                 # nodes only transmit
        nodes.append(SensorNode(0x1000 + i, nt, mac, synth_read,
                                gw_channel=0xFFFF, cadence=a.slot_dur))

    print(f"DCF-Sense demo — {a.nodes} nodes, MAC={a.mac}, {len(SENSORS)} sensors/node, "
          f"{a.cycles} cycles")
    if a.mac == "tdma":
        print("TDMA schedule (slot -> guarded window within a superframe):")
        for n in nodes:
            s, e = mac.window(n.slot)
            print(f"  node 0x{n.node_id:04x}  slot {n.slot}  [{s:.2f}, {e:.2f}] s")

    expected = 0
    for _ in range(a.cycles):
        for n in nodes:                              # report in slot order (no collision)
            expected += n.report_once(ts_us=int(time.monotonic() * 1e6) & 0xFFFFFF)
    # let the sender threads + gateway drain
    t0 = time.monotonic()
    while gw.decoded < expected and time.monotonic() - t0 < 5.0:
        time.sleep(0.02)

    per = 100.0 * (expected - gw.decoded) / expected if expected else 0.0
    print(f"\nreadings sent={expected}  decoded={gw.decoded}  PER={per:.2f}%")
    print("sample (last reading per node):")
    last = {}
    for r in gw.readings:
        last[r["node_id"]] = r
    for nid in sorted(last):
        seen = [x for x in gw.readings if x["node_id"] == nid]
        line = "  ".join(f"{x['sensor']}={x['value']:g}{x['unit']}"
                         for x in seen[-len(SENSORS):])
        print(f"  node 0x{nid:04x}: {line}")

    for n in nodes:
        n.transport.stop()
    gw.stop()
    return 0 if gw.decoded == expected else 1


if __name__ == "__main__":
    raise SystemExit(main())
