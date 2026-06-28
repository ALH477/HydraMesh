# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense network builder — one SenseConfig constructs the whole system (gateway +
sensor nodes + MAC) over a chosen transport. This is the "configurable system" entry
point: pick topology + MAC + per-node sensors in config, supply a `read` callback and a
transport factory, get a runnable Network.

Star topology today (shared-bus or dedicated, by transport factory); mesh is Phase 3.
"""
import os
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))

from dcf import transport as T          # noqa: E402
from .mac import make_mac               # noqa: E402
from .node import SensorNode            # noqa: E402
from .gateway import Gateway            # noqa: E402


class Network:
    """A built DCF-Sense network: one gateway + N sensor nodes sharing a medium."""

    def __init__(self, gateway, nodes):
        self.gateway = gateway
        self.nodes = nodes

    def run_cycles(self, cycles=1):
        """Each node reports `cycles` times, in MAC-slot order (collision-free). Returns
        the number of readings transmitted."""
        sent = 0
        for _ in range(cycles):
            for n in sorted(self.nodes, key=lambda x: x.slot):
                sent += n.report_once(ts_us=int(time.monotonic() * 1e6) & 0xFFFFFF)
        return sent

    def stop(self):
        for n in self.nodes:
            n.transport.stop()
        self.gateway.stop()


# ── transport factories ───────────────────────────────────────────────────────
def loopback_factory():
    """In-process shared medium (deterministic) — returns factory(role, name)."""
    medium = T.LoopbackMedium()
    def factory(role, name):
        return T.LoopbackTransport(name, medium)
    return factory


def hydra_factory(link_dir):
    """Real HydraModem over a shared dir (= shared bus). Gateway reads, nodes write."""
    def factory(role, name):
        if role == "gateway":
            return T.HydraTransport(name, in_dir=link_dir)
        return T.HydraTransport(name, out_dir=link_dir)
    return factory


# ── builder ───────────────────────────────────────────────────────────────────
def build_network(cfg, read, transport_factory=None):
    """Build a Network from a SenseConfig.

      cfg               : SenseConfig (topology / mac / gw_channel / egress / nodes)
      read(node_id)     : -> {sensor_name|id: value} for that node
      transport_factory : (role, name) -> Transport;  defaults to loopback
    """
    if transport_factory is None:
        transport_factory = loopback_factory()
    if cfg.topology not in ("star",):
        raise ValueError(f"topology {cfg.topology!r} not supported yet (star; mesh = Phase 3)")

    mac = make_mac(cfg.mac)
    gateway = Gateway(transport_factory("gateway", "gw"),
                      csv_path=(cfg.egress or {}).get("csv")).start()

    nodes = []
    for nc in cfg.nodes:
        nt = transport_factory("node", f"node-{nc.node_id:#06x}")
        nt.start(lambda f, m: None)                       # nodes only transmit
        allow = set(nc.sensors)                            # per-node sensor filter
        def node_read(nid, _read=read, _allow=allow):
            r = _read(nid)
            return {k: v for k, v in r.items() if not _allow or k in _allow}
        nodes.append(SensorNode(nc.node_id, nt, mac, node_read,
                                gw_channel=cfg.gw_channel, cadence=nc.cadence))
    return Network(gateway, nodes)
