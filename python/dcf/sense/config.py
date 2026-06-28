# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense unified config — one description drives both nodes and the gateway:
topology (star-shared / star-dedicated / mesh), media access (tdma/dedicated/...),
the per-node sensor list + cadence, and the gateway egress. Loadable from a dict
(JSON/TOML at the edges)."""
from dataclasses import dataclass, field


@dataclass
class NodeCfg:
    node_id: int
    sensors: list = field(default_factory=list)   # sensor names (or ids)
    cadence: float = 1.0                            # seconds between reports


@dataclass
class SenseConfig:
    topology: str = "star"                          # star | mesh
    mac: dict = field(default_factory=lambda: {"mode": "tdma", "num_slots": 1,
                                               "slot_dur": 1.0, "guard": 0.05})
    gw_channel: int = 0xFFFF                         # DeModFrame dst the nodes report on
    egress: dict = field(default_factory=dict)       # {csv: path, ...}
    nodes: list = field(default_factory=list)        # list[NodeCfg]

    @staticmethod
    def from_dict(d):
        nodes = [NodeCfg(**n) if not isinstance(n, NodeCfg) else n
                 for n in d.get("nodes", [])]
        return SenseConfig(
            topology=d.get("topology", "star"),
            mac=d.get("mac", {"mode": "tdma", "num_slots": max(1, len(nodes)),
                              "slot_dur": 1.0, "guard": 0.05}),
            gw_channel=int(d.get("gw_channel", 0xFFFF)),
            egress=d.get("egress", {}),
            nodes=nodes,
        )
