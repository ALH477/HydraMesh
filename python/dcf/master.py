# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
from .networking import Networking
from .serialization import build_message
from .utils import parse_peer
import json

class DCFMaster:
    def __init__(self, config):
        self.config = config
        self.clients = {}
        for peer in config.get("peers", []):
            host, port = parse_peer(peer)
            self.clients[peer] = Networking("gRPC", host, int(port), "master")

    def assign_role(self, peer, role):
        if peer not in self.clients:
            raise ValueError(f"Unknown peer: {peer}")
        msg = build_message(command="set_role", role=role, recipient=peer)
        self.clients[peer].send(msg)

    def update_config(self, peer, key, value):
        msg = build_message(command="update_config", key=key, value=value, recipient=peer)
        self.clients[peer].send(msg)

    def collect_metrics(self):
        metrics = {}
        for peer in self.clients:
            msg = build_message(command="collect_metrics", recipient=peer)
            resp = self.clients[peer].send(msg)
            try:
                metrics[peer] = json.loads(resp.data)
            except json.JSONDecodeError:
                metrics[peer] = {}
        return metrics

    def optimize_network(self):
        metrics = self.collect_metrics()
        avg_rtt = sum(m.get("rtt", 0) for m in metrics.values()) / max(1, len(metrics))
        if avg_rtt > self.config.get("group_rtt_threshold", 50):
            print("Optimizing: High RTT detected")
