# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import asyncio
import time
import networkx as nx
from .utils import parse_peer

class Redundancy:
    def __init__(self, peers, threshold=50, net=None):
        self.peers = peers
        self.threshold = threshold
        self.net = net
        self.graph = nx.Graph()
        self.groups = {}
        self.lock = asyncio.Lock()

    async def measure_rtt(self, peer):
        start = time.perf_counter_ns()
        resp = await self.net.health_check(peer)
        rtt = (time.perf_counter_ns() - start) / 1e6
        return rtt if resp.healthy else float('inf')

    async def group_peers(self):
        async with self.lock:
            self.graph.clear()
            rtts = await asyncio.gather(*(self.measure_rtt(p) for p in self.peers))
            for peer, rtt in zip(self.peers, rtts):
                group_id = f"group_{int(rtt // self.threshold)}" if rtt < float('inf') else "isolated"
                self.groups[peer] = group_id
                self.graph.add_node(peer, rtt=rtt)
            for i, p1 in enumerate(self.peers):
                for p2 in self.peers[i+1:]:
                    weight = (self.graph.nodes[p1]['rtt'] + self.graph.nodes[p2]['rtt']) / 2
                    self.graph.add_edge(p1, p2, weight=weight)

    async def monitor(self):
        while True:
            await self.group_peers()
            for peer in list(self.peers):
                rtt = await self.measure_rtt(peer)
                if rtt == float('inf'):
                    async with self.lock:
                        if peer in self.graph:
                            self.graph.remove_node(peer)
            await asyncio.sleep(10)
