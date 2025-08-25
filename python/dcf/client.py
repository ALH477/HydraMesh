# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import asyncio
import enum
from .config import load_config
from .networking import Networking
from .redundancy import Redundancy
from .plugins import PluginManager
from .serialization import build_message
from . import messages_pb2
import json

class Mode(enum.Enum):
    CLIENT = "client"
    SERVER = "server"
    P2P = "p2p"
    AUTO = "auto"
    MASTER = "master"

class DCFClient:
    def __init__(self, config_path="config.json"):
        self.config = load_config(config_path)
        self.mode = Mode(self.config["mode"])
        self.net = Networking(self.config["transport"], self.config["host"], self.config["port"], self.config["mode"])
        self.plugins = PluginManager(self.config.get("plugins"))
        self.redundancy = None
        self.master = None
        self.loop = asyncio.get_event_loop()
        if self.mode == Mode.P2P:
            self.redundancy = Redundancy(self.config.get("peers", []), self.config.get("group_rtt_threshold", 50), self.net)
            self.loop.create_task(self.redundancy.monitor())
        elif self.mode == Mode.AUTO:
            self.loop.create_task(self.listen_for_master())
        elif self.mode == Mode.MASTER:
            self.master = DCFMaster(self.config)

    async def send_message(self, data, recipient):
        msg = build_message(data=data, recipient=recipient, sender=self.config["node_id"])
        resp = self.net.send(msg)
        return resp.data

    async def listen_for_master(self):
        if not self.net.stub:
            self.net.connect()
        try:
            async for msg in self.net.stub.ReceiveStream(messages_pb2.Empty()):
                if msg.command == "set_role":
                    self.mode = Mode(msg.role)
                    print(f"Switched to mode: {self.mode.value}")
                elif msg.command == "update_config":
                    self.config[msg.key] = msg.value
                    print(f"Updated config {msg.key} = {msg.value}")
                elif msg.command == "collect_metrics":
                    metrics = {"rtt": 10, "group_id": "group_0"} if self.redundancy else {}
                    resp = build_message(data=json.dumps(metrics), recipient=msg.sender)
                    self.net.send(resp)
        except grpc.RpcError:
            print("Stream error; retrying...")
            await asyncio.sleep(5)
            await self.listen_for_master()

    def start(self):
        if self.mode in [Mode.SERVER, Mode.P2P, Mode.MASTER]:
            pass

    def stop(self):
        self.net.stop_server()
        self.loop.stop()
