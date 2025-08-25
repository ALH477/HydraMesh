# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import grpc
import asyncio
from concurrent import futures
from . import services_pb2_grpc, messages_pb2
from .serialization import build_message

class DCFServiceImpl(services_pb2_grpc.DCFServiceServicer):
    def __init__(self, message_queue):
        self.message_queue = message_queue

    def SendMessage(self, request, context):
        return build_message(data=f"Echo: {request.data}")

    async def ReceiveStream(self, request, context):
        while True:
            msg = await self.message_queue.get()
            yield msg

    def HealthCheck(self, request, context):
        return messages_pb2.HealthResponse(healthy=True, status="OK")

class Networking:
    def __init__(self, transport, host, port, mode):
        self.transport = transport
        self.address = f"{host}:{port}"
        self.channel = None
        self.stub = None
        self.server = None
        self.message_queue = asyncio.Queue()
        if mode in ["server", "p2p", "master", "auto"]:
            self.start_server()

    def start_server(self):
        self.server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
        services_pb2_grpc.add_DCFServiceServicer_to_server(DCFServiceImpl(self.message_queue), self.server)
        self.server.add_insecure_port(self.address)
        self.server.start()

    def stop_server(self):
        if self.server:
            self.server.stop(grace=1).wait()

    def connect(self):
        self.channel = grpc.insecure_channel(self.address)
        self.stub = services_pb2_grpc.DCFServiceStub(self.channel)

    def send(self, msg):
        if not self.stub:
            self.connect()
        try:
            return self.stub.SendMessage(msg)
        except grpc.RpcError:
            raise ConnectionError("gRPC send failed")

    async def health_check(self, peer):
        if not self.stub:
            self.connect()
        try:
            return self.stub.HealthCheck(messages_pb2.HealthRequest(peer=peer))
        except grpc.RpcError:
            return messages_pb2.HealthResponse(healthy=False)