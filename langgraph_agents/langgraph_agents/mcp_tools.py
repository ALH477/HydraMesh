# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""LangChain Tool wrappers for DCF mesh MCP server tools.

These let LangGraph agent nodes call mesh_send, mesh_recv, mesh_status
as standard LangChain tools — the graph doesn't know or care that the
backend is MCP over HTTP; it just calls tool.invoke(args).
"""
from __future__ import annotations

import httpx
from langchain_core.tools import BaseTool
from pydantic import BaseModel, Field


class MeshSendInput(BaseModel):
    """Input schema for mesh_send tool."""
    text: str = Field(description="Text to send onto the DCF mesh")
    channel: str = Field(default="", description="Target channel (empty=broadcast)")


class MeshRecvInput(BaseModel):
    """Input schema for mesh_recv tool."""
    timeout_s: float = Field(default=5.0, description="Seconds to wait for a message")


class MeshStatusInput(BaseModel):
    """Input schema for mesh_status tool."""
    pass


class MeshSendTool(BaseTool):
    """Send text onto the DCF mesh via MCP."""
    name: str = "mesh_send"
    description: str = "Send text onto the DCF mesh. Returns confirmation with datagram count."
    args_schema: type[BaseModel] = MeshSendInput
    mesh_url: str = ""

    def _run(self, text: str, channel: str = "") -> str:
        resp = httpx.post(
            f"{self.mesh_url}/tools/mesh_send",
            json={"text": text, "channel": channel},
            timeout=10.0,
        )
        resp.raise_for_status()
        return resp.json().get("result", "sent")


class MeshRecvTool(BaseTool):
    """Receive messages from the DCF mesh via MCP."""
    name: str = "mesh_recv"
    description: str = "Block until a message arrives from the mesh (or timeout). Returns list of message dicts."
    args_schema: type[BaseModel] = MeshRecvInput
    mesh_url: str = ""

    def _run(self, timeout_s: float = 5.0) -> list[dict]:
        resp = httpx.post(
            f"{self.mesh_url}/tools/mesh_recv",
            json={"timeout_s": timeout_s},
            timeout=timeout_s + 5.0,
        )
        resp.raise_for_status()
        return resp.json().get("result", [])


class MeshStatusTool(BaseTool):
    """Query mesh endpoint status via MCP."""
    name: str = "mesh_status"
    description: str = "Report agent mesh endpoint info: node id, channel, peers, port."
    args_schema: type[BaseModel] = MeshStatusInput
    mesh_url: str = ""

    def _run(self) -> dict:
        resp = httpx.post(
            f"{self.mesh_url}/tools/mesh_status",
            json={},
            timeout=5.0,
        )
        resp.raise_for_status()
        return resp.json().get("result", {})
