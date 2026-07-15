# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Universal MCP server: expose the agent system as MCP tools.

Any MCP client (Hermes, Claude, etc.) can connect and use:
  agent_chat    — send a message to an agent, get response
  agent_list    — list configured agents
  backend_list  — list available LLM backends
  provider_list — list available provider presets
  mesh_send     — send text onto the DCF mesh
  mesh_recv     — receive messages from the DCF mesh
  mesh_status   — query mesh endpoint status

Runs over stdio (standard MCP transport) or HTTP.

Encryption-free for export control purposes.
"""
from __future__ import annotations

import json
import logging
from typing import Any

import httpx
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import Tool, TextContent

from .agent_graph import build_agent_graph
from .coordinator import build_coordinator_graph
from .llm_node import list_backends, list_providers, register_from_config
from .streaming_bridge import chunk_response

logger = logging.getLogger("dcf-agent-mcp")


def create_mcp_server(config_path: str = "agents.jsonc", mesh_url: str = "") -> Server:
    """Build the MCP server with agent tools registered."""
    from .__main__ import load_config

    # Load config
    try:
        config = load_config(config_path)
        providers = config.get("providers", [])
        if providers:
            register_from_config(providers)
        agents = config.get("agents", [])
        default_mesh_url = mesh_url or config.get("mesh_server", {}).get(
            "url", "http://127.0.0.1:8765"
        )
    except Exception as e:
        logger.warning(f"Config load failed ({e}), using defaults")
        agents = []
        default_mesh_url = mesh_url or "http://127.0.0.1:8765"

    server = Server("dcf-agent")
    state = {"agents": agents, "mesh_url": default_mesh_url, "config_path": config_path}

    @server.list_tools()
    async def list_tools() -> list[Tool]:
        return [
            Tool(
                name="agent_chat",
                description="Send a message to a LangGraph agent and get a response. "
                            "Supports base and coordinator graphs with pluggable LLM backends.",
                inputSchema={
                    "type": "object",
                    "properties": {
                        "message": {"type": "string", "description": "Message text to send"},
                        "backend": {"type": "string", "description": "LLM backend name (echo, glm5p2, grok, etc.)", "default": "echo"},
                        "graph": {"type": "string", "description": "Graph type: base or coordinator", "default": "base"},
                        "channel": {"type": "string", "description": "DCF channel name", "default": "mcp"},
                        "sender": {"type": "string", "description": "Sender identity", "default": "mcp-client"},
                    },
                    "required": ["message"],
                },
            ),
            Tool(
                name="agent_list",
                description="List all configured agents from agents.jsonc.",
                inputSchema={"type": "object", "properties": {}},
            ),
            Tool(
                name="backend_list",
                description="List all available LLM backends and provider presets.",
                inputSchema={"type": "object", "properties": {}},
            ),
            Tool(
                name="provider_list",
                description="List all available provider presets (fireworks, openai, grok, ollama, etc.).",
                inputSchema={"type": "object", "properties": {}},
            ),
            Tool(
                name="mesh_send",
                description="Send text onto the DCF mesh via the mesh MCP server.",
                inputSchema={
                    "type": "object",
                    "properties": {
                        "text": {"type": "string", "description": "Text to send"},
                        "channel": {"type": "string", "description": "Target channel (empty=broadcast)", "default": ""},
                    },
                    "required": ["text"],
                },
            ),
            Tool(
                name="mesh_recv",
                description="Receive messages from the DCF mesh (blocking with timeout).",
                inputSchema={
                    "type": "object",
                    "properties": {
                        "timeout_s": {"type": "number", "description": "Seconds to wait", "default": 5.0},
                    },
                },
            ),
            Tool(
                name="mesh_status",
                description="Query DCF mesh endpoint status: node id, channel, peers, port.",
                inputSchema={"type": "object", "properties": {}},
            ),
        ]

    @server.call_tool()
    async def call_tool(name: str, arguments: dict) -> list[TextContent]:
        if name == "agent_chat":
            msg = arguments.get("message", "")
            backend = arguments.get("backend", "echo")
            graph_type = arguments.get("graph", "base")
            channel = arguments.get("channel", "mcp")
            sender = arguments.get("sender", "mcp-client")

            if graph_type == "coordinator":
                graph = build_coordinator_graph()
            else:
                graph = build_agent_graph(llm_backend=backend)

            result = graph.invoke({
                "messages": [{"role": "user", "content": msg}],
                "channel": channel,
                "sender": sender,
                "routing_decision": "",
                "response": "",
                "metadata": {},
            })
            response = result.get("response", "")
            chunks = chunk_response(response)
            return [TextContent(
                type="text",
                text=json.dumps({
                    "response": response,
                    "backend": backend,
                    "graph": graph_type,
                    "chunks": len(chunks),
                }),
            )]

        elif name == "agent_list":
            return [TextContent(type="text", text=json.dumps(state["agents"], indent=2))]

        elif name == "backend_list":
            return [TextContent(type="text", text=json.dumps({
                "backends": list_backends(),
                "providers": list_providers(),
            }, indent=2))]

        elif name == "provider_list":
            return [TextContent(type="text", text=json.dumps(list_providers(), indent=2))]

        elif name == "mesh_send":
            async with httpx.AsyncClient() as client:
                resp = await client.post(
                    f"{state['mesh_url']}/tools/mesh_send",
                    json={"text": arguments.get("text", ""), "channel": arguments.get("channel", "")},
                    timeout=10.0,
                )
                resp.raise_for_status()
                return [TextContent(type="text", text=json.dumps(resp.json()))]

        elif name == "mesh_recv":
            timeout_s = arguments.get("timeout_s", 5.0)
            async with httpx.AsyncClient() as client:
                resp = await client.post(
                    f"{state['mesh_url']}/tools/mesh_recv",
                    json={"timeout_s": timeout_s},
                    timeout=timeout_s + 5.0,
                )
                resp.raise_for_status()
                return [TextContent(type="text", text=json.dumps(resp.json()))]

        elif name == "mesh_status":
            async with httpx.AsyncClient() as client:
                resp = await client.post(
                    f"{state['mesh_url']}/tools/mesh_status",
                    json={},
                    timeout=5.0,
                )
                resp.raise_for_status()
                return [TextContent(type="text", text=json.dumps(resp.json().get("result", {})))]

        else:
            return [TextContent(type="text", text=f"Unknown tool: {name}")]

    return server


async def run_stdio(config_path: str = "agents.jsonc") -> None:
    """Run the MCP server over stdio."""
    server = create_mcp_server(config_path=config_path)
    async with stdio_server() as (read_stream, write_stream):
        await server.run(read_stream, write_stream, server.create_initialization_options())
