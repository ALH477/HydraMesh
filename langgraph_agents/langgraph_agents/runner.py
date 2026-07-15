# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Agent runner: ties a LangGraph agent to the DCF mesh via MCP.

Polls mesh_recv for inbound messages, feeds each through the compiled
graph, and sends the response back via mesh_send. Runs as a long-lived
asyncio task or a single-shot processor.
"""
from __future__ import annotations

import asyncio
import logging
from typing import Optional

import httpx

from .agent_graph import build_agent_graph

logger = logging.getLogger(__name__)


class AgentRunner:
    """Runs a single LangGraph agent against the mesh."""

    def __init__(
        self,
        graph_backend: str = "echo",
        mesh_url: str = "http://127.0.0.1:8765",
        channel: str = "agent",
        poll_interval: float = 1.0,
    ):
        self.channel = channel
        self.mesh_url = mesh_url
        self.poll_interval = poll_interval
        self.graph = build_agent_graph(llm_backend=graph_backend)
        self._running = False

    def process_message(self, text: str, sender: str, channel: str) -> str:
        """Run one message through the graph synchronously. Returns the response text."""
        result = self.graph.invoke({
            "messages": [{"role": "user", "content": text}],
            "channel": channel,
            "sender": sender,
            "routing_decision": "",
            "response": "",
            "metadata": {},
        })
        return result.get("response", "")

    async def _fetch_messages(self, timeout_s: float = 5.0) -> list[dict]:
        """Call mesh_recv on the MCP server."""
        if not self.mesh_url:
            return []
        try:
            async with httpx.AsyncClient() as client:
                resp = await client.post(
                    f"{self.mesh_url}/tools/mesh_recv",
                    json={"timeout_s": timeout_s},
                    timeout=timeout_s + 5.0,
                )
                resp.raise_for_status()
                return resp.json().get("result", [])
        except Exception as e:
            logger.warning(f"mesh_recv failed: {e}")
            return []

    async def _send_response(self, text: str) -> None:
        """Call mesh_send on the MCP server."""
        if not self.mesh_url:
            logger.info(f"[dry-run] would send: {text}")
            return
        try:
            async with httpx.AsyncClient() as client:
                resp = await client.post(
                    f"{self.mesh_url}/tools/mesh_send",
                    json={"text": text},
                    timeout=10.0,
                )
                resp.raise_for_status()
        except Exception as e:
            logger.error(f"mesh_send failed: {e}")

    async def run_loop(self) -> None:
        """Main loop: poll → process → respond, forever."""
        self._running = True
        logger.info(f"Agent runner started on channel {self.channel!r}")
        while self._running:
            messages = await self._fetch_messages(timeout_s=self.poll_interval)
            for msg in messages:
                text = msg.get("text", "")
                sender = msg.get("src", "unknown")
                channel = msg.get("channel", self.channel)
                if not text:
                    continue
                logger.info(f"Processing from {sender}: {text[:80]}")
                response = self.process_message(text, sender, channel)
                if response:
                    await self._send_response(response)
                    logger.info(f"Responded: {response[:80]}")
            await asyncio.sleep(0.1)

    def stop(self) -> None:
        """Signal the run loop to stop."""
        self._running = False
