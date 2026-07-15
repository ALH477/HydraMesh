# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""CLI entry point: python -m langgraph_agents [config_path]

Launches all agents declared in the config, each in its own asyncio task.
Also starts the mesh_mcp.py server as a subprocess if configured.
"""
from __future__ import annotations

import asyncio
import json
import logging
import re
import subprocess
import sys
from pathlib import Path

from .coordinator import build_coordinator_graph
from .runner import AgentRunner

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(name)s] %(message)s")
logger = logging.getLogger("langgraph-agents")


def strip_jsonc(text: str) -> str:
    """Strip // and /* */ comments, allow trailing commas.

    Only strips // comments that start a line (after optional whitespace)
    to avoid eating // inside string values like URLs.
    """
    text = re.sub(r'(?m)^\s*//.*$', '', text)
    text = re.sub(r'/\*.*?\*/', '', text, flags=re.DOTALL)
    text = re.sub(r',\s*([}\]])', r'\1', text)
    return text


def load_config(path: str) -> dict:
    """Load and parse a JSONC config file."""
    raw = Path(path).read_text()
    return json.loads(strip_jsonc(raw))


def build_runner(agent_cfg: dict) -> AgentRunner:
    """Construct an AgentRunner from a config block."""
    graph_type = agent_cfg.get("graph", "base")
    runner = AgentRunner(
        graph_backend=agent_cfg.get("llm_backend", "echo"),
        mesh_url=agent_cfg.get("mesh_url", ""),
        channel=agent_cfg.get("channel", "agent"),
        poll_interval=agent_cfg.get("poll_interval", 1.0),
    )
    if graph_type == "coordinator":
        runner.graph = build_coordinator_graph()
    return runner


async def main(config_path: str = "agents.jsonc") -> None:
    """Main entry: load config, optionally start mesh server, launch agents."""
    config = load_config(config_path)

    # Optionally start mesh_mcp.py as a subprocess
    mesh_srv = config.get("mesh_server")
    mesh_proc = None
    if mesh_srv:
        cmd = [mesh_srv["command"]] + mesh_srv.get("args", [])
        logger.info(f"Starting mesh MCP server: {' '.join(cmd)}")
        mesh_proc = subprocess.Popen(cmd)
        await asyncio.sleep(1.0)

    # Launch all agents
    runners = [build_runner(a) for a in config.get("agents", [])]
    tasks = [asyncio.create_task(r.run_loop()) for r in runners]
    logger.info(f"Launched {len(tasks)} agent(s)")

    try:
        await asyncio.gather(*tasks)
    except KeyboardInterrupt:
        logger.info("Shutting down...")
        for r in runners:
            r.stop()
    finally:
        if mesh_proc:
            mesh_proc.terminate()


if __name__ == "__main__":
    config_file = sys.argv[1] if len(sys.argv) > 1 else "agents.jsonc"
    asyncio.run(main(config_file))
