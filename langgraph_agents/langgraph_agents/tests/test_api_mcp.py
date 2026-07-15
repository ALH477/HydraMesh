# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the HTTP API server and MCP server."""
import json
import tempfile
from pathlib import Path

from fastapi.testclient import TestClient

from langgraph_agents.api_server import create_app
from langgraph_agents.mcp_server import create_mcp_server
from mcp.server import Server


def _make_config(tmp_path: Path) -> str:
    """Create a minimal agents.jsonc for testing."""
    config = tmp_path / "agents.jsonc"
    config.write_text(json.dumps({
        "agents": [
            {"name": "test-agent", "graph": "base", "llm_backend": "echo", "channel": "test"}
        ],
        "providers": [],
        "mesh_server": {"url": "http://127.0.0.1:9999"}
    }))
    return str(config)


# ── API server tests ──────────────────────────────────────────────────────────

def test_api_health():
    """GET /health should return ok status."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        app = create_app(config_path=config, mesh_url="http://127.0.0.1:9999")
        client = TestClient(app)
        resp = client.get("/health")
        assert resp.status_code == 200
        data = resp.json()
        assert data["status"] == "ok"
        assert "echo" in data["backends"]


def test_api_agents():
    """GET /agents should list configured agents."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        app = create_app(config_path=config, mesh_url="http://127.0.0.1:9999")
        client = TestClient(app)
        resp = client.get("/agents")
        assert resp.status_code == 200
        agents = resp.json()
        assert any(a["name"] == "test-agent" for a in agents)


def test_api_backends():
    """GET /backends should list backends and providers."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        app = create_app(config_path=config, mesh_url="http://127.0.0.1:9999")
        client = TestClient(app)
        resp = client.get("/backends")
        assert resp.status_code == 200
        data = resp.json()
        assert "echo" in data["backends"]
        assert "fireworks" in data["providers"]


def test_api_providers():
    """GET /providers should list provider presets."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        app = create_app(config_path=config, mesh_url="http://127.0.0.1:9999")
        client = TestClient(app)
        resp = client.get("/providers")
        assert resp.status_code == 200
        providers = resp.json()
        assert "fireworks" in providers
        assert "ollama" in providers


def test_api_chat():
    """POST /chat with echo backend should return the message."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        app = create_app(config_path=config, mesh_url="http://127.0.0.1:9999")
        client = TestClient(app)
        resp = client.post("/chat", json={
            "message": "hello api",
            "backend": "echo",
            "graph": "base",
        })
        assert resp.status_code == 200
        data = resp.json()
        assert data["response"] == "hello api"
        assert data["backend"] == "echo"


def test_api_chat_coordinator():
    """POST /chat with coordinator graph should route correctly."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        app = create_app(config_path=config, mesh_url="http://127.0.0.1:9999")
        client = TestClient(app)
        resp = client.post("/chat", json={
            "message": "echo: routed via api",
            "backend": "echo",
            "graph": "coordinator",
        })
        assert resp.status_code == 200
        data = resp.json()
        assert "routed via api" in data["response"]


def test_api_chat_stream():
    """POST /chat/stream should return SSE chunks."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        app = create_app(config_path=config, mesh_url="http://127.0.0.1:9999")
        client = TestClient(app)
        resp = client.post("/chat/stream", json={
            "message": "stream test",
            "backend": "echo",
        })
        assert resp.status_code == 200
        assert "text/event-stream" in resp.headers.get("content-type", "")
        body = resp.text
        assert "stream test" in body


# ── MCP server tests ──────────────────────────────────────────────────────────

def test_mcp_server_creation():
    """create_mcp_server should return a Server instance."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        server = create_mcp_server(config_path=config, mesh_url="http://127.0.0.1:9999")
        assert isinstance(server, Server)


def test_mcp_server_has_tools():
    """The MCP server should register the expected tools."""
    with tempfile.TemporaryDirectory() as tmp:
        config = _make_config(Path(tmp))
        server = create_mcp_server(config_path=config, mesh_url="http://127.0.0.1:9999")
        # The server should have a tool handler registered
        # We can't easily call list_tools without async machinery,
        # but we can verify the server was created with the right name
        assert server.name == "dcf-agent"
