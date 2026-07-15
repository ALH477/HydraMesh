# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for MCP tool wrappers."""
from langgraph_agents.mcp_tools import MeshRecvTool, MeshSendTool, MeshStatusTool


def test_mesh_send_tool_has_correct_name():
    tool = MeshSendTool(mesh_url="http://127.0.0.1:8765")
    assert tool.name == "mesh_send"


def test_mesh_recv_tool_has_correct_name():
    tool = MeshRecvTool(mesh_url="http://127.0.0.1:8765")
    assert tool.name == "mesh_recv"


def test_mesh_status_tool_has_correct_name():
    tool = MeshStatusTool(mesh_url="http://127.0.0.1:8765")
    assert tool.name == "mesh_status"


def test_mesh_send_tool_description():
    tool = MeshSendTool(mesh_url="http://127.0.0.1:8765")
    assert "mesh" in tool.description.lower()


def test_mesh_recv_tool_description():
    tool = MeshRecvTool(mesh_url="http://127.0.0.1:8765")
    assert "message" in tool.description.lower()
