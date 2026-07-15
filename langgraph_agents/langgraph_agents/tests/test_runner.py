# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the agent runner."""
from langgraph_agents.runner import AgentRunner


def test_runner_init():
    runner = AgentRunner(
        graph_backend="echo",
        mesh_url="http://127.0.0.1:8765",
        channel="test",
    )
    assert runner.channel == "test"
    assert runner.mesh_url == "http://127.0.0.1:8765"


def test_runner_process_message():
    """Process a single inbound message through the graph and get a response."""
    runner = AgentRunner(graph_backend="echo", mesh_url="", channel="test")
    response = runner.process_message(
        text="hello mesh",
        sender="0x0001",
        channel="test",
    )
    assert response == "hello mesh"


def test_runner_process_empty_message():
    """Empty message should return empty response."""
    runner = AgentRunner(graph_backend="echo", mesh_url="", channel="test")
    response = runner.process_message(
        text="",
        sender="0x0001",
        channel="test",
    )
    assert response == ""


def test_runner_stop():
    """Stop should set _running to False."""
    runner = AgentRunner(graph_backend="echo", mesh_url="", channel="test")
    runner._running = True
    runner.stop()
    assert runner._running is False
