# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the coordinator graph."""
from langgraph_agents.coordinator import build_coordinator_graph


def test_coordinator_builds():
    graph = build_coordinator_graph()
    assert hasattr(graph, "invoke")


def test_coordinator_routes_echo():
    graph = build_coordinator_graph()
    result = graph.invoke({
        "messages": [{"role": "user", "content": "echo: hello"}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "echo"
    assert result["response"] == "hello"


def test_coordinator_routes_summarizer():
    graph = build_coordinator_graph()
    result = graph.invoke({
        "messages": [{"role": "user", "content": "summarize: " + "x" * 200}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "summarizer"
    assert len(result["response"]) <= 103  # 100 + "..."


def test_coordinator_default_route():
    """Unrecognized prefix defaults to echo."""
    graph = build_coordinator_graph()
    result = graph.invoke({
        "messages": [{"role": "user", "content": "random message"}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "echo"
    assert result["response"] == "random message"
