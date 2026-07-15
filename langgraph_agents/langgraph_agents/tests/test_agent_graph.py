# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the base agent graph."""
from langgraph_agents.agent_graph import build_agent_graph


def test_build_graph_returns_compiled():
    graph = build_agent_graph(llm_backend="echo")
    assert hasattr(graph, "invoke")


def test_graph_has_four_nodes():
    graph = build_agent_graph(llm_backend="echo")
    node_names = set(graph.get_graph().nodes.keys())
    for expected in ("receive", "route", "process", "respond"):
        assert expected in node_names, f"missing node: {expected}"


def test_echo_graph_roundtrip():
    """With echo backend, response should mirror the inbound message."""
    graph = build_agent_graph(llm_backend="echo")
    result = graph.invoke({
        "messages": [{"role": "user", "content": "hello mesh"}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["response"] == "hello mesh"


def test_graph_with_empty_messages():
    """Graph should handle empty messages gracefully."""
    graph = build_agent_graph(llm_backend="echo")
    result = graph.invoke({
        "messages": [],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["response"] == ""
