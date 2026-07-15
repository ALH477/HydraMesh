# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Integration tests: verify the full stack without network."""
from langgraph_agents.coordinator import build_coordinator_graph
from langgraph_agents.runner import AgentRunner
from langgraph_agents.streaming_bridge import chunk_response


def test_echo_agent_roundtrip():
    """Full roundtrip: message → runner → graph → response."""
    runner = AgentRunner(graph_backend="echo", mesh_url="", channel="test")
    response = runner.process_message("ping", "0x0001", "test")
    assert response == "ping"


def test_coordinator_echo_route():
    """Coordinator routes echo: prefix to echo specialist."""
    graph = build_coordinator_graph()
    result = graph.invoke({
        "messages": [{"role": "user", "content": "echo: test message"}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "echo"
    assert result["response"] == "test message"


def test_coordinator_summarizer_route():
    """Coordinator routes summarize: prefix to summarizer specialist."""
    graph = build_coordinator_graph()
    long_text = "summarize: " + "x" * 200
    result = graph.invoke({
        "messages": [{"role": "user", "content": long_text}],
        "channel": "test",
        "sender": "0x0002",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "summarizer"
    assert len(result["response"]) <= 103  # 100 + "..."


def test_streaming_chunks_fit_dcf_text():
    """All chunks must fit within DCF-Text per-message cap."""
    chunks = chunk_response("a" * 10000, max_chunk=4092)
    assert len(chunks) == 3
    assert all(len(c.encode("utf-8")) <= 4092 for c in chunks)


def test_llm_backend_echo():
    """Echo backend returns last user message."""
    from langgraph_agents.llm_node import get_backend
    backend = get_backend("echo")
    result = backend([{"role": "user", "content": "test"}])
    assert result == "test"


def test_llm_backend_unknown_raises():
    """Unknown backend name raises ValueError."""
    from langgraph_agents.llm_node import get_backend
    import pytest
    with pytest.raises(ValueError, match="Unknown LLM backend"):
        get_backend("nonexistent")
