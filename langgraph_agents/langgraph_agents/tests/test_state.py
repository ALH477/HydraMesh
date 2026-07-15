# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the shared AgentState TypedDict."""
from langgraph.graph import StateGraph

from langgraph_agents.state import AgentState


def test_state_graph_accepts_agent_state():
    """AgentState must be usable as a LangGraph StateGraph schema."""
    graph = StateGraph(AgentState)
    assert graph is not None


def test_state_has_required_fields():
    """All required fields must be declared."""
    hints = AgentState.__annotations__
    for field in ("messages", "channel", "sender", "routing_decision", "response", "metadata"):
        assert field in hints, f"missing field: {field}"
