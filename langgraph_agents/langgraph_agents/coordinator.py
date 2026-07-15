# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Coordinator graph: routes messages to specialist subgraphs.

Routing is keyword-based (prefix matching). A future version
can use an LLM to classify and route.
"""
from __future__ import annotations

from langgraph.graph import END, START, StateGraph

from .specialists.echo import echo_process
from .specialists.summarizer import summarize_process
from .state import AgentState

ROUTE_MAP = {
    "echo:": "echo",
    "summarize:": "summarizer",
}

SPECIALISTS = {
    "echo": echo_process,
    "summarizer": summarize_process,
}


def coordinator_route(state: AgentState) -> dict:
    """Route based on message prefix."""
    messages = state.get("messages", [])
    for msg in reversed(messages):
        # Handle both Message objects and dicts
        if hasattr(msg, "content"):
            content = msg.content
        elif isinstance(msg, dict):
            content = msg.get("content", "")
        else:
            content = str(msg)
        for prefix, target in ROUTE_MAP.items():
            if content.startswith(prefix):
                return {"routing_decision": target}
    return {"routing_decision": "echo"}


def coordinator_dispatch(state: AgentState) -> dict:
    """Dispatch to the chosen specialist and capture its response."""
    target = state.get("routing_decision", "echo")
    specialist_fn = SPECIALISTS.get(target, echo_process)
    result = specialist_fn(state)
    return result


def build_coordinator_graph():
    """Build the supervisor coordinator graph."""
    graph = StateGraph(AgentState)
    graph.add_node("route", coordinator_route)
    graph.add_node("dispatch", coordinator_dispatch)
    graph.add_edge(START, "route")
    graph.add_edge("route", "dispatch")
    graph.add_edge("dispatch", END)
    return graph.compile()
