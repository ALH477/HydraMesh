# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Base LangGraph agent graph: receive → route → process → respond.

The graph is the skeleton every agent shares. Specialist agents override
the 'route' and 'process' nodes by passing custom callables.
"""
from __future__ import annotations

from typing import Callable, Optional

from langgraph.graph import END, START, StateGraph

from .llm_node import get_backend
from .state import AgentState


def receive_node(state: AgentState) -> dict:
    """Receive node: validate and pass through."""
    if not state.get("messages"):
        return {"response": ""}
    return {}


def default_route(state: AgentState) -> dict:
    """Default router: always route to 'general' processing."""
    return {"routing_decision": "general"}


def make_process_node(backend_name: str) -> Callable:
    """Factory: returns a process node function bound to the given LLM backend."""
    backend = get_backend(backend_name)

    def process_node(state: AgentState) -> dict:
        messages = state.get("messages", [])
        msg_dicts = []
        for m in messages:
            # Handle both Message objects and dicts
            if hasattr(m, "content"):
                # Message object
                role = getattr(m, "type", getattr(m, "role", "user"))
                content = m.content
                msg_dicts.append({"role": role, "content": content})
            elif isinstance(m, dict):
                role = m.get("type", m.get("role", "user"))
                content = m.get("content", "")
                msg_dicts.append({"role": role, "content": content})
            else:
                msg_dicts.append({"role": "user", "content": str(m)})
        response = backend(msg_dicts)
        return {"response": response}

    return process_node


def respond_node(state: AgentState) -> dict:
    """Respond node: finalizes state. Actual send happens in the runner."""
    return {}


def build_agent_graph(
    llm_backend: str = "echo",
    route_fn: Optional[Callable] = None,
    process_fn: Optional[Callable] = None,
):
    """Build and compile the base agent graph.

    Args:
        llm_backend: name of the LLM backend ('echo', 'grok')
        route_fn: optional custom router (replaces default_route)
        process_fn: optional custom processor (replaces LLM-backed process)

    Returns:
        Compiled LangGraph graph.
    """
    graph = StateGraph(AgentState)
    graph.add_node("receive", receive_node)
    graph.add_node("route", route_fn or default_route)
    graph.add_node("process", process_fn or make_process_node(llm_backend))
    graph.add_node("respond", respond_node)

    graph.add_edge(START, "receive")
    graph.add_edge("receive", "route")
    graph.add_edge("route", "process")
    graph.add_edge("process", "respond")
    graph.add_edge("respond", END)

    return graph.compile()
