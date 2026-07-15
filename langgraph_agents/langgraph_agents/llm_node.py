# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Pluggable LLM backends for agent graph process nodes.

Each backend is a callable: (messages: list[dict]) -> str
The 'echo' backend mirrors input (for testing). Real backends call APIs.
"""
from __future__ import annotations

import os
from typing import Callable


def echo_backend(messages: list[dict]) -> str:
    """Test backend: returns the last user message content verbatim."""
    for msg in reversed(messages):
        # Handle both Message objects and dicts
        if hasattr(msg, "content"):
            content = msg.content
        else:
            content = msg.get("content", "")
        if content:
            return content
    return ""


def grok_backend(messages: list[dict]) -> str:
    """Grok (xAI) chat completion. Requires GROK_API_KEY env."""
    import httpx
    api_key = os.environ.get("GROK_API_KEY", "")
    if not api_key:
        raise ValueError("GROK_API_KEY not set")
    resp = httpx.post(
        "https://api.x.ai/v1/chat/completions",
        headers={"Authorization": f"Bearer {api_key}"},
        json={"model": "grok-beta", "messages": messages, "stream": False},
        timeout=30.0,
    )
    resp.raise_for_status()
    return resp.json()["choices"][0]["message"]["content"]


BACKENDS: dict[str, Callable[[list[dict]], str]] = {
    "echo": echo_backend,
    "grok": grok_backend,
}


def get_backend(name: str) -> Callable[[list[dict]], str]:
    """Look up a backend by name. Raises ValueError for unknown names."""
    if name not in BACKENDS:
        raise ValueError(f"Unknown LLM backend: {name!r}. Available: {list(BACKENDS)}")
    return BACKENDS[name]
