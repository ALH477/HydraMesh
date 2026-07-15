# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Echo specialist: returns the message verbatim (for testing)."""
from ..state import AgentState


def echo_process(state: AgentState) -> dict:
    """Process node: echo back the last user message content."""
    messages = state.get("messages", [])
    for msg in reversed(messages):
        # Handle both Message objects and dicts
        if hasattr(msg, "content"):
            content = msg.content
        elif isinstance(msg, dict):
            content = msg.get("content", "")
        else:
            content = str(msg)
        if content:
            text = content.removeprefix("echo: ").strip()
            return {"response": text}
    return {"response": ""}
