# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Summarizer specialist: truncates long messages to a summary."""
from ..state import AgentState

MAX_SUMMARY_LEN = 100


def summarize_process(state: AgentState) -> dict:
    """Process node: truncate long messages to MAX_SUMMARY_LEN chars."""
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
            text = content.removeprefix("summarize: ").strip()
            if len(text) <= MAX_SUMMARY_LEN:
                return {"response": text}
            return {"response": text[:MAX_SUMMARY_LEN] + "..."}
    return {"response": ""}
