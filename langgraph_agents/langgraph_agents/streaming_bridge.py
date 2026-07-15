# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Streaming bridge: chunk LLM output for DCF-Text transport.

DCF-Text caps messages at 4092 bytes. For streaming LLM output that
exceeds this, we split into chunks and send each as a separate
mesh_send call.
"""
from __future__ import annotations

import asyncio
import logging
from typing import AsyncIterator, Callable

logger = logging.getLogger(__name__)

DCF_TEXT_MAX = 4092  # bytes per DCF-Text message


def chunk_response(text: str, max_chunk: int = DCF_TEXT_MAX) -> list[str]:
    """Split a response into chunks that fit DCF-Text's per-message cap.

    Handles UTF-8 boundary safety — never splits mid-character.
    """
    if not text:
        return []
    encoded = text.encode("utf-8")
    if len(encoded) <= max_chunk:
        return [text]
    chunks = []
    offset = 0
    while offset < len(encoded):
        end = min(offset + max_chunk, len(encoded))
        # Back up until we land on a valid UTF-8 boundary
        while end > offset:
            try:
                encoded[offset:end].decode("utf-8")
                break
            except UnicodeDecodeError:
                end -= 1
        if end == offset:
            break  # safety: shouldn't happen with max_chunk >= 4
        chunks.append(encoded[offset:end].decode("utf-8"))
        offset = end
    return chunks


async def stream_and_send(
    text_stream: AsyncIterator[str],
    send_fn: Callable[[str], None],
    flush_interval: float = 0.5,
) -> int:
    """Consume an async text stream, buffer, and send chunks when full.

    Returns the number of chunks sent.
    """
    buffer = ""
    sent = 0
    async for token in text_stream:
        buffer += token
        encoded = buffer.encode("utf-8")
        if len(encoded) >= DCF_TEXT_MAX:
            piece = encoded[:DCF_TEXT_MAX]
            while piece and piece[-1] & 0xC0 == 0x80:
                piece = piece[:-1]
            send_fn(piece.decode("utf-8", errors="replace"))
            sent += 1
            buffer = encoded[len(piece):].decode("utf-8", errors="replace")
    # Flush remainder
    if buffer.strip():
        send_fn(buffer)
        sent += 1
    return sent
