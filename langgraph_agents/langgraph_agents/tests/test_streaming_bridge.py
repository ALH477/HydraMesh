# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the streaming bridge."""
from langgraph_agents.streaming_bridge import chunk_response


def test_chunk_short_message():
    """Messages under DCF-Text max fit in one chunk."""
    chunks = chunk_response("hello", max_chunk=4092)
    assert chunks == ["hello"]


def test_chunk_long_message():
    """Long messages get split at max_chunk boundaries."""
    text = "a" * 5000
    chunks = chunk_response(text, max_chunk=4092)
    assert len(chunks) == 2
    assert len(chunks[0]) == 4092
    assert len(chunks[1]) == 908


def test_chunk_empty():
    chunks = chunk_response("", max_chunk=4092)
    assert chunks == []


def test_chunk_exact_boundary():
    """Message exactly at boundary is one chunk."""
    text = "a" * 4092
    chunks = chunk_response(text, max_chunk=4092)
    assert len(chunks) == 1
    assert chunks[0] == text


def test_chunk_very_long():
    """Very long messages split into many chunks."""
    text = "x" * 20000
    chunks = chunk_response(text, max_chunk=4092)
    assert len(chunks) == 5
    total_len = sum(len(c) for c in chunks)
    assert total_len == 20000


def test_chunk_utf8_boundary():
    """Chunking should not split mid-character for multi-byte UTF-8."""
    # Each emoji is 4 bytes in UTF-8
    text = "🚀" * 2000  # 8000 bytes
    chunks = chunk_response(text, max_chunk=4092)
    # Verify all chunks are valid UTF-8 and total length matches
    reconstructed = "".join(chunks)
    assert reconstructed == text
    assert all(len(c.encode("utf-8")) <= 4092 for c in chunks)
