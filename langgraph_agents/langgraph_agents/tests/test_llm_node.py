# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for LLM backend registration and wiring."""
from langgraph_agents.llm_node import BACKENDS, get_backend


def test_echo_backend_registered():
    assert "echo" in BACKENDS


def test_grok_backend_registered():
    assert "grok" in BACKENDS


def test_glm5p2_backend_registered():
    """glm5p2 should be a registered backend name."""
    assert "glm5p2" in BACKENDS, f"glm5p2 not in BACKENDS: {list(BACKENDS)}"


def test_glm5p2_backend_callable():
    """The glm5p2 backend should be callable with a messages list."""
    backend = get_backend("glm5p2")
    assert callable(backend)


def test_get_backend_unknown_raises():
    """Unknown backend name should raise ValueError."""
    try:
        get_backend("nonexistent")
        assert False, "should have raised"
    except ValueError:
        pass


def test_glm5p2_backend_makes_correct_api_call(monkeypatch):
    """Verify glm5p2 backend calls Fireworks with correct model and headers."""
    from unittest.mock import MagicMock
    from langgraph_agents.llm_node import fireworks_backend

    monkeypatch.setenv("FIREWORKS_API_KEY", "fw_test123")

    mock_resp = MagicMock()
    mock_resp.json.return_value = {
        "choices": [{"message": {"content": "GLM response here"}}]
    }
    mock_resp.raise_for_status = MagicMock()

    import httpx
    calls = []

    def mock_post(url, **kwargs):
        calls.append({"url": url, "kwargs": kwargs})
        return mock_resp

    monkeypatch.setattr(httpx, "post", mock_post)

    messages = [{"role": "user", "content": "test prompt"}]
    result = fireworks_backend(messages)

    assert result == "GLM response here"
    assert len(calls) == 1
    assert calls[0]["url"] == "https://api.fireworks.ai/inference/v1/chat/completions"
    assert calls[0]["kwargs"]["json"]["model"] == "accounts/fireworks/models/glm-5p2"
    assert calls[0]["kwargs"]["headers"]["Authorization"] == "Bearer fw_test123"


def test_glm5p2_backend_no_api_key_raises(monkeypatch):
    """Should raise ValueError when FIREWORKS_API_KEY is not set."""
    from langgraph_agents.llm_node import fireworks_backend

    monkeypatch.delenv("FIREWORKS_API_KEY", raising=False)
    try:
        fireworks_backend([{"role": "user", "content": "hi"}])
        assert False, "should have raised"
    except ValueError as e:
        assert "FIREWORKS_API_KEY" in str(e)
