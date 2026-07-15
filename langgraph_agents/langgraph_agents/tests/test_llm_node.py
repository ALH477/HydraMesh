# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Tests for the universal LLM backend system."""
from langgraph_agents.llm_node import (
    BACKENDS, UniversalBackend, PROVIDER_PRESETS,
    make_provider_backend, register_provider, register_from_config,
    list_backends, list_providers,
)


def test_universal_backend_is_callable():
    """UniversalBackend should be callable like other backends."""
    b = UniversalBackend(base_url="http://example.com/v1", api_key="k", model="m")
    assert callable(b)


def test_universal_backend_strips_trailing_slash():
    b = UniversalBackend(base_url="http://example.com/v1/", api_key="k", model="m")
    assert b.base_url == "http://example.com/v1"


def test_provider_presets_contain_known_providers():
    for name in ("fireworks", "openai", "grok", "ollama", "deepseek", "openrouter"):
        assert name in PROVIDER_PRESETS, f"missing preset: {name}"


def test_make_provider_backend_fireworks():
    """make_provider_backend with fireworks preset should create UniversalBackend."""
    import os
    os.environ["FIREWORKS_API_KEY"] = "fw_test"
    b = make_provider_backend("fireworks")
    assert isinstance(b, UniversalBackend)
    assert b.base_url == "https://api.fireworks.ai/inference/v1"
    assert b.api_key == "fw_test"
    assert b.model == "accounts/fireworks/models/glm-5p2"


def test_make_provider_backend_unknown_raises():
    try:
        make_provider_backend("nonexistent")
        assert False
    except ValueError:
        pass


def test_register_provider_adds_to_backends():
    """register_provider should add a callable to BACKENDS."""
    def custom_backend(msgs):
        return "custom"
    register_provider("test-custom", custom_backend)
    assert "test-custom" in BACKENDS
    assert BACKENDS["test-custom"]([{"role": "user", "content": "hi"}]) == "custom"
    # cleanup
    del BACKENDS["test-custom"]


def test_register_from_config():
    """register_from_config should register providers from config list."""
    register_from_config([
        {"name": "cfg-fireworks", "provider": "fireworks", "model": "test-model"},
        {"name": "cfg-direct", "base_url": "http://custom.api/v1", "api_key": "k", "model": "m"},
    ])
    assert "cfg-fireworks" in BACKENDS
    assert "cfg-direct" in BACKENDS
    assert isinstance(BACKENDS["cfg-fireworks"], UniversalBackend)
    assert isinstance(BACKENDS["cfg-direct"], UniversalBackend)
    # cleanup
    del BACKENDS["cfg-fireworks"]
    del BACKENDS["cfg-direct"]


def test_list_backends_returns_sorted():
    backends = list_backends()
    assert backends == sorted(backends)
    assert "echo" in backends


def test_list_providers_returns_sorted():
    providers = list_providers()
    assert providers == sorted(providers)
    assert "fireworks" in providers


def test_ollama_preset_has_no_env():
    """Ollama runs locally, so no API key env should be required."""
    assert PROVIDER_PRESETS["ollama"]["api_key_env"] == ""
