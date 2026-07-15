# Copyright (C) 2026 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0-only
"""Pluggable LLM backends for agent graph process nodes.

Each backend is a callable: (messages: list[dict]) -> str
The 'echo' backend mirrors input (for testing). Real backends call APIs.

The universal backend supports any OpenAI-compatible provider via config:
    {"provider": "fireworks", "model": "accounts/fireworks/models/glm-5p2",
     "base_url": "https://api.fireworks.ai/inference/v1", "api_key": "..."}

Pre-registered backends (echo, grok, glm5p2) remain for backward compat.
New providers are registered at runtime via register_provider().
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


def fireworks_backend(messages: list[dict]) -> str:
    """GLM-5p2 via Fireworks AI (OpenAI-compatible API). Requires FIREWORKS_API_KEY env."""
    import httpx
    api_key = os.environ.get("FIREWORKS_API_KEY", "")
    if not api_key:
        raise ValueError("FIREWORKS_API_KEY not set")
    resp = httpx.post(
        "https://api.fireworks.ai/inference/v1/chat/completions",
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
        json={
            "model": "accounts/fireworks/models/glm-5p2",
            "messages": messages,
            "stream": False,
        },
        timeout=60.0,
    )
    resp.raise_for_status()
    return resp.json()["choices"][0]["message"]["content"]


class UniversalBackend:
    """Universal LLM backend for any OpenAI-compatible API.

    One class, any provider. Pass base_url + api_key + model at construction;
    it calls {base_url}/chat/completions with standard OpenAI format.

    Example:
        backend = UniversalBackend(
            base_url="https://api.fireworks.ai/inference/v1",
            api_key="fw_...",
            model="accounts/fireworks/models/glm-5p2",
        )
        response = backend([{"role": "user", "content": "hello"}])
    """

    def __init__(
        self,
        base_url: str,
        api_key: str = "",
        model: str = "",
        timeout: float = 60.0,
        extra_headers: dict | None = None,
    ) -> None:
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key
        self.model = model
        self.timeout = timeout
        self.extra_headers = extra_headers or {}

    def __call__(self, messages: list[dict]) -> str:
        """Make a chat completion call. Returns the response text."""
        import httpx
        if not self.api_key:
            raise ValueError(f"No API key for {self.base_url}")
        if not self.model:
            raise ValueError(f"No model specified for {self.base_url}")
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
        }
        headers.update(self.extra_headers)
        resp = httpx.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json={
                "model": self.model,
                "messages": messages,
                "stream": False,
            },
            timeout=self.timeout,
        )
        resp.raise_for_status()
        return resp.json()["choices"][0]["message"]["content"]

    def __repr__(self) -> str:
        return f"UniversalBackend(base_url={self.base_url!r}, model={self.model!r})"


# ── provider registry ────────────────────────────────────────────────────────

# Pre-defined provider configs: name → (base_url, env_var, default_model)
PROVIDER_PRESETS: dict[str, dict] = {
    "fireworks": {
        "base_url": "https://api.fireworks.ai/inference/v1",
        "api_key_env": "FIREWORKS_API_KEY",
        "default_model": "accounts/fireworks/models/glm-5p2",
    },
    "openai": {
        "base_url": "https://api.openai.com/v1",
        "api_key_env": "OPENAI_API_KEY",
        "default_model": "gpt-4o",
    },
    "anthropic": {
        "base_url": "https://api.anthropic.com/v1",
        "api_key_env": "ANTHROPIC_API_KEY",
        "default_model": "claude-sonnet-4-20250514",
        "extra_headers": {"anthropic-version": "2023-06-01"},
    },
    "grok": {
        "base_url": "https://api.x.ai/v1",
        "api_key_env": "XAI_API_KEY",
        "default_model": "grok-beta",
    },
    "ollama": {
        "base_url": "http://127.0.0.1:11434/v1",
        "api_key_env": "",
        "default_model": "llama3",
    },
    "deepseek": {
        "base_url": "https://api.deepseek.com/v1",
        "api_key_env": "DEEPSEEK_API_KEY",
        "default_model": "deepseek-chat",
    },
    "openrouter": {
        "base_url": "https://openrouter.ai/api/v1",
        "api_key_env": "OPENROUTER_API_KEY",
        "default_model": "openai/gpt-4o",
    },
}


def make_provider_backend(
    provider: str,
    model: str = "",
    api_key: str = "",
    base_url: str = "",
) -> UniversalBackend:
    """Create a UniversalBackend from a provider preset name.

    Args:
        provider: preset name (fireworks, openai, grok, ollama, etc.)
        model: override model (defaults to preset's default_model)
        api_key: override key (defaults to env var from preset)
        base_url: override URL (defaults to preset's base_url)

    Raises:
        ValueError: if provider is unknown or no API key available.
    """
    preset = PROVIDER_PRESETS.get(provider)
    if not preset:
        raise ValueError(
            f"Unknown provider: {provider!r}. "
            f"Available: {list(PROVIDER_PRESETS)}"
        )
    url = base_url or preset["base_url"]
    key = api_key
    if not key and preset["api_key_env"]:
        key = os.environ.get(preset["api_key_env"], "")
    mdl = model or preset["default_model"]
    extra = preset.get("extra_headers")
    return UniversalBackend(
        base_url=url,
        api_key=key,
        model=mdl,
        extra_headers=extra,
    )


def register_provider(name: str, backend: Callable[[list[dict]], str]) -> None:
    """Register a custom backend at runtime."""
    BACKENDS[name] = backend


def register_from_config(providers: list[dict]) -> None:
    """Register providers from a config list.

    Each entry: {"name": "my-backend", "provider": "fireworks",
                 "model": "...", "api_key": "...", "base_url": "..."}
    """
    for entry in providers:
        name = entry.get("name")
        if not name:
            continue
        provider = entry.get("provider", "")
        if provider in PROVIDER_PRESETS:
            backend = make_provider_backend(
                provider=provider,
                model=entry.get("model", ""),
                api_key=entry.get("api_key", ""),
                base_url=entry.get("base_url", ""),
            )
        else:
            # Direct universal config
            backend = UniversalBackend(
                base_url=entry.get("base_url", ""),
                api_key=entry.get("api_key", ""),
                model=entry.get("model", ""),
            )
        register_provider(name, backend)


BACKENDS: dict[str, Callable[[list[dict]], str]] = {
    "echo": echo_backend,
    "grok": grok_backend,
    "glm5p2": fireworks_backend,
}


def get_backend(name: str) -> Callable[[list[dict]], str]:
    """Look up a backend by name. Raises ValueError for unknown names."""
    if name not in BACKENDS:
        raise ValueError(f"Unknown LLM backend: {name!r}. Available: {list(BACKENDS)}")
    return BACKENDS[name]


def list_backends() -> list[str]:
    """List all registered backend names."""
    return sorted(BACKENDS.keys())


def list_providers() -> list[str]:
    """List all available provider presets."""
    return sorted(PROVIDER_PRESETS.keys())
