# GLM-5p2 Fireworks LLM Backend Implementation Plan

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Add `accounts/fireworks/models/glm-5p2` as a callable LLM backend in the langgraph_agents system, accessible via the Fireworks OpenAI-compatible API.

**Architecture:** Add a `fireworks_backend` function to `llm_node.py` that calls the Fireworks chat completions endpoint (`https://api.fireworks.ai/inference/v1/chat/completions`) with the GLM-5p2 model ID. Register it in the `BACKENDS` dict under the key `"glm5p2"`. Update `agents.jsonc` to include a new agent entry using this backend.

**Tech Stack:** Python, httpx, Fireworks AI API (OpenAI-compatible), GLM-5p2 model

---

### Task 1: Write failing test for fireworks_backend

**Objective:** Define the expected behavior of the GLM-5p2 backend before implementing it.

**Files:**
- Modify: `langgraph_agents/langgraph_agents/tests/test_llm_node.py`

**Step 1: Add test for backend registration**

Append to the existing test file:

```python
def test_glm5p2_backend_registered():
    """glm5p2 should be a registered backend name."""
    from langgraph_agents.llm_node import BACKENDS
    assert "glm5p2" in BACKENDS, f"glm5p2 not in BACKENDS: {list(BACKENDS)}"


def test_glm5p2_backend_callable():
    """The glm5p2 backend should be callable with a messages list."""
    from langgraph_agents.llm_node import get_backend
    backend = get_backend("glm5p2")
    assert callable(backend)
```

**Step 2: Run test to verify failure**

Run: `cd /home/asher/Documents/HydraMesh/langgraph_agents && source .venv/bin/activate && unset PYTHONPATH && python -m pytest langgraph_agents/tests/test_llm_node.py::test_glm5p2_backend_registered -v`
Expected: FAIL — "glm5p2 not in BACKENDS"

**Step 3: Commit**

```bash
git add langgraph_agents/langgraph_agents/tests/test_llm_node.py
git commit -m "test: add failing tests for glm5p2 backend registration"
```

---

### Task 2: Implement fireworks_backend in llm_node.py

**Objective:** Add the GLM-5p2 backend function and register it.

**Files:**
- Modify: `langgraph_agents/langgraph_agents/llm_node.py`

**Step 1: Add the fireworks_backend function**

Insert after the `grok_backend` function (before the `BACKENDS` dict):

```python
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
```

**Step 2: Register in BACKENDS dict**

Update the `BACKENDS` dict:

```python
BACKENDS: dict[str, Callable[[list[dict]], str]] = {
    "echo": echo_backend,
    "grok": grok_backend,
    "glm5p2": fireworks_backend,
}
```

**Step 3: Run tests to verify pass**

Run: `cd /home/asher/Documents/HydraMesh/langgraph_agents && source .venv/bin/activate && unset PYTHONPATH && python -m pytest langgraph_agents/tests/test_llm_node.py -v`
Expected: PASS — all tests including new glm5p2 registration tests

**Step 4: Run full suite to verify no regressions**

Run: `cd /home/asher/Documents/HydraMesh/langgraph_agents && source .venv/bin/activate && unset PYTHONPATH && python -m pytest -c pytest.ini -v`
Expected: 33 passed (31 existing + 2 new)

**Step 5: Commit**

```bash
git add langgraph_agents/langgraph_agents/llm_node.py
git commit -m "feat: add glm5p2 (GLM-5p2) Fireworks backend"
```

---

### Task 3: Add glm5p2 agent entry to agents.jsonc

**Objective:** Configure an agent that uses the new GLM-5p2 backend.

**Files:**
- Modify: `langgraph_agents/agents.jsonc`

**Step 1: Add the new agent block**

Add a third agent entry to the `"agents"` array:

```jsonc
{
  "name": "glm5p2-agent",
  "graph": "coordinator",
  "llm_backend": "glm5p2",
  "channel": "glm",
  "mesh_url": "http://127.0.0.1:8765",
  "poll_interval": 1.0
}
```

**Step 2: Verify JSONC parses correctly**

Run: `cd /home/asher/Documents/HydraMesh/langgraph_agents && source .venv/bin/activate && unset PYTHONPATH && python -c "from langgraph_agents.__main__ import load_config; c = load_config('agents.jsonc'); print([a['name'] for a in c['agents']])"`
Expected: `['echo-agent', 'coordinator-agent', 'glm5p2-agent']`

**Step 3: Commit**

```bash
git add langgraph_agents/agents.jsonc
git commit -m "config: add glm5p2-agent entry to agents.jsonc"
```

---

### Task 4: Write integration test for glm5p2 backend (mocked)

**Objective:** Verify the backend makes the correct API call without hitting the real API.

**Files:**
- Modify: `langgraph_agents/langgraph_agents/tests/test_llm_node.py`

**Step 1: Add mocked integration test**

```python
def test_glm5p2_backend_makes_correct_api_call(monkeypatch):
    """Verify glm5p2 backend calls Fireworks with correct model and headers."""
    import os
    from unittest.mock import MagicMock
    from langgraph_agents.llm_node import fireworks_backend

    # Mock env
    monkeypatch.setenv("FIREWORKS_API_KEY", "fw_test123")

    # Mock httpx.post
    mock_resp = MagicMock()
    mock_resp.json.return_value = {
        "choices": [{"message": {"content": "GLM response here"}}]
    }
    mock_resp.raise_for_status = MagicMock()

    import httpx
    original_post = httpx.post
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
```

**Step 2: Run test**

Run: `cd /home/asher/Documents/HydraMesh/langgraph_agents && source .venv/bin/activate && unset PYTHONPATH && python -m pytest langgraph_agents/tests/test_llm_node.py::test_glm5p2_backend_makes_correct_api_call -v`
Expected: PASS

**Step 3: Commit**

```bash
git add langgraph_agents/langgraph_agents/tests/test_llm_node.py
git commit -m "test: add mocked API call test for glm5p2 backend"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Failing tests for glm5p2 registration | test_llm_node.py |
| 2 | Implement fireworks_backend + register | llm_node.py |
| 3 | Add glm5p2-agent to agents.jsonc | agents.jsonc |
| 4 | Mocked integration test | test_llm_node.py |

**Env requirement:** `FIREWORKS_API_KEY` must be set in the environment (or `.env`) for the backend to work at runtime. The key is already available in the Hermes config as `fw_GRFNUDoS4hEuFhnXmGaVda`.

**Verification:** Full test suite should go from 31 → 34 passing tests.
