# LangGraph + MCP Realtime Agent Communication System

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Build a LangGraph-orchestrated multi-agent system that communicates in realtime over the DCF mesh via MCP tools — agents send/receive DeModFrame text through `mesh_mcp.py`, process with streaming LLM calls, and coordinate via a LangGraph state graph.

**Architecture:** A new `langgraph-agents/` module under the HydraMesh repo. Each agent is a LangGraph `StateGraph` node with an LLM step. Agents communicate by reading/writing to the DCF mesh through MCP tools exposed by the existing `mesh_mcp.py` server. A coordinator graph routes incoming mesh messages to specialist agents and sends their responses back. The MCP server gets extended with streaming-aware tools (SSE/HTTP transport) so LangGraph's astream events can drive realtime mesh I/O.

**Tech Stack:** Python 3.12+, LangGraph (langgraph>=0.2), LangChain Core, FastMCP (already in repo), existing `textlab_core.py` / `dcf_node.py` / `mesh_mcp.py`, aiohttp or httpx for streaming LLM APIs.

---

## Current Context

The repo already has:
- **`matrix-bridge/mesh_mcp.py`** — MCP server with `mesh_inbox`, `mesh_recv`, `mesh_send`, `mesh_status` tools. Runs stdio or streamable-HTTP. Uses `DcfTextNode` for UDP mesh I/O.
- **`python/MCP/wirelab_mcp.py`** — Wire quantum inspection MCP server.
- **`python/dcf/mcp_server.py`** + **`mcp_client.py`** — Generic DCF MCP server/client with retries, OAuth stub, HTTP transport.
- **`matrix-bridge/a2a_endpoint.py`** — Resolves stdio/http/sse transport modes.
- **`hydra-llm-interface/`** — Older gRPC+StreamDB+Grok approach (superseded by this plan).
- **DCF-Text** (`textlab_core.py`) — Certified L2 text fragmentation over DeModFrame (4092B/msg, 10-bit frag index).
- **Transport layer** (`dcf/transport.py`) — Multi-transport with bounded outbound queues.

No LangGraph code exists yet. This is a greenfield addition.

---

## Proposed Approach

### Layer 1: MCP Server Extensions
Extend `mesh_mcp.py` with:
- `mesh_subscribe(channel)` — resource that streams incoming messages (SSE-style)
- `mesh_send_streaming(text, channel)` — tool that returns a stream handle for chunked sends
- Agent registry resource — lists active agents on the mesh

### Layer 2: LangGraph Agent Core
New `langgraph-agents/` package:
- `agent_graph.py` — Base `StateGraph` with nodes: `receive` → `route` → `process` → `respond`
- `state.py` — TypedDict state: `messages`, `channel`, `sender`, `routing_decision`, `response`
- `llm_node.py` — Streaming LLM call node (pluggable backend: Grok/OpenAI/local)
- `mcp_tools.py` — LangChain tools wrapping MCP calls to `mesh_mcp.py`

### Layer 3: Multi-Agent Coordination
- `coordinator.py` — Supervisor graph that routes messages to specialist subgraphs
- `specialists/` — Example specialist agents (echo, summarizer, translator)
- `registry.py` — Agent discovery: which agents handle which channels/topics

### Layer 4: Realtime Transport
- `streaming_bridge.py` — Bridges LangGraph `astream_events` to MCP streaming resources
- Config: `agents.jsonc` — declares agents, their graphs, LLM backends, channel bindings

---

## Step-by-Step Plan

### Phase 1: Project Setup

### Task 1: Create langgraph-agents package structure

**Objective:** Scaffold the new module with pyproject.toml and directory layout.

**Files:**
- Create: `langgraph-agents/pyproject.toml`
- Create: `langgraph-agents/__init__.py`
- Create: `langgraph-agents/state.py`
- Create: `langgraph-agents/README.md`

**Step 1: Create directory structure**

```bash
mkdir -p langgraph-agents/{specialists,tests}
touch langgraph-agents/__init__.py
touch langgraph-agents/specialists/__init__.py
touch langgraph-agents/tests/__init__.py
```

**Step 2: Write pyproject.toml**

```toml
[build-system]
requires = ["setuptools>=64"]
build-backend = "setuptools.build_meta"

[project]
name = "hydramesh-langgraph-agents"
version = "0.1.0"
description = "LangGraph multi-agent system communicating over DCF mesh via MCP"
requires-python = ">=3.11"
license = { text = "LGPL-3.0-only" }
dependencies = [
    "langgraph>=0.2",
    "langchain-core>=0.3",
    "langchain-openai>=0.2",
    "mcp>=1.0",
    "httpx>=0.27",
    "aiohttp>=3.9",
]

[project.optional-dependencies]
dev = ["pytest", "pytest-asyncio"]
```

**Step 3: Write state.py (TypedDict for graph state)**

```python
"""Shared state schema for all LangGraph agent graphs in HydraMesh."""
from __future__ import annotations
from typing import Annotated, Any, TypedDict
from langgraph.graph.message import add_messages

class AgentState(TypedDict):
    """State flowing through every agent graph."""
    messages: Annotated[list, add_messages]  # LangGraph message accumulator
    channel: str                             # DCF channel name (rendezvous dst)
    sender: str                              # hex node id of message origin
    routing_decision: str                    # which specialist handles this
    response: str                            # outbound text to send back
    metadata: dict[str, Any]                 # extensible per-agent data
```

**Step 4: Commit**

```bash
git add langgraph-agents/
git commit -m "feat: scaffold langgraph-agents package with state schema"
```

---

### Task 2: Write failing test for AgentState

**Objective:** Verify the state TypedDict works with LangGraph's message annotation.

**Files:**
- Create: `langgraph-agents/tests/test_state.py`

**Step 1: Write test**

```python
"""Tests for the shared AgentState TypedDict."""
import pytest
from langgraph.graph import StateGraph
from langgraph-agents.state import AgentState

def test_state_graph_accepts_agent_state():
    """AgentState must be usable as a LangGraph StateGraph schema."""
    graph = StateGraph(AgentState)
    assert graph is not None

def test_state_has_required_fields():
    """All required fields must be declared."""
    hints = AgentState.__annotations__
    for field in ("messages", "channel", "sender", "routing_decision", "response", "metadata"):
        assert field in hints, f"missing field: {field}"
```

**Step 2: Run test to verify failure**

Run: `cd langgraph-agents && python -m pytest tests/test_state.py -v`
Expected: FAIL — ModuleNotFoundError (langgraph not installed) or import error

**Step 3: Install dependencies**

```bash
cd langgraph-agents && pip install -e ".[dev]"
```

**Step 4: Run test to verify pass**

Run: `cd langgraph-agents && python -m pytest tests/test_state.py -v`
Expected: 2 passed

**Step 5: Commit**

```bash
git add langgraph-agents/tests/test_state.py
git commit -m "test: add AgentState schema tests"
```

---

### Phase 2: MCP Tool Wrappers for LangGraph

### Task 3: Create MCP tool wrappers

**Objective:** Wrap mesh_mcp.py tools as LangChain Tool objects that LangGraph nodes can call.

**Files:**
- Create: `langgraph-agents/mcp_tools.py`

**Step 1: Write failing test**

```python
# langgraph-agents/tests/test_mcp_tools.py
import pytest
from langgraph_agents.mcp_tools import MeshSendTool, MeshRecvTool, MeshStatusTool

def test_mesh_send_tool_has_correct_name():
    tool = MeshSendTool(mesh_url="http://127.0.0.1:8765")
    assert tool.name == "mesh_send"

def test_mesh_recv_tool_has_correct_name():
    tool = MeshRecvTool(mesh_url="http://127.0.0.1:8765")
    assert tool.name == "mesh_recv"

def test_mesh_status_tool_has_correct_name():
    tool = MeshStatusTool(mesh_url="http://127.0.0.1:8765")
    assert tool.name == "mesh_status"
```

**Step 2: Run test to verify failure**

Run: `cd langgraph-agents && python -m pytest tests/test_mcp_tools.py -v`
Expected: FAIL — ImportError

**Step 3: Implement mcp_tools.py**

```python
"""LangChain Tool wrappers for DCF mesh MCP server tools.

These let LangGraph agent nodes call mesh_send, mesh_recv, mesh_status
as standard LangChain tools — the graph doesn't know or care that the
backend is MCP over HTTP; it just calls tool.invoke(args).
"""
from __future__ import annotations
import httpx
from langchain_core.tools import BaseTool
from pydantic import BaseModel, Field


class MeshSendInput(BaseModel):
    text: str = Field(description="Text to send onto the DCF mesh")
    channel: str = Field(default="", description="Target channel (empty=broadcast)")


class MeshRecvInput(BaseModel):
    timeout_s: float = Field(default=5.0, description="Seconds to wait for a message")


class MeshStatusInput(BaseModel):
    pass


class MeshSendTool(BaseTool):
    name: str = "mesh_send"
    description: str = "Send text onto the DCF mesh. Returns confirmation with datagram count."
    args_schema: type[BaseModel] = MeshSendInput
    mesh_url: str = ""

    def _run(self, text: str, channel: str = "") -> str:
        resp = httpx.post(
            f"{self.mesh_url}/tools/mesh_send",
            json={"text": text, "channel": channel},
            timeout=10.0,
        )
        resp.raise_for_status()
        return resp.json().get("result", "sent")


class MeshRecvTool(BaseTool):
    name: str = "mesh_recv"
    description: str = "Block until a message arrives from the mesh (or timeout). Returns list of message dicts."
    args_schema: type[BaseModel] = MeshRecvInput
    mesh_url: str = ""

    def _run(self, timeout_s: float = 5.0) -> list[dict]:
        resp = httpx.post(
            f"{self.mesh_url}/tools/mesh_recv",
            json={"timeout_s": timeout_s},
            timeout=timeout_s + 5.0,
        )
        resp.raise_for_status()
        return resp.json().get("result", [])


class MeshStatusTool(BaseTool):
    name: str = "mesh_status"
    description: str = "Report agent mesh endpoint info: node id, channel, peers, port."
    args_schema: type[BaseModel] = MeshStatusInput
    mesh_url: str = ""

    def _run(self) -> dict:
        resp = httpx.post(
            f"{self.mesh_url}/tools/mesh_status",
            json={},
            timeout=5.0,
        )
        resp.raise_for_status()
        return resp.json().get("result", {})
```

**Step 4: Run tests to verify pass**

Run: `cd langgraph-agents && python -m pytest tests/test_mcp_tools.py -v`
Expected: 3 passed

**Step 5: Commit**

```bash
git add langgraph-agents/mcp_tools.py langgraph-agents/tests/test_mcp_tools.py
git commit -m "feat: LangChain tool wrappers for mesh MCP server"
```

---

### Phase 3: Core Agent Graph

### Task 4: Build the base agent graph (receive → route → process → respond)

**Objective:** A LangGraph StateGraph that receives a mesh message, routes it, calls an LLM, and sends a response.

**Files:**
- Create: `langgraph-agents/agent_graph.py`
- Create: `langgraph-agents/llm_node.py`
- Test: `langgraph-agents/tests/test_agent_graph.py`

**Step 1: Write failing test**

```python
# langgraph-agents/tests/test_agent_graph.py
import pytest
from langgraph_agents.agent_graph import build_agent_graph
from langgraph_agents.state import AgentState

def test_build_graph_returns_compiled():
    graph = build_agent_graph(llm_backend="echo")
    assert hasattr(graph, "invoke")

def test_graph_has_four_nodes():
    graph = build_agent_graph(llm_backend="echo")
    # compiled graph has a .nodes mapping
    node_names = set(graph.get_graph().nodes.keys())
    for expected in ("receive", "route", "process", "respond"):
        assert expected in node_names, f"missing node: {expected}"

def test_echo_graph_roundtrip():
    """With echo backend, response should mirror the inbound message."""
    graph = build_agent_graph(llm_backend="echo")
    result = graph.invoke({
        "messages": [{"role": "user", "content": "hello mesh"}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["response"] == "hello mesh"
```

**Step 2: Run test to verify failure**

Run: `cd langgraph-agents && python -m pytest tests/test_agent_graph.py -v`
Expected: FAIL — ImportError

**Step 3: Implement llm_node.py**

```python
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
        if msg.get("role") == "user":
            return msg["content"]
    return ""


def grok_backend(messages: list[dict]) -> str:
    """Grok (xAI) streaming chat completion. Requires GROK_API_KEY env."""
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
    if name not in BACKENDS:
        raise ValueError(f"Unknown LLM backend: {name!r}. Available: {list(BACKENDS)}")
    return BACKENDS[name]
```

**Step 4: Implement agent_graph.py**

```python
"""Base LangGraph agent graph: receive → route → process → respond.

The graph is the skeleton every agent shares. Specialist agents override
the 'route' and 'process' nodes by passing custom callables.
"""
from __future__ import annotations
from typing import Callable, Optional
from langgraph.graph import StateGraph, START, END
from .state import AgentState
from .llm_node import get_backend


def receive_node(state: AgentState) -> dict:
    """Receive node: extract the inbound message from the messages list."""
    # The messages are already in state (populated by the entry point).
    # This node just validates and passes through.
    if not state.get("messages"):
        return {"response": "", **state}
    return {}


def default_route(state: AgentState) -> dict:
    """Default router: always route to 'general' processing."""
    return {"routing_decision": "general"}


def make_process_node(backend_name: str) -> Callable:
    """Factory: returns a process node function bound to the given LLM backend."""
    backend = get_backend(backend_name)

    def process_node(state: AgentState) -> dict:
        messages = state.get("messages", [])
        msg_dicts = [
            {"role": m.get("type", m.get("role", "user")), "content": m.get("content", "")}
            if isinstance(m, dict) else {"role": "user", "content": str(m)}
            for m in messages
        ]
        response = backend(msg_dicts)
        return {"response": response}
    return process_node


def respond_node(state: AgentState) -> dict:
    """Respond node: marks the response as ready for the caller to send."""
    # The actual mesh_send call happens in the runner (outside the graph),
    # or via a tool call. This node just finalizes state.
    return {}


def build_agent_graph(
    llm_backend: str = "echo",
    route_fn: Optional[Callable] = None,
    process_fn: Optional[Callable] = None,
) -> StateGraph:
    """Build and compile the base agent graph.

    Args:
        llm_backend: name of the LLM backend ('echo', 'grok')
        route_fn: optional custom router (replaces default_route)
        process_fn: optional custom processor (replaces LLM-backed process)
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
```

**Step 5: Run tests to verify pass**

Run: `cd langgraph-agents && python -m pytest tests/test_agent_graph.py -v`
Expected: 3 passed

**Step 6: Commit**

```bash
git add langgraph-agents/agent_graph.py langgraph-agents/llm_node.py langgraph-agents/tests/test_agent_graph.py
git commit -m "feat: base LangGraph agent graph with receive→route→process→respond"
```

---

### Phase 4: Agent Runner (Graph + Mesh Loop)

### Task 5: Build the agent runner that ties the graph to the mesh

**Objective:** A long-running process that polls mesh_mcp for messages, feeds them into the graph, and sends responses back.

**Files:**
- Create: `langgraph-agents/runner.py`
- Test: `langgraph-agents/tests/test_runner.py`

**Step 1: Write failing test**

```python
# langgraph-agents/tests/test_runner.py
import pytest
from langgraph_agents.runner import AgentRunner

def test_runner_init():
    runner = AgentRunner(
        graph_backend="echo",
        mesh_url="http://127.0.0.1:8765",
        channel="test",
    )
    assert runner.channel == "test"

def test_runner_process_message():
    """Process a single inbound message through the graph and get a response."""
    runner = AgentRunner(graph_backend="echo", mesh_url="", channel="test")
    response = runner.process_message(
        text="hello mesh",
        sender="0x0001",
        channel="test",
    )
    assert response == "hello mesh"
```

**Step 2: Run test to verify failure**

Run: `cd langgraph-agents && python -m pytest tests/test_runner.py -v`
Expected: FAIL — ImportError

**Step 3: Implement runner.py**

```python
"""Agent runner: ties a LangGraph agent to the DCF mesh via MCP.

Polls mesh_recv for inbound messages, feeds each through the compiled
graph, and sends the response back via mesh_send. Runs as a long-lived
asyncio task or a single-shot processor.
"""
from __future__ import annotations
import asyncio
import logging
from typing import Optional
import httpx
from .agent_graph import build_agent_graph

logger = logging.getLogger(__name__)


class AgentRunner:
    """Runs a single LangGraph agent against the mesh."""

    def __init__(
        self,
        graph_backend: str = "echo",
        mesh_url: str = "http://127.0.0.1:8765",
        channel: str = "agent",
        poll_interval: float = 1.0,
    ):
        self.channel = channel
        self.mesh_url = mesh_url
        self.poll_interval = poll_interval
        self.graph = build_agent_graph(llm_backend=graph_backend)
        self._running = False

    def process_message(self, text: str, sender: str, channel: str) -> str:
        """Run one message through the graph synchronously. Returns the response text."""
        result = self.graph.invoke({
            "messages": [{"role": "user", "content": text}],
            "channel": channel,
            "sender": sender,
            "routing_decision": "",
            "response": "",
            "metadata": {},
        })
        return result.get("response", "")

    async def _fetch_messages(self, timeout_s: float = 5.0) -> list[dict]:
        """Call mesh_recv on the MCP server."""
        if not self.mesh_url:
            return []
        try:
            async with httpx.AsyncClient() as client:
                resp = await client.post(
                    f"{self.mesh_url}/tools/mesh_recv",
                    json={"timeout_s": timeout_s},
                    timeout=timeout_s + 5.0,
                )
                resp.raise_for_status()
                return resp.json().get("result", [])
        except Exception as e:
            logger.warning(f"mesh_recv failed: {e}")
            return []

    async def _send_response(self, text: str) -> None:
        """Call mesh_send on the MCP server."""
        if not self.mesh_url:
            logger.info(f"[dry-run] would send: {text}")
            return
        try:
            async with httpx.AsyncClient() as client:
                resp = await client.post(
                    f"{self.mesh_url}/tools/mesh_send",
                    json={"text": text},
                    timeout=10.0,
                )
                resp.raise_for_status()
        except Exception as e:
            logger.error(f"mesh_send failed: {e}")

    async def run_loop(self) -> None:
        """Main loop: poll → process → respond, forever."""
        self._running = True
        logger.info(f"Agent runner started on channel {self.channel!r}")
        while self._running:
            messages = await self._fetch_messages(timeout_s=self.poll_interval)
            for msg in messages:
                text = msg.get("text", "")
                sender = msg.get("src", "unknown")
                channel = msg.get("channel", self.channel)
                if not text:
                    continue
                logger.info(f"Processing from {sender}: {text[:80]}")
                response = self.process_message(text, sender, channel)
                if response:
                    await self._send_response(response)
                    logger.info(f"Responded: {response[:80]}")
            await asyncio.sleep(0.1)  # brief pause between polls

    def stop(self) -> None:
        self._running = False
```

**Step 4: Run tests to verify pass**

Run: `cd langgraph-agents && python -m pytest tests/test_runner.py -v`
Expected: 2 passed

**Step 5: Commit**

```bash
git add langgraph-agents/runner.py langgraph-agents/tests/test_runner.py
git commit -m "feat: agent runner tying LangGraph to mesh via MCP"
```

---

### Phase 5: Coordinator (Multi-Agent Supervisor)

### Task 6: Build the coordinator graph that routes to specialists

**Objective:** A supervisor LangGraph that inspects incoming messages and dispatches to specialist subgraphs (echo, summarizer, etc.).

**Files:**
- Create: `langgraph-agents/coordinator.py`
- Create: `langgraph-agents/specialists/echo.py`
- Create: `langgraph-agents/specialists/summarizer.py`
- Test: `langgraph-agents/tests/test_coordinator.py`

**Step 1: Write failing test**

```python
# langgraph-agents/tests/test_coordinator.py
import pytest
from langgraph_agents.coordinator import build_coordinator_graph

def test_coordinator_builds():
    graph = build_coordinator_graph()
    assert hasattr(graph, "invoke")

def test_coordinator_routes_echo():
    graph = build_coordinator_graph()
    result = graph.invoke({
        "messages": [{"role": "user", "content": "echo: hello"}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "echo"
    assert "hello" in result["response"]
```

**Step 2: Implement specialists/echo.py**

```python
"""Echo specialist: returns the message verbatim (for testing)."""
from langgraph_agents.state import AgentState


def echo_process(state: AgentState) -> dict:
    messages = state.get("messages", [])
    for msg in reversed(messages):
        content = msg.get("content", "") if isinstance(msg, dict) else str(msg)
        if content:
            # Strip "echo: " prefix if present
            text = content.removeprefix("echo: ").strip()
            return {"response": text}
    return {"response": ""}
```

**Step 3: Implement specialists/summarizer.py**

```python
"""Summarizer specialist: truncates long messages to a summary."""
from langgraph_agents.state import AgentState

MAX_SUMMARY_LEN = 100


def summarize_process(state: AgentState) -> dict:
    messages = state.get("messages", [])
    for msg in reversed(messages):
        content = msg.get("content", "") if isinstance(msg, dict) else str(msg)
        if content:
            text = content.removeprefix("summarize: ").strip()
            if len(text) <= MAX_SUMMARY_LEN:
                return {"response": text}
            return {"response": text[:MAX_SUMMARY_LEN] + "..."}
    return {"response": ""}
```

**Step 4: Implement coordinator.py**

```python
"""Coordinator graph: routes messages to specialist subgraphs.

Routing is keyword-based for now (prefix matching). A future version
can use an LLM to classify and route.
"""
from __future__ import annotations
from langgraph.graph import StateGraph, START, END
from .state import AgentState
from .specialists.echo import echo_process
from .specialists.summarizer import summarize_process


ROUTE_MAP = {
    "echo:": "echo",
    "summarize:": "summarizer",
}

SPECIALISTS = {
    "echo": echo_process,
    "summarizer": summarize_process,
}


def coordinator_route(state: AgentState) -> dict:
    """Route based on message prefix."""
    messages = state.get("messages", [])
    for msg in reversed(messages):
        content = msg.get("content", "") if isinstance(msg, dict) else str(msg)
        for prefix, target in ROUTE_MAP.items():
            if content.startswith(prefix):
                return {"routing_decision": target}
    # Default to echo
    return {"routing_decision": "echo"}


def coordinator_dispatch(state: AgentState) -> dict:
    """Dispatch to the chosen specialist and capture its response."""
    target = state.get("routing_decision", "echo")
    specialist_fn = SPECIALISTS.get(target, echo_process)
    result = specialist_fn(state)
    return result


def build_coordinator_graph():
    """Build the supervisor coordinator graph."""
    graph = StateGraph(AgentState)
    graph.add_node("route", coordinator_route)
    graph.add_node("dispatch", coordinator_dispatch)
    graph.add_edge(START, "route")
    graph.add_edge("route", "dispatch")
    graph.add_edge("dispatch", END)
    return graph.compile()
```

**Step 5: Run tests**

Run: `cd langgraph-agents && python -m pytest tests/test_coordinator.py -v`
Expected: 2 passed

**Step 6: Commit**

```bash
git add langgraph-agents/coordinator.py langgraph-agents/specialists/ langgraph-agents/tests/test_coordinator.py
git commit -m "feat: coordinator graph with echo and summarizer specialists"
```

---

### Phase 6: Streaming Bridge

### Task 7: Bridge LangGraph astream_events to MCP streaming

**Objective:** Enable realtime streaming — when the LLM generates tokens, stream them back to the mesh incrementally (chunked DCF-Text messages).

**Files:**
- Create: `langgraph-agents/streaming_bridge.py`
- Test: `langgraph-agents/tests/test_streaming_bridge.py`

**Step 1: Write failing test**

```python
# langgraph-agents/tests/test_streaming_bridge.py
import pytest
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
```

**Step 2: Implement streaming_bridge.py**

```python
"""Streaming bridge: chunk LLM output for DCF-Text transport.

DCF-Text caps messages at 4092 bytes. For streaming LLM output that
exceeds this, we split into FLAG_MORE chunks and send each as a
separate mesh_send call. The receiver's TextReassembler handles
reassembly (future: extend with multi-message reassembly).
"""
from __future__ import annotations
import asyncio
import logging
from typing import AsyncIterator, Callable, Optional

logger = logging.getLogger(__name__)

DCF_TEXT_MAX = 4092  # bytes per DCF-Text message


def chunk_response(text: str, max_chunk: int = DCF_TEXT_MAX) -> list[str]:
    """Split a response into chunks that fit DCF-Text's per-message cap."""
    if not text:
        return []
    encoded = text.encode("utf-8")
    if len(encoded) <= max_chunk:
        return [text]
    chunks = []
    while encoded:
        piece = encoded[:max_chunk]
        # Don't split mid-character: back up if we landed in a multi-byte seq
        while piece and piece[-1] & 0xC0 == 0x80:
            piece = piece[:-1]
        chunks.append(piece.decode("utf-8", errors="replace"))
        encoded = encoded[max_chunk:]
    return chunks


async def stream_and_send(
    text_stream: AsyncIterator[str],
    send_fn: Callable[[str], None],
    flush_interval: float = 0.5,
) -> int:
    """Consume an async text stream, buffer, and send chunks when full or on interval.

    Returns the number of chunks sent.
    """
    buffer = ""
    sent = 0
    async for token in text_stream:
        buffer += token
        encoded = buffer.encode("utf-8")
        if len(encoded) >= DCF_TEXT_MAX:
            # Send what fits
            piece = encoded[:DCF_TEXT_MAX]
            while piece and piece[-1] & 0xC0 == 0x80:
                piece = piece[:-1]
            send_fn(piece.decode("utf-8", errors="replace"))
            sent += 1
            buffer = encoded[DCF_TEXT_MAX:].decode("utf-8", errors="replace")
    # Flush remainder
    if buffer.strip():
        send_fn(buffer)
        sent += 1
    return sent
```

**Step 3: Run tests**

Run: `cd langgraph-agents && python -m pytest tests/test_streaming_bridge.py -v`
Expected: 3 passed

**Step 4: Commit**

```bash
git add langgraph-agents/streaming_bridge.py langgraph-agents/tests/test_streaming_bridge.py
git commit -m "feat: streaming bridge for chunked DCF-Text responses"
```

---

### Phase 7: Configuration and Entry Point

### Task 8: Create agents.jsonc config and CLI entry point

**Objective:** A JSONC config declaring agents, their graphs, LLM backends, and channel bindings. A CLI entry point that reads config and launches agents.

**Files:**
- Create: `langgraph-agents/agents.jsonc`
- Create: `langgraph-agents/__main__.py`

**Step 1: Write agents.jsonc**

```jsonc
{
  // Agent configuration for the LangGraph MCP realtime system.
  // Each agent binds a graph type to a DCF channel and LLM backend.
  "agents": [
    {
      "name": "echo-agent",
      "graph": "base",              // "base" | "coordinator" | custom
      "llm_backend": "echo",        // "echo" | "grok" | custom
      "channel": "echo",
      "mesh_url": "http://127.0.0.1:8765",
      "poll_interval": 1.0
    },
    {
      "name": "coordinator-agent",
      "graph": "coordinator",
      "llm_backend": "grok",
      "channel": "agent",
      "mesh_url": "http://127.0.0.1:8765",
      "poll_interval": 0.5
    }
  ],
  "mesh_server": {
    "command": "python3",
    "args": ["matrix-bridge/mesh_mcp.py", "http"],
    "port": 8765,
    "host": "127.0.0.1"
  }
}
```

**Step 2: Write __main__.py**

```python
"""CLI entry point: python -m langgraph_agents [config_path]

Launches all agents declared in the config, each in its own asyncio task.
Also starts the mesh_mcp.py server as a subprocess if configured.
"""
from __future__ import annotations
import asyncio
import json
import logging
import re
import subprocess
import sys
from pathlib import Path

from .runner import AgentRunner
from .coordinator import build_coordinator_graph

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(name)s] %(message)s")
logger = logging.getLogger("langgraph-agents")


def strip_jsonc(text: str) -> str:
    """Strip // and /* */ comments, allow trailing commas."""
    text = re.sub(r'//.*$', '', text, flags=re.MULTILINE)
    text = re.sub(r'/\*.*?\*/', '', text, flags=re.DOTALL)
    text = re.sub(r',\s*([}\]])', r'\1', text)
    return text


def load_config(path: str) -> dict:
    raw = Path(path).read_text()
    return json.loads(strip_jsonc(raw))


def build_runner(agent_cfg: dict) -> AgentRunner:
    """Construct an AgentRunner from a config block."""
    graph_type = agent_cfg.get("graph", "base")
    if graph_type == "coordinator":
        runner = AgentRunner(
            graph_backend=agent_cfg.get("llm_backend", "echo"),
            mesh_url=agent_cfg.get("mesh_url", ""),
            channel=agent_cfg.get("channel", "agent"),
            poll_interval=agent_cfg.get("poll_interval", 1.0),
        )
        runner.graph = build_coordinator_graph()
        return runner
    return AgentRunner(
        graph_backend=agent_cfg.get("llm_backend", "echo"),
        mesh_url=agent_cfg.get("mesh_url", ""),
        channel=agent_cfg.get("channel", "agent"),
        poll_interval=agent_cfg.get("poll_interval", 1.0),
    )


async def main(config_path: str = "agents.jsonc") -> None:
    config = load_config(config_path)

    # Optionally start mesh_mcp.py as a subprocess
    mesh_srv = config.get("mesh_server")
    mesh_proc = None
    if mesh_srv:
        cmd = [mesh_srv["command"]] + mesh_srv.get("args", [])
        logger.info(f"Starting mesh MCP server: {' '.join(cmd)}")
        mesh_proc = subprocess.Popen(cmd)
        await asyncio.sleep(1.0)  # let it bind

    # Launch all agents
    runners = [build_runner(a) for a in config.get("agents", [])]
    tasks = [asyncio.create_task(r.run_loop()) for r in runners]
    logger.info(f"Launched {len(tasks)} agent(s)")

    try:
        await asyncio.gather(*tasks)
    except KeyboardInterrupt:
        logger.info("Shutting down...")
        for r in runners:
            r.stop()
    finally:
        if mesh_proc:
            mesh_proc.terminate()


if __name__ == "__main__":
    config_file = sys.argv[1] if len(sys.argv) > 1 else "agents.jsonc"
    asyncio.run(main(config_file))
```

**Step 3: Commit**

```bash
git add langgraph-agents/agents.jsonc langgraph-agents/__main__.py
git commit -m "feat: CLI entry point and JSONC config for multi-agent launch"
```

---

### Phase 8: Integration Test

### Task 9: End-to-end integration test

**Objective:** Spin up mesh_mcp.py + two agents, verify a message roundtrips through the graph.

**Files:**
- Create: `langgraph-agents/tests/test_integration.py`

**Step 1: Write integration test**

```python
# langgraph-agents/tests/test_integration.py
"""Integration test: verify the full stack without network.

Tests that a message flows: input → graph.invoke → response output.
Does NOT require a running mesh_mcp server (uses echo backend + direct invoke).
"""
import pytest
from langgraph_agents.runner import AgentRunner
from langgraph_agents.coordinator import build_coordinator_graph


def test_echo_agent_roundtrip():
    runner = AgentRunner(graph_backend="echo", mesh_url="", channel="test")
    response = runner.process_message("ping", "0x0001", "test")
    assert response == "ping"


def test_coordinator_echo_route():
    graph = build_coordinator_graph()
    result = graph.invoke({
        "messages": [{"role": "user", "content": "echo: test message"}],
        "channel": "test",
        "sender": "0x0001",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "echo"
    assert result["response"] == "test message"


def test_coordinator_summarizer_route():
    graph = build_coordinator_graph()
    long_text = "summarize: " + "x" * 200
    result = graph.invoke({
        "messages": [{"role": "user", "content": long_text}],
        "channel": "test",
        "sender": "0x0002",
        "routing_decision": "",
        "response": "",
        "metadata": {},
    })
    assert result["routing_decision"] == "summarizer"
    assert len(result["response"]) <= 103  # 100 + "..."


def test_streaming_chunks():
    from langgraph_agents.streaming_bridge import chunk_response
    chunks = chunk_response("a" * 10000, max_chunk=4092)
    assert len(chunks) == 3
    assert all(len(c.encode("utf-8")) <= 4092 for c in chunks)
```

**Step 2: Run all tests**

Run: `cd langgraph-agents && python -m pytest tests/ -v`
Expected: all passed (10+ tests)

**Step 3: Commit**

```bash
git add langgraph-agents/tests/test_integration.py
git commit -m "test: end-to-end integration tests for agent stack"
```

---

## Files Likely to Change

| File | Action |
|------|--------|
| `langgraph-agents/pyproject.toml` | Create |
| `langgraph-agents/__init__.py` | Create |
| `langgraph-agents/__main__.py` | Create |
| `langgraph-agents/state.py` | Create |
| `langgraph-agents/agent_graph.py` | Create |
| `langgraph-agents/llm_node.py` | Create |
| `langgraph-agents/runner.py` | Create |
| `langgraph-agents/coordinator.py` | Create |
| `langgraph-agents/streaming_bridge.py` | Create |
| `langgraph-agents/mcp_tools.py` | Create |
| `langgraph-agents/agents.jsonc` | Create |
| `langgraph-agents/specialists/echo.py` | Create |
| `langgraph-agents/specialists/summarizer.py` | Create |
| `langgraph-agents/tests/test_state.py` | Create |
| `langgraph-agents/tests/test_mcp_tools.py` | Create |
| `langgraph-agents/tests/test_agent_graph.py` | Create |
| `langgraph-agents/tests/test_runner.py` | Create |
| `langgraph-agents/tests/test_coordinator.py` | Create |
| `langgraph-agents/tests/test_streaming_bridge.py` | Create |
| `langgraph-agents/tests/test_integration.py` | Create |
| `matrix-bridge/mesh_mcp.py` | Possibly extend with `mesh_subscribe` resource (Phase 9+) |

---

## Tests / Validation

- **Unit tests per phase** — each task includes TDD cycle with pytest
- **Integration test** — full graph invoke without network (Phase 8)
- **Live validation** — start `mesh_mcp.py http` + `python -m langgraph_agents agents.jsonc`, send a DCF-Text message from another node, verify echo response arrives
- **Streaming validation** — send a long message (>4092B response), verify chunked delivery

---

## Risks, Tradeoffs, and Open Questions

1. **MCP HTTP vs stdio**: The plan uses streamable-HTTP for agent↔mesh communication. If agents run on the same host as the MCP server, stdio with subprocess spawning is simpler but doesn't scale to multiple agents. HTTP is the right default.

2. **Latency**: Polling `mesh_recv` adds latency (poll_interval). A future optimization is MCP streaming resources (SSE) so the agent gets push notifications instead of polling.

3. **LangGraph version**: LangGraph is pre-1.0 and APIs shift. Pin `langgraph>=0.2,<0.4` and test against the installed version.

4. **Multi-agent coordination**: The current coordinator uses keyword-prefix routing. A production system needs LLM-based routing or a learned classifier. The graph structure supports swapping the route node.

5. **DCF-Text chunking**: Responses >4092B need multi-message reassembly on the receiver side. The current `TextReassembler` handles single-message fragmentation; multi-message correlation (FLAG_MORE) needs extension.

6. **ITAR**: This code is in the HydraMesh repo (not dsp/). The LangGraph system communicates *over* DCF — it doesn't modify the wire protocol. No export-control concerns for this module.

7. **LLM provider lock-in**: Currently Grok (xAI) is the default real backend. The `BACKENDS` dict makes adding OpenAI/Anthropic/local trivial — just add a callable.
