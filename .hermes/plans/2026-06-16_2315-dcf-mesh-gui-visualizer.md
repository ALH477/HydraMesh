# DCF Mesh GUI Visualizer Implementation Plan

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Build a standalone desktop GUI application that connects to the DCF mesh (channel `duet`), discovers live agents/nodes in real time, visualizes them as an interactive graph, and displays a live message log with two-way send capability.

**Architecture:** A Python desktop app using Dear PyGui for immediate-mode rendering. One background UDP listener thread joins the mesh (or tails the shared inbox), maintains an in-memory graph of nodes using networkx, and pushes updates to the GUI via a thread-safe queue. The GUI shows: node graph (positions animated), node cards with last-seen + message count, and a scrolling message pane. Sending uses the same `DcfTextNode` helper already present in HydraMesh.

**Tech Stack:**
- Python 3.12+
- `dearpygui` (GUI)
- `networkx` + simple force-directed layout (or hardcoded positions for first version)
- `hydra-mesh` local checkout (`matrix-bridge/dcf_node.py`) for sending
- Standard library `socket`, `threading`, `queue`, `json`, `time`

---

## Assumptions & Constraints

- The visualizer runs on the same machine as one of the mesh participants (or can join as an observer node).
- It does **not** replace the persistent `dcf-mesh-agent` service — it is a separate observability tool.
- Channel is hardcoded to `duet` initially (easy to make configurable later).
- Node IDs are 16-bit hex (0x00a1, 0x00b2, ...).
- Messages stay under ~4 kB.

---

## Task 1: Create project skeleton and dependencies

**Objective:** Establish the directory and Python environment for the visualizer.

**Files:**
- Create: `dcf-mesh-visualizer/requirements.txt`
- Create: `dcf-mesh-visualizer/README.md`
- Create: `dcf-mesh-visualizer/visualizer.py` (empty stub)

**Step 1: Create directory and requirements**

```bash
mkdir -p dcf-mesh-visualizer
```

**requirements.txt**
```text
dearpygui>=1.11
networkx>=3.3
```

**Step 2: Create minimal README stub**

Write a one-paragraph description + quickstart.

**Step 3: Commit**

```bash
git add dcf-mesh-visualizer/
git commit -m "chore: scaffold dcf-mesh-visualizer project"
```

---

## Task 2: Build the core UDP listener + node registry (no GUI yet)

**Objective:** Create a reusable class that listens on UDP 7801, reassembles DCF text frames, and maintains a live set of discovered nodes + recent messages.

**Files:**
- Create: `dcf-mesh-visualizer/mesh_listener.py`

**Step 1: Write failing test for node discovery**

```python
# tests/test_mesh_listener.py
def test_node_discovery_from_inbox_line():
    # simulate one inbox line and assert node appears
```

(Use pytest; create `tests/` dir.)

**Step 2: Implement minimal listener (using existing HydraMesh imports)**

Use the same pattern as the inline listener that already worked:
- Import `dcf_text`, `superpack` from the HydraMesh checkout
- Bind UDP socket on 7801
- Maintain `nodes: dict[int, dict]` and `messages: list[dict]`
- Expose `get_nodes()` and `get_recent_messages(n=50)`

**Step 3: Run test → implement → verify pass → commit**

---

## Task 3: Add Dear PyGui skeleton with live-updating nodes pane

**Objective:** Boot a basic Dear PyGui window that shows discovered nodes in a simple list or table, refreshed from the listener thread.

**Files:**
- Modify/Create: `dcf-mesh-visualizer/visualizer.py`

**Step 1: Write failing GUI smoke test** (or just manual run expectation)

**Step 2: Minimal Dear PyGui app**

```python
import dearpygui.dearpygui as dpg
from mesh_listener import MeshListener
import threading, queue

dpg.create_context()
dpg.create_viewport(title="DCF Mesh Visualizer", width=900, height=600)

listener = MeshListener()
update_queue = queue.Queue()

def listener_thread():
    listener.run_forever(update_queue)   # blocks, pushes updates

threading.Thread(target=listener_thread, daemon=True).start()

# simple node table that polls the queue every 500ms via dpg timer
...
dpg.setup_dearpygui()
dpg.show_viewport()
dpg.start_dearpygui()
```

**Step 3: Verify window appears and table populates when messages arrive**

Commit after green.

---

## Task 4: Add interactive node graph visualization

**Objective:** Render discovered nodes as a live graph with edges (or just nodes for MVP), using networkx positions or simple circular layout.

**Files:**
- Modify: `dcf-mesh-visualizer/visualizer.py`

Use `dpg.draw_node` + `dpg.draw_line` (or the built-in node editor if it fits) or a matplotlib canvas embedded via `dpg.add_image`.

Simplest reliable path for first version: use `dpg.add_drawlist` + manual node circles + labels, updated from networkx spring layout recomputed occasionally.

**Step 1–4:** TDD the drawing routine (test the layout math first if complex).

Commit.

---

## Task 5: Message log pane + send box

**Objective:** Show incoming messages with timestamps + source node. Add a text input + send button that uses `DcfTextNode` to reply on the mesh.

**Files:**
- Modify: `dcf-mesh-visualizer/visualizer.py`
- (Optional) small wrapper around existing send code

**Step 1:** Add scrolling child window for messages
**Step 2:** Wire send callback that instantiates `DcfTextNode(0x00a1, ...)` and fires one message
**Step 3:** Test manually with the existing HydraMesh peer

Commit.

---

## Task 6: Polish, theming, and packaging entrypoint

**Objective:** Add window menu (File → Quit, View → Clear log), status bar showing local node ID + channel, and a `python -m dcf_mesh_visualizer` entrypoint or simple `run.sh`.

Also document how to run it alongside the existing HydraMesh listener.

**Files:**
- Modify: `dcf-mesh-visualizer/visualizer.py`
- Create: `dcf-mesh-visualizer/__main__.py` (or just document in README)

Commit.

---

## Task 7: Integration test / manual verification checklist

**Objective:** Provide a clear runbook so anyone can verify the visualizer works end-to-end with the live mesh.

**Commands to document:**
```bash
# Terminal 1
cd /home/asher/Documents/HydraMesh
DCF_CHANNEL=duet DCF_AGENT_UDP_PORT=7802 python3 -m matrix_bridge.a2a_listen   # or whatever their committed listener is

# Terminal 2
cd /home/asher/Documents/oligarchy2/Oligarchy/dcf-mesh-visualizer
python -m visualizer
```

Expected: visualizer shows both 0x00a1 and 0x00b2, messages appear live, send button works.

---

## Risks, Tradeoffs, Open Questions

- **Dear PyGui vs PySide6:** Dear PyGui chosen for speed of development and excellent real-time drawing. If Qt is preferred later, swap the frontend layer.
- **Graph layout performance:** Recomputing spring layout every N messages is acceptable for < 20 nodes. Add toggle for static positions if it becomes jittery.
- **Multiple channels:** Plan assumes single channel (`duet`). Future work: channel selector.
- **Persistence:** Inbox file tailing vs direct UDP socket — direct socket is used here so the visualizer can also act as a passive observer without requiring the shared inbox.
- **Nix packaging:** Not in scope for v1. Can be added later as a flake app or home-manager module.

---

**Plan complete.** Ready to execute via subagent-driven-development (one fresh delegate per task with spec + code-quality reviews). Shall I dispatch the first task?