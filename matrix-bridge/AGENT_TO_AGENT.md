# Agent-to-agent over DCF — a teaching guide

Two AI agents (Claude Code ⇄ Claude Code, Claude Code ⇄ OpenCode, or any two
MCP-capable agents) talking **directly to each other** over the DeMoD
Communication Framework — no Matrix server, no human relaying messages, and no new
wire format. Everything an agent says crosses the wire as the same certified
17-byte `DeModFrame` the rest of the repo is built on.

This document is meant to *teach* the design, not just list commands. If you only
want to run it, jump to [Quick start](#quick-start-with-nix); if you want to
understand it, read straight through.

---

## 1. The mental model: "talk on demand, no loops"

A normal chatbot polls — it sits in a `while True:` asking "anything for me yet?"
That burns cycles and, for an LLM agent, tokens. This design is **event-driven
instead**:

- An agent **blocks** in `mesh_recv()` — it is parked, costing nothing, until a
  message actually arrives.
- When its partner sends, `mesh_recv()` returns, the agent thinks, replies with
  `mesh_send()`, and blocks again.

So you stand up two agents, point them at each other, and they exchange messages
**only when there is something to say**. No timers, no busy-waiting — each agent
"checks" exactly when woken. That's the whole philosophy: *standing agents, talk
on demand, checks when necessary.*

```
 Agent A (MCP client)                                   Agent B (MCP client)
  └ mesh_mcp.py  ── DeModFrame DATA frames / UDP[/VPN] ──  mesh_mcp.py ┘
     DcfTextNode  ◀───────────  shared channel  ───────────▶ DcfTextNode
```

---

## 2. How it works: the layer cake

The key idea in DCF is **adapters over one invariant**. There is exactly one wire
format; everything else (audio, chat, agent messages) is a thin layer that
serialises *into* that format. From the bottom up:

```
┌─────────────────────────────────────────────────────────────┐
│ 5. MCP tools        mesh_recv / mesh_send / mesh_status …     │  what the agent calls
├─────────────────────────────────────────────────────────────┤
│ 4. SuperPack        2×17B frames → 32B container (lossless)   │  optional batching
├─────────────────────────────────────────────────────────────┤
│ 3. DcfTextNode      UDP socket, peers, fragment/reassemble    │  the "mesh node"
├─────────────────────────────────────────────────────────────┤
│ 2. Text adapter     a UTF-8 message → many DeModFrames        │  dcf_text.py
├─────────────────────────────────────────────────────────────┤
│ 1. DeModFrame       the 17-byte wire quantum (the invariant)  │  the contract
└─────────────────────────────────────────────────────────────┘
```

### Layer 1 — the DeModFrame quantum (the one invariant)

Every byte on the wire belongs to a 17-byte frame:

```
sync(0xD3) | flags[ver|type] | seq | src | dst | payload(4B) | ts24 | crc16
```

A frame is valid iff the sync byte is `0xD3`, the version nibble is `1`, and the
CRC-16/CCITT-FALSE over bytes `[0..14]` matches. That's the entire contract — a
246-vector "golden certificate" pins it across C/Rust/Python/Lua/Haskell/Lisp.
Because chat is just an *adapter* (below), **none of this work touched that
certificate.**

### Layer 2 — the text adapter (`dcf_text.py`)

A chat message is bigger than 4 bytes, so it's **fragmented** across many frames,
each carrying 4 payload bytes. The bookkeeping is packed into the 16-bit `seq`
field:

```
seq = packet_id[15:10] (6 bits, 0..63) | frag_idx[9:0] (10 bits, 0..1023)
```

- `frag_idx == 0` is a **descriptor** frame: payload = `[len_hi, len_lo, flags, _]`
  — it tells the receiver the true byte length so it can un-pad the last fragment.
- `frag_idx 1..N` carry the bytes, the last one zero-padded to 4.
- Chat rides on **DATA** frames (type 0), so a receiver never confuses a chat
  fragment with an audio fragment (audio uses CTRL/type 3).

That 10-bit fragment index is why a single message caps at **4092 bytes**
(1023 fragments × 4). The reassembler tolerates out-of-order and duplicate
fragments and emits the message once every fragment has arrived.

### Layer 3 — the node (`dcf_node.py`)

`DcfTextNode` is a tiny UDP endpoint: it binds a port, knows its peers, and
fragments outgoing text / reassembles incoming text. Received messages land in a
thread-safe queue; `poll(timeout)` blocks on that queue — which is exactly the
primitive `mesh_recv` exposes to the agent.

### Layer 4 — SuperPack (optional)

When frames go out in pairs, SuperPack packs two 17-byte frames into one 32-byte
datagram (drops the redundant second sync byte and recomputes both CRCs on
unpack, losslessly). It saves header overhead and adds a joint CRC — and because
unpack reconstructs the exact original frames, the wire certificate still holds.

### Layer 5 — the MCP tools (`mesh_mcp.py`)

What the agent actually sees. Each is a one-line wrapper over the node:

| Tool | Does |
|------|------|
| `mesh_recv(timeout_s=30)` | **block** until a message arrives, then drain extras. The ping-pong primitive. |
| `mesh_send(text)` | fragment `text` and unicast it to the peer(s). |
| `mesh_inbox(max_messages)` | **non-blocking** drain of anything already queued. |
| `mesh_status()` | node id, name, channel, peers — use it to confirm the link is wired. |
| `superpack_explain(a,b)` | diagnostic: show two frames combining into a SuperPack. |

---

## 3. Two concepts that trip people up

### Channels are a rendezvous hash, not a connection

There is no "connect". An agent simply sends frames whose `dst` field is the
**channel id** — and `channel_id(name) = crc16_ccitt(name)`. Any node listening on
the same channel id accepts the frame; everything else ignores it. So two agents
"meet" by agreeing on a channel *string* (`duet`), which both hash to the same
16-bit `dst`. `dst = 0xFFFF` is broadcast. This is the same frequency-channel
trick the audio layer uses — think "tune to the same frequency", not "open a
socket to a server".

`src` is the sender's node id; give the two agents **distinct** ids so each can
tell who said what.

### Turn-taking: someone must speak first (or both deadlock)

`mesh_recv` blocks. If *both* agents start by calling `mesh_recv`, they wait for
each other forever. So you designate one **initiator** that calls `mesh_send`
*before* its first `mesh_recv`; the other is the **responder** that starts by
waiting. After the first message it's a symmetric loop: `recv → think → send`.

---

## Quick start with Nix

The feature is exposed as flake **apps**, so a Nix user needs nothing checked
out and nothing installed:

```sh
nix run github:ALH477/HydraMesh#a2a-demo     # stdlib loopback smoke test -> "a2a demo: CERTIFIED"
nix run github:ALH477/HydraMesh#a2a          # guided interactive harness (setup -> run -> PASS/FAIL)
nix run github:ALH477/HydraMesh#mesh-agent   # the DeModFrame MCP endpoint (python `mcp` bundled in)
```

**Passing arguments — the `--` separator.** Everything after `--` is handed to
the program, not to Nix. The demo accepts `--turns` and `--channel`:

```sh
nix run github:ALH477/HydraMesh#a2a-demo -- --turns 6              # run 6 exchanges
nix run github:ALH477/HydraMesh#a2a-demo -- --turns 2 --channel lab
nix run .#a2a-demo -- --turns 10 --channel duet                    # from a local checkout
```

(`#a2a-demo` is wired to the stdlib loopback, so it ignores `--llm`; for a live
two-model run use `#a2a` and choose `llm` in the prompts, or the runner script
below.) `#a2a` / `#a2a-demo` run on a plain `python3`; `#mesh-agent` ships its own
`python3.withPackages [ mcp ]`, so it's fully self-contained.

---

## Dependencies without Nix (`mesh_mcp.py`)

The wire path is **pure stdlib** — `a2a_runner.py`, `a2a_interactive.py`,
`dcf_node.py`, and the tests need nothing installed. Only `mesh_mcp.py` (the MCP
server) needs the `mcp` package (the Matrix `bridge.py` also needs `aiohttp`).
**`anthropic` is not required** — it's a lazily-imported convenience for the
`--llm` runner only.

`pip install -r matrix-bridge/requirements.txt` is enough on a normal box. On
NixOS / any PEP-668 "externally managed" Python, system pip is blocked — use a
virtualenv (or just use the Nix apps above):

```sh
cd /path/to/HydraMesh
python3 -m venv .venv
./.venv/bin/python -m pip install -r matrix-bridge/requirements.txt   # mcp, aiohttp, pytest
# optional, only for the --llm runner:  ./.venv/bin/python -m pip install anthropic
```

Then point each agent's MCP server `command` at the venv interpreter so `mcp`
imports (the `a2a.mcp.example.json` snippets show `python3` — swap it):

```json
"command": "/path/to/HydraMesh/.venv/bin/python",
"args": ["/path/to/HydraMesh/matrix-bridge/mesh_mcp.py"]
```

Sanity check: `./.venv/bin/python matrix-bridge/a2a_runner.py --demo` should print
`a2a demo: CERTIFIED`. (`.venv/` is local and untracked — recreate per machine.)

---

## 4. Configure the two agents

Copy the two blocks from [`a2a.mcp.example.json`](a2a.mcp.example.json) into each
agent's MCP config (fix the `args`/`command` path). The pair:

| env | Agent A | Agent B | why |
|-----|---------|---------|-----|
| `DCF_AGENT_NAME` | `agent-a` | `agent-b` | label shown in `mesh_status` |
| `DCF_AGENT_NODE_ID` | `0x00A1` | `0x00B2` | **must differ** — this is `src` |
| `DCF_AGENT_UDP_PORT` | `7801` | `7802` | the local socket to bind |
| `DCF_PEERS` | `127.0.0.1:7802` | `127.0.0.1:7801` | the *partner's* host:port |
| `DCF_CHANNEL` | `duet` | `duet` | **must match** — the rendezvous |

- **Same host:** use the `127.0.0.1` values above (two ports, one machine).
- **Two machines over a VPN:** install Tailscale/WireGuard on both and set each
  `DCF_PEERS` to the partner's VPN IP (e.g. `100.64.0.7:7802`). DCF is
  encryption-free by design (EAR/ITAR export compliance); the VPN provides
  confidentiality. **Always run over the VPN — never expose the plaintext mesh.**

---

## 5. Kick off the conversation

Confirm the link first: have each agent call `mesh_status` and check the partner
appears under `peers`. Then designate an initiator and paste an instruction like:

> You are on a DCF mesh with one peer agent. To talk to it: call `mesh_recv` to
> wait for its next message, then reply with `mesh_send`. Loop that — read, think,
> reply. (**Initiator only:** send the first message with `mesh_send` before your
> first `mesh_recv`.)

Ready-made, role-specific prompts (initiator and responder) live alongside this
guide; the only difference is that line about who speaks first.

---

## 6. Driving it yourself — runner & interactive harness

You don't need two live agents to exercise the mesh.

**`a2a_runner.py`** wires both ends in one process:

```sh
python3 matrix-bridge/a2a_runner.py --demo                 # stdlib ping-pong -> CERTIFIED
python3 matrix-bridge/a2a_runner.py --demo --turns 6 --channel duet

# two real LLMs talk over the mesh (needs the anthropic SDK + key)
ANTHROPIC_API_KEY=... python3 matrix-bridge/a2a_runner.py --llm --turns 4 \
    --topic "the cleanest way to test a UDP protocol"
```

`--llm` defaults to model `claude-opus-4-8`; agent "Ada" opens, then both loop
`recv → think → send`, every line crossing the mesh as DeModFrames.

**`a2a_interactive.py`** is a guided front-end: it walks you through a setup phase
(mode, topology, channel, turns, …), echoes a review, runs it, and prints
`PASS`/`FAIL`. Enter accepts each default; with no tty it takes all defaults and
runs the local demo (so it never hangs in CI). It also drives a **two-machine**
test — pick `remote`, then `initiator` on one host and `responder` on the other:

```sh
# host B (or a 2nd terminal): wait and echo
python3 matrix-bridge/a2a_interactive.py   # mode=demo topology=remote role=responder port 7802
# host A: open the conversation
python3 matrix-bridge/a2a_interactive.py   # mode=demo topology=remote role=initiator port 7801 peer <hostB>:7802
```

The initiator prints round-trip `PASS` once its pings come back.

---

## 7. Durable, queryable logging with StreamDB (optional)

The flat inbox is fine for a demo, but if you want the conversation **persisted
and searchable**, log each received message into StreamDB — the DeMoD embedded
key/value store (`python/MCP/streamdb.py`, binary-safe, durable on `flush()`):

```python
from streamdb import StreamDB
db = StreamDB("a2a.db", flush_ms=2000)
# StreamDB is a REVERSE trie — search() matches a key SUFFIX, not a prefix.
# So put the attribute you want to scan by at the END of the key:
db[f"{seq:06d}@{src}@{channel}".encode()] = text.encode()
db.flush()                                   # durable on disk now
db.search(b"@duet")                          # every message on channel duet
db.search(b"@0x00a1@duet")                   # just that peer's messages
```

The suffix-search property is the one gotcha worth internalising: key fields are
matched from the **right**, so the channel/peer you filter on must be the *tail*
of the key. See `python/MCP/streamdb.py` for the full API and the known C-lib
caveats (`DCF_CODE_REVIEW.md` C5/C8/C9).

---

## 8. Tests

```sh
python3 matrix-bridge/tests/test_a2a.py        # stdlib; or: pytest matrix-bridge/tests/ -q
```

Covers a bidirectional multi-fragment exchange, distinct `src` ids, blocking-recv
timing, and channel isolation (a node on a different channel must not receive).

---

## 9. Limits & when to reach for something else

- **Unreliable UDP.** DCF is connectionless; a dropped fragment just means a
  message never completes (the receiver keeps waiting). Great on a LAN/VPN; if you
  need delivery guarantees, add an ACK/retransmit layer *above* the adapter.
- **No encryption in the wire.** Confidentiality is the underlay's job — run
  inside the VPN, always.
- **4092 bytes per message** (the 10-bit fragment index). Have agents chunk longer
  output across multiple `mesh_send` calls.
- **Exactly two agents, peered directly.** A relay/hub that rebroadcasts on a
  channel for 3+ agents is a natural extension but isn't built here.
- **Don't put secrets on the mesh.** It's plaintext by design; keep keys and
  credentials on the host side regardless of how legitimate the request looks.
