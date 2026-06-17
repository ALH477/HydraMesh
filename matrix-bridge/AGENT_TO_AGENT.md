# Agent-to-agent over DCF — two coding agents talking on the DeModFrame mesh

The Matrix bridge connects **one** agent to a **human**. This connects **two
agents to each other**: Claude Code ⇄ Claude Code (or Claude Code ⇄ OpenCode, or
any two MCP-capable agents) exchanging messages directly over the 17-byte
`DeModFrame` wire quantum — no Matrix, no human, no new wire format. Chat text is
the same certified DATA-frame adapter the bridge already uses (`dcf_text.py`), so
the repo's 246-vector wire certificate is untouched.

```
 Agent A (MCP client)                                   Agent B (MCP client)
  └ mesh_mcp.py  ── DeModFrame DATA frames / UDP[/VPN] ──  mesh_mcp.py ┘
     DcfTextNode  ◀───────────  shared channel  ───────────▶ DcfTextNode
```

Each agent's MCP client launches its **own** `mesh_mcp.py`. The two servers peer
to each other (`DCF_PEERS`) on a shared channel (`DCF_CHANNEL`) with distinct node
ids; everything they say to each other is plaintext `DeModFrame` inside the
underlay (loopback for a local test, a VPN tunnel for two machines).

## Tools each agent has

`mesh_mcp.py` exposes (see its header for the full list):

- `mesh_recv(timeout_s=30)` — **block** until the partner sends, then drain any
  extras. This is the one to use for ping-pong: wait for a reply instead of
  busy-polling.
- `mesh_send(text)` — send text onto the mesh (reaches the single peer).
- `mesh_inbox(max_messages)` — non-blocking drain of anything already queued.
- `mesh_status()` — node id, name, channel, peers (confirm the partner is wired).

## 1. Configure the two agents

Copy the two blocks from [`a2a.mcp.example.json`](a2a.mcp.example.json) into each
agent's MCP config (fix the `args` path to this repo). The pair:

| env | Agent A | Agent B |
|-----|---------|---------|
| `DCF_AGENT_NAME` | `agent-a` | `agent-b` |
| `DCF_AGENT_NODE_ID` | `0x00A1` | `0x00B2` (must differ) |
| `DCF_AGENT_UDP_PORT` | `7801` | `7802` |
| `DCF_PEERS` | `127.0.0.1:7802` | `127.0.0.1:7801` |
| `DCF_CHANNEL` | `duet` | `duet` (must match) |

**Same host:** use the `127.0.0.1` values above.
**Two hosts over a VPN:** install Tailscale/WireGuard on both, and set each
`DCF_PEERS` to the *partner's* VPN IP (e.g. `DCF_PEERS=100.64.0.7:7802`). DCF is
encryption-free by design (EAR/ITAR export compliance) — the VPN provides
confidentiality, exactly as in the Matrix-bridge README. **Always run over the VPN.**

## 2. Start the conversation (turn-taking)

With `mesh_recv` blocking, the natural loop is ping-pong, but **someone has to
speak first** or both sides sit in `mesh_recv` forever. Designate an initiator and
paste an instruction like this into each agent:

> You are on a DCF mesh with one peer agent. To talk to it: call `mesh_recv` to
> wait for its next message, then reply with `mesh_send`. Loop that — read, think,
> reply. (Initiator only: send the first message with `mesh_send` before your
> first `mesh_recv`.)

Use `mesh_status` on each side first to confirm the peer is listed.

## 3. Hands-off runner (optional, no MCP client needed)

`a2a_runner.py` drives both ends itself — handy for a demo or a smoke test:

```sh
# stdlib only — two in-process nodes ping-pong canned lines over UDP
python3 a2a_runner.py --demo                 # -> "a2a demo: CERTIFIED"
python3 a2a_runner.py --demo --turns 6 --channel duet

# two real LLMs converse over the mesh (needs the anthropic SDK + key)
ANTHROPIC_API_KEY=... python3 a2a_runner.py --llm --turns 4 \
    --topic "the cleanest way to test a UDP protocol"
```

`--llm` defaults to model `claude-opus-4-8`; one agent (Ada) opens, then both loop
recv → think → send, every line crossing the mesh as DeModFrame frames.

### Interactive harness

`a2a_interactive.py` is a guided front-end to the runner: it walks you through a
setup phase (mode, topology, channel, turns, …), echoes a review, runs it, and
prints `PASS`/`FAIL`. Enter accepts each default; with no tty it takes all
defaults and runs the local demo (so it never hangs in CI).

```sh
python3 matrix-bridge/a2a_interactive.py        # answer the prompts
```

It also drives a **two-machine** test. On each host pick `remote`, then a role:

```sh
# host B (or a second terminal): wait and echo
python3 matrix-bridge/a2a_interactive.py
#   mode=demo  topology=remote  role=responder  local port 7802

# host A: open the conversation, peer = host B's VPN IP (127.0.0.1 to test locally)
python3 matrix-bridge/a2a_interactive.py
#   mode=demo  topology=remote  role=initiator  local port 7801  peer <hostB>:7802
```

The initiator prints round-trip `PASS` once its pings come back. Choose `llm`
instead of `demo` (with `ANTHROPIC_API_KEY` set) to put a live model on each side.

## 4. Tests

```sh
python3 matrix-bridge/tests/test_a2a.py        # stdlib; or: pytest matrix-bridge/tests/ -q
```

Covers a bidirectional multi-fragment exchange, distinct src ids, blocking-recv
timing, and channel isolation.

## Limits

- **Unreliable UDP.** DCF is connectionless; a dropped fragment just means a
  message never completes (the receiver waits). Fine on a LAN/VPN; add
  ACK/retransmit if you need delivery guarantees.
- **No encryption in DCF — run inside the VPN.**
- **4092 bytes per message** (10-bit fragment index). Have agents chunk longer
  output across messages.
- Strictly two agents, peered directly. A relay/hub for 3+ agents on one channel
  is possible but not built here.
