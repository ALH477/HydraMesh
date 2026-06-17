# Design note: DCF mesh as a native OpenClaw channel (follow-up)

The shipped `dcf-mesh/` skill is the **immediate** integration: OpenClaw consumes the mesh
through MCP tools (`mesh_send`/`mesh_recv`). This note sketches the **deeper, follow-up**
integration — making the DCF mesh a **first-class OpenClaw messaging channel**, like the
12+ platforms (Matrix, Slack, …) its daemon already bridges — so mesh peers appear as a chat
surface and the agent's normal channel routing/memory applies. **No daemon code ships in this
pass**; it needs an OpenClaw-side platform/event API we won't guess at. This is the blueprint
for when that API is pinned.

## It is the Matrix bridge with the endpoints swapped

`matrix-bridge/bridge.py` already implements exactly this shape for Matrix. A native OpenClaw
channel is the same gateway with the Matrix CS-API calls replaced by OpenClaw's platform API:

```
   OpenClaw platform  ⇄  channel adapter (owns a DcfTextNode)  ⇄  DCF mesh (DeModFrame/UDP)
        (today: Matrix homeserver  ⇄  bridge.py  ⇄  mesh)
```

**Reuse from `bridge.py` verbatim:**
- **Node ownership / config** — construct a single `DcfTextNode(node_id, bind_host, port,
  channel, use_superpack)` from config, exactly as `MatrixBridge.__init__` does.
- **mesh → platform** — set `node.on_message = self._on_mesh_message`; the callback fires on
  every reassembled message (`src`, `dst`, `text`, `flags`). Forward it to OpenClaw as an
  inbound channel event instead of POSTing to a Matrix room.
- **platform → mesh** — on an outbound OpenClaw message, call `node.send_text(text, flags=0x01)`
  (`FLAG_AGENT`). Reuse `bridge.py`'s `_chunks(body, 4092)` UTF-8-boundary splitter to respect
  the 4092-byte per-message cap (chunks ride `FLAG_MORE`).
- **Rendezvous** — peers/channel come from config (`DCF_PEERS` / `DCF_CHANNEL`), same as the skill.

## What's genuinely new (the OpenClaw side)

- A **channel/platform plugin** registered with OpenClaw's gateway: declare a `dcf-mesh`
  channel, map an inbound mesh message → an OpenClaw event, and an outbound OpenClaw message →
  `node.send_text`. The unknown is the exact plugin/event interface OpenClaw exposes.
- **Identity & sessions** — map each mesh `src` node id to a stable OpenClaw "contact" so memory
  and session routing work per-peer (the mesh already gives a distinct `src` per agent).
- **Presence** — once the parent plan's Phase 2 (BEACON presence) lands, surface learned peers as
  channel "online" state; until then, peers are config-static.

## Why a follow-up, not now

The MCP skill ships today with **zero** OpenClaw-internal coupling — it's a standard MCP server.
A native channel requires building against OpenClaw's platform API surface, which should be
verified against a specific OpenClaw version rather than guessed. When that lands, this adapter
is a ~150-line daemon: a `bridge.py` clone with the Matrix half replaced. Track it as
`matrix-bridge/agents/openclaw/dcf_channel.py` when implemented.
