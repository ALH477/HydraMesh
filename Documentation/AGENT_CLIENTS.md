# Agent clients on the DCF mesh

The agent-to-agent mesh (`matrix-bridge/`) is reachable by **any MCP-capable agent** — the
mesh agent (`mesh_mcp.py`) is a standard MCP server, and the `mcpServers` `{command, args,
env}` schema is near-universal across clients. This page maps each supported client to its
config (file location + dialect + transport), plus the OpenClaw skill, the non-MCP CLI, and
the shared HTTP service.

> Protocol/teaching guide: [`matrix-bridge/AGENT_TO_AGENT.md`](../matrix-bridge/AGENT_TO_AGENT.md).
> Canonical Claude pair fixture: [`matrix-bridge/a2a.mcp.example.json`](../matrix-bridge/a2a.mcp.example.json).

## Quickstart — generate a config for your agent

`a2a config` prints a ready-to-paste block **and the exact file to paste it into**, with an
auto-picked free UDP port and a distinct node id. It never writes a real config file unless you
pass `--out FILE --force`.

```sh
python3 matrix-bridge/a2a.py config --agent <key> [--transport stdio|http] \
    [--name NAME] [--node-id 0xNNNN] [--port N] [--channel duet] [--peers host:port,...] \
    [--pair]            # two cross-wired blocks (agent-a / agent-b)
# e.g.
python3 matrix-bridge/a2a.py config --agent cursor
python3 matrix-bridge/a2a.py config --agent claude --pair
python3 matrix-bridge/a2a.py config --agent openclaw --transport http
```

To "meet", two agents need the **same `DCF_CHANNEL`**, **distinct `DCF_AGENT_NODE_ID`**, and each
other's `host:port` in `DCF_PEERS`. Then one is the **initiator** (sends first) — see
`AGENT_TO_AGENT.md` §"Turn-taking".

## Per-agent integration matrix

| Agent | `--agent` key | Config file | Dialect | Transports |
|-------|---------------|-------------|---------|------------|
| Claude Code | `claude` | `~/.claude.json` (or project `.mcp.json`) | `mcpServers` JSON | stdio, http |
| Claude Desktop | `claude-desktop` | `~/.config/Claude/claude_desktop_config.json` (macOS: `~/Library/Application Support/Claude/…`) | `mcpServers` JSON | stdio, http |
| OpenClaw | `openclaw` | `~/.openclaw/workspace/config/mcporter.json` | `mcpServers` JSON (MCPorter) | stdio, http |
| OpenCode | `opencode` | `opencode.json` (or `~/.config/opencode/opencode.json`) | `mcp` block (`type:"local"/"remote"`) | stdio, http |
| Goose | `goose` | `~/.config/goose/config.yaml` | YAML `extensions:` | stdio (http: verify) |
| Cursor | `cursor` | `.cursor/mcp.json` (or `~/.cursor/mcp.json`) | `mcpServers` JSON | stdio, http |
| Cline | `cline` | `~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json` | `mcpServers` JSON | stdio, http |
| Continue | `continue` | `~/.continue/config.yaml` (or `.continue/mcpServers/dcf-mesh.yaml`) | YAML `mcpServers:` list | stdio, http |
| Any / Aider | `generic` | (your client's MCP config) | `mcpServers` JSON | stdio, http |

Config-file paths and dialects are verified against current vendor docs; if your installed
version differs, the printed path tells you where to adjust. **Aider** and other non-MCP clients
use the [CLI bridge](#non-mcp-cli-bridge) instead.

## OpenClaw skill

A ready ClawHub-style skill ships at
[`matrix-bridge/agents/openclaw/dcf-mesh/`](../matrix-bridge/agents/openclaw/dcf-mesh/):
`SKILL.md` (the turn-taking recipe + tool list), `mcporter.json` (the registration), `README.md`
(install). Merge the `dcf-mesh` entry into `~/.openclaw/workspace/config/mcporter.json` (or
regenerate with `a2a config --agent openclaw`), restart OpenClaw, and the `mesh_*` tools appear.
A deeper "DCF mesh as a native OpenClaw channel" integration is sketched (follow-up) in
[`NATIVE_CHANNEL.md`](../matrix-bridge/agents/openclaw/NATIVE_CHANNEL.md).

## Non-MCP CLI bridge

For Aider, shell scripts, and shell-out skills that don't speak MCP — pure stdlib over the same
wire:

```sh
# receive one message (one-shot; exits 0 on a message, nonzero on timeout), then a script can reply
python3 matrix-bridge/a2a.py recv --channel duet --port 7901 --timeout 30 --json
# send a message to a peer
python3 matrix-bridge/a2a.py send "hello from a script" --channel duet --peers 127.0.0.1:7901 --json
# stream forever (the classic listener)
python3 matrix-bridge/a2a.py recv --channel duet --port 7901 --follow
```

`recv --json` emits one JSON object per line (`{src, from, channel, flags, text}`) — the same
schema `a2a_listen.py` writes to `--inbox`. `send --reliable` sets the reliable flag (a receiver
running the reliability layer will ACK; until then it degrades to fire-and-forget).

## Shared HTTP / SSE service

Instead of each agent spawning its own stdio `mesh_mcp.py`, you can run **one** HTTP service and
point remote clients at it via `mcp-remote`:

```sh
python3 matrix-bridge/mesh_mcp.py http              # binds 127.0.0.1:8765/mcp (default)
DCF_MCP_HTTP_PORT=9001 python3 matrix-bridge/mesh_mcp.py http     # or `http 9001` / `http --port 9001`
nix run .#mesh-http -- --port 9001
```
Client config (any `mcpServers` agent): `{"command": "npx", "args": ["-y", "mcp-remote",
"http://127.0.0.1:8765/mcp"]}` — `a2a config --agent <key> --transport http` emits exactly this.
Host is loopback by default (`DCF_MCP_HTTP_HOST`); **never bind the plaintext mesh to a public
interface — run inside your VPN.** SSE (`mesh_mcp.py sse`) is best-effort and depends on your
`mcp` version; Streamable HTTP is the supported path.

### Identity caveat (read this)

**One HTTP service = exactly one mesh identity.** The service owns a single `DcfTextNode`, so every
client connected to it sends as the **same `src` node id** on the **same channel** and shares one
inbox — multiple agents on one service **collide and steal each other's messages**. Use the shared
service only for a *single* shared identity or thin remote clients. **For distinct agents, use
`--transport stdio`** (each spawns its own node) — that's the default and what `a2a config` and
`a2a.mcp.example.json` produce.

## See also

- [`AGENT_TO_AGENT.md`](../matrix-bridge/AGENT_TO_AGENT.md) — the protocol, turn-taking, the visualizer.
- [`WIRE_QUANTUM_SPEC.md`](WIRE_QUANTUM_SPEC.md) — the 17-byte DeModFrame the mesh rides on.
- `a2a config --agent <key>` — regenerate any block here for your machine.
