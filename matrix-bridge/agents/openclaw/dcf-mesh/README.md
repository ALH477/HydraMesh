# dcf-mesh — a ClawHub-style skill for OpenClaw

Puts an OpenClaw agent on the DCF agent-to-agent mesh so it can talk directly to another
MCP-capable agent over the certified 17-byte DeModFrame wire. The skill *is* the
`mesh_mcp.py` MCP server (ClawHub skills are MCP servers); `SKILL.md` is the instruction
file, `mcporter.json` is the registration.

## Install

1. Ensure the MCP server's one dependency is available: `pip install mcp` (or use the repo's
   `nix run .#mesh-agent`, which bundles it).
2. Merge the `dcf-mesh` entry from `mcporter.json` into
   `~/.openclaw/workspace/config/mcporter.json`. Fix the `args` path, pick a distinct
   `DCF_AGENT_NODE_ID`, set `DCF_PEERS` to your partner's `host:port`, and share `DCF_CHANNEL`.
   Regenerate it for your machine instead of hand-editing:
   ```sh
   python3 matrix-bridge/a2a.py config --agent openclaw                    # stdio, own identity
   python3 matrix-bridge/a2a.py config --agent openclaw --transport http   # shared mesh-http service
   ```
3. Restart OpenClaw; the `dcf-mesh` tools (`mesh_status`, `mesh_send`, `mesh_recv`, …) appear.
   Confirm with `mesh_status`, then follow the turn-taking recipe in `SKILL.md`.

## Two ways to run

- **stdio (default, recommended):** OpenClaw spawns its own `mesh_mcp.py` → its **own** mesh
  identity (`src` node id) and inbox. Use this for distinct agents.
- **HTTP (shared `mesh-http` service):** the `--transport http` form points `mcp-remote` at a
  running `mesh_mcp.py http`. **Caveat:** one service = one mesh identity shared by every client
  on it (they collide). Prefer stdio unless you specifically want one shared identity.

See `../NATIVE_CHANNEL.md` for the planned deeper integration (the mesh as a first-class OpenClaw
channel), and `matrix-bridge/AGENT_TO_AGENT.md` for the protocol.
