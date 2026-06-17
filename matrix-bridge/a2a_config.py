"""Multi-agent MCP config generator for the DCF mesh agent (`a2a config`).

Emits a ready-to-paste config block (and prints the exact target file path) for each
supported agent client, so OpenClaw / OpenCode / Goose / Cursor / Cline / Continue /
Claude Code/Desktop can all join the same mesh as `a2a.mcp.example.json` wires Claude.

The `mcpServers` {command,args,env} schema is near-universal; the differences are the
config FILE PATH, a few custom dialects (OpenCode/Goose/Continue), and stdio vs HTTP.
Pure stdlib — JSON via `json`, the YAML dialects hand-emitted (no PyYAML dependency).

Safety: never writes a real config file unless the caller passes `--out`+`--force`;
default is stdout.
"""
import argparse
import json
import os
import socket
import sys
import zlib
from dataclasses import dataclass, field

HERE = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(HERE)
MESH_MCP = os.path.join(HERE, "mesh_mcp.py")
SERVER_NAME = "dcf-mesh"
DEFAULT_CHANNEL = "duet"
DEFAULT_HTTP_URL = "http://127.0.0.1:8765/mcp"


# ── parameters shared by every emitter ──────────────────────────────────────────
@dataclass
class Params:
    interp: str
    script: str
    env: dict
    url: str
    transport: str
    server_name: str = SERVER_NAME


def detect_interp():
    """Prefer a repo-local .venv (so `mcp` imports), else this interpreter, else python3."""
    venv = os.path.join(REPO_ROOT, ".venv", "bin", "python")
    if os.path.exists(venv):
        return venv
    return sys.executable or "python3"


def free_ports(n):
    """`n` distinct free UDP ports (bound simultaneously so they cannot collide)."""
    socks = [socket.socket(socket.AF_INET, socket.SOCK_DGRAM) for _ in range(n)]
    try:
        for s in socks:
            s.bind(("127.0.0.1", 0))
        return [s.getsockname()[1] for s in socks]
    finally:
        for s in socks:
            s.close()


def pick_node_id(seed):
    """Deterministic 16-bit id in 0x00A0..0x00BF from a seed string."""
    return 0x00A0 | (zlib.crc32(seed.encode("utf-8")) & 0x1F)


def build_env(name, node_id, port, channel, peers):
    """The five env vars `mesh_mcp.py` reads — same order/shape as a2a.mcp.example.json."""
    return {
        "DCF_AGENT_NAME": name,
        "DCF_AGENT_NODE_ID": f"{node_id:#06x}",
        "DCF_AGENT_UDP_PORT": str(port),
        "DCF_PEERS": peers,
        "DCF_CHANNEL": channel,
    }


# ── YAML scalar helper (quote everything; env values are strings) ───────────────
def _yq(v):
    s = str(v)
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


# ── emitters: each takes Params -> config text (str) ────────────────────────────
def _mcpservers_block(p):
    if p.transport == "http":
        server = {"command": "npx", "args": ["-y", "mcp-remote", p.url]}
    else:
        server = {"command": p.interp, "args": [p.script], "env": p.env}
    return {"mcpServers": {p.server_name: server}}


def emit_mcpservers(p):
    """Claude Code/Desktop, OpenClaw (mcporter.json), Cursor, Cline, generic."""
    return json.dumps(_mcpservers_block(p), indent=2)


def emit_opencode(p):
    """OpenCode opencode.json — `mcp` block, type local/remote, command is an array,
    `environment` not `env`."""
    if p.transport == "http":
        server = {"type": "remote", "url": p.url, "enabled": True}
    else:
        server = {"type": "local", "command": [p.interp, p.script],
                  "enabled": True, "environment": p.env}
    return json.dumps({"$schema": "https://opencode.ai/config.json",
                       "mcp": {p.server_name: server}}, indent=2)


def emit_goose_yaml(p):
    """Goose ~/.config/goose/config.yaml — `extensions:` map."""
    if p.transport == "http":
        return ("# NOTE: verify the remote extension type against your Goose version.\n"
                "extensions:\n"
                f"  {p.server_name}:\n"
                "    type: streamable_http\n"
                f"    uri: {_yq(p.url)}\n"
                "    enabled: true\n"
                "    timeout: 300\n")
    lines = ["extensions:", f"  {p.server_name}:", "    type: stdio",
             f"    cmd: {_yq(p.interp)}", "    args:", f"      - {_yq(p.script)}", "    envs:"]
    lines += [f"      {k}: {_yq(v)}" for k, v in p.env.items()]
    lines += ["    enabled: true", "    timeout: 300"]
    return "\n".join(lines) + "\n"


def emit_continue(p):
    """Continue ~/.continue/config.yaml — `mcpServers:` list (modern YAML)."""
    if p.transport == "http":
        return ("mcpServers:\n"
                f"  - name: {p.server_name}\n"
                "    type: streamable-http\n"
                f"    url: {_yq(p.url)}\n")
    lines = ["mcpServers:", f"  - name: {p.server_name}",
             f"    command: {_yq(p.interp)}", "    args:", f"      - {_yq(p.script)}", "    env:"]
    lines += [f"      {k}: {_yq(v)}" for k, v in p.env.items()]
    return "\n".join(lines) + "\n"


@dataclass
class AgentSpec:
    label: str
    emit: callable
    path: str
    transports: tuple = ("stdio", "http")


AGENTS = {
    "claude": AgentSpec("Claude Code", emit_mcpservers,
                        "~/.claude.json   (or project-scoped .mcp.json)"),
    "claude-desktop": AgentSpec("Claude Desktop", emit_mcpservers,
                        "~/.config/Claude/claude_desktop_config.json   "
                        "(macOS: ~/Library/Application Support/Claude/claude_desktop_config.json)"),
    "openclaw": AgentSpec("OpenClaw (MCPorter)", emit_mcpservers,
                        "~/.openclaw/workspace/config/mcporter.json"),
    "opencode": AgentSpec("OpenCode", emit_opencode,
                        "opencode.json   (project root; or ~/.config/opencode/opencode.json)"),
    "goose": AgentSpec("Goose", emit_goose_yaml, "~/.config/goose/config.yaml"),
    "cursor": AgentSpec("Cursor", emit_mcpservers,
                        ".cursor/mcp.json   (project; or ~/.cursor/mcp.json)"),
    "cline": AgentSpec("Cline", emit_mcpservers,
                        "~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/"
                        "settings/cline_mcp_settings.json"),
    "continue": AgentSpec("Continue", emit_continue,
                        "~/.continue/config.yaml   (or .continue/mcpServers/dcf-mesh.yaml)"),
    "generic": AgentSpec("Generic MCP client", emit_mcpservers,
                        "(paste into your client's mcpServers config)"),
}


# ── generation ──────────────────────────────────────────────────────────────────
def _params_for(spec, transport, interp, env, url):
    return Params(interp=interp, script=MESH_MCP, env=env, url=url, transport=transport)


def generate(agent_key, transport="stdio", name=None, node_id=None, port=None,
             channel=DEFAULT_CHANNEL, peers=None, http_url=None, interpreter=None,
             pair=False):
    """Return a structured result: {agent,label,path,transport,blocks:[{label,text,
    node_id,port,peers,env}],notes:[...]}.  Auto-picks distinct free ports/ids."""
    if agent_key not in AGENTS:
        raise ValueError(f"unknown agent {agent_key!r}; choose from {', '.join(AGENTS)}")
    spec = AGENTS[agent_key]
    interp = interpreter or detect_interp()
    url = http_url or DEFAULT_HTTP_URL
    notes = []
    if transport not in spec.transports:
        notes.append(f"{spec.label} may not support {transport!r}; emitting anyway.")

    blocks = []
    if pair:
        a_port, b_port = (free_ports(2) if port is None else (port, port + 1))
        a_id = node_id if node_id is not None else 0x00A1
        b_id = 0x00B2 if (node_id is None or node_id != 0x00B2) else 0x00C3
        for label, nid, lport, peer in (("agent-a", a_id, a_port, f"127.0.0.1:{b_port}"),
                                        ("agent-b", b_id, b_port, f"127.0.0.1:{a_port}")):
            env = build_env(name or label, nid, lport, channel, peer)
            text = spec.emit(_params_for(spec, transport, interp, env, url))
            blocks.append({"label": label, "text": text, "node_id": nid,
                           "port": lport, "peers": peer, "env": env})
    else:
        lport = port if port is not None else free_ports(1)[0]
        nid = node_id if node_id is not None else pick_node_id(agent_key + (name or ""))
        peer = peers if peers is not None else (os.environ.get("DCF_PEERS", "") or "127.0.0.1:7802")
        env = build_env(name or agent_key, nid, lport, channel, peer)
        text = spec.emit(_params_for(spec, transport, interp, env, url))
        blocks.append({"label": agent_key, "text": text, "node_id": nid,
                       "port": lport, "peers": peer, "env": env})

    if transport == "http":
        notes.append("HTTP/shared service: one service = ONE mesh identity (one src id + "
                     "one shared inbox). Multiple clients on it COLLIDE. For distinct agent "
                     "identities use --transport stdio (a per-agent node).")
    return {"agent": agent_key, "label": spec.label, "path": spec.path,
            "transport": transport, "blocks": blocks, "notes": notes}


def render(result):
    """Human-readable rendering of a generate() result (header + path + blocks)."""
    out = [f"# {result['label']}  ({result['transport']})",
           f"# config file: {result['path']}", ""]
    for b in result["blocks"]:
        if len(result["blocks"]) > 1:
            out.append(f"## {b['label']}  (node {b['node_id']:#06x}, udp {b['port']})")
        out.append(b["text"].rstrip("\n"))
        out.append("")
    for n in result["notes"]:
        out.append(f"# NOTE: {n}")
    return "\n".join(out).rstrip("\n") + "\n"


# ── argparse glue ───────────────────────────────────────────────────────────────
def _int0(s):
    return int(s, 0)


def add_arguments(sub):
    c = sub.add_parser("config", help="generate an MCP config block for an agent client")
    c.add_argument("--agent", required=True, choices=sorted(AGENTS),
                   help="target agent client")
    c.add_argument("--transport", choices=("stdio", "http"), default="stdio")
    c.add_argument("--name", default=None)
    c.add_argument("--node-id", type=_int0, default=None)
    c.add_argument("--port", type=int, default=None, help="local UDP port (default: auto-pick)")
    c.add_argument("--channel", default=os.environ.get("DCF_CHANNEL", DEFAULT_CHANNEL))
    c.add_argument("--peers", default=None, help="host:port,... for DCF_PEERS")
    c.add_argument("--http-url", default=None, help=f"remote URL (default {DEFAULT_HTTP_URL})")
    c.add_argument("--interpreter", default=None)
    c.add_argument("--pair", action="store_true", help="emit two cross-wired blocks")
    c.add_argument("--out", default=None, help="write to FILE (refuses to clobber)")
    c.add_argument("--force", action="store_true", help="allow --out to overwrite")
    c.set_defaults(func=cmd_config)


def cmd_config(args):
    result = generate(args.agent, transport=args.transport, name=args.name,
                      node_id=args.node_id, port=args.port, channel=args.channel,
                      peers=args.peers, http_url=args.http_url,
                      interpreter=args.interpreter, pair=args.pair)
    text = render(result)
    if args.out:
        if os.path.exists(args.out) and not args.force:
            print(f"refusing to overwrite {args.out} (pass --force)", file=sys.stderr)
            return 1
        with open(args.out, "w") as fh:
            fh.write(text)
        print(f"wrote {args.out}  (paste into {result['path']})", file=sys.stderr)
    else:
        sys.stdout.write(text)
    return 0


def main(argv=None):
    p = argparse.ArgumentParser(prog="a2a_config",
                                description="Generate MCP configs for agent clients.")
    add_arguments(p.add_subparsers(dest="cmd", required=True))
    args = p.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
