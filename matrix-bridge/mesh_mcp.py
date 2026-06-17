"""DCF mesh MCP server — lets an LLM agent participate on the DeModFrame mesh.

This is the agent's side of the setup.  An MCP-capable agent (e.g. Claude Code /
Claude Desktop) connects to this server and uses its tools to read chat that
arrived from the phone (over Matrix -> bridge -> mesh) and to reply back onto the
mesh (-> bridge -> Matrix -> phone).  The agent therefore speaks only DeModFrame,
never Matrix: the 17-byte wire quantum is the whole contract.

The same server also powers **agent-to-agent**: point two instances at each other
(set DCF_PEERS to the partner's host:port, share a DCF_CHANNEL, give each a distinct
DCF_AGENT_NODE_ID) and the two agents converse over DeModFrame with no Matrix or
human in the loop.  See matrix-bridge/AGENT_TO_AGENT.md.

Tools:
  mesh_inbox(max_messages)  drain pending inbound mesh messages (non-blocking)
  mesh_recv(timeout_s)      block until a message arrives (for agent ping-pong)
  mesh_send(text)           send text onto the mesh
  mesh_status()             node id, name, channel, peers, UDP port
  superpack_explain(a,b)    show how two frames combine into a 32-byte SuperPack

Config via env (or matrix-bridge/config.json `agent` block):
  DCF_AGENT_NODE_ID   default 0x00A6   (give each agent a distinct id)
  DCF_AGENT_NAME      default ""        (optional human label, shown in mesh_status)
  DCF_AGENT_UDP_PORT  default 7788
  DCF_CHANNEL         default "agent"   (peers must share this)
  DCF_PEERS           comma-separated host:port list of peers, e.g.
                      "127.0.0.1:7777" or "100.1.2.3:7801,100.1.2.4:7802"
  DCF_BRIDGE_HOST     default 127.0.0.1 } fallback single-peer for the Matrix
  DCF_BRIDGE_PORT     default 7777      } bridge setup (used only if DCF_PEERS unset)

Run:  python3 mesh_mcp.py            (stdio, for an MCP client)
      python3 mesh_mcp.py http       (streamable HTTP on :8765)
"""
import os
import sys

from mcp.server.fastmcp import FastMCP

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "python", "MCP"))
from dcf_node import DcfTextNode
import superpack
from wirelab_core import decode

CHANNEL = os.environ.get("DCF_CHANNEL", "agent")
NAME = os.environ.get("DCF_AGENT_NAME", "")
NODE = DcfTextNode(
    node_id=int(os.environ.get("DCF_AGENT_NODE_ID", "0x00A6"), 0),
    port=int(os.environ.get("DCF_AGENT_UDP_PORT", "7788")),
    channel=CHANNEL,
    use_superpack=True,
)


def _add_peers():
    """Register peers from DCF_PEERS ("host:port,host:port"); fall back to the
    single DCF_BRIDGE_HOST/PORT peer (the Matrix-bridge setup) when unset."""
    spec = os.environ.get("DCF_PEERS", "").strip()
    if spec:
        for i, item in enumerate(p.strip() for p in spec.split(",") if p.strip()):
            host, _, port = item.rpartition(":")
            if not host or not port.isdigit():
                raise ValueError(f"DCF_PEERS entry {item!r} must be host:port")
            NODE.add_peer(f"peer{i}", host, int(port))
    else:
        NODE.add_peer("bridge",
                      os.environ.get("DCF_BRIDGE_HOST", "127.0.0.1"),
                      int(os.environ.get("DCF_BRIDGE_PORT", "7777")))


_add_peers()
NODE.start()

mcp = FastMCP(name="DCF Mesh Agent")


def _fmt_event(ev) -> dict:
    """Render a node inbox tuple as a JSON-friendly message dict."""
    _, src, dst, text, flags, ts = ev
    return {"src": f"{src:#06x}", "channel": f"{dst:#06x}",
            "text": text, "flags": flags, "ts_us": ts}


@mcp.tool()
def mesh_inbox(max_messages: int = 10) -> list[dict]:
    """Drain up to `max_messages` messages already waiting from the mesh (from the
    human on Matrix, or a peer agent).  Non-blocking: returns [] if nothing is
    pending.  Use `mesh_recv` instead to wait for the next message."""
    out = []
    for _ in range(max(1, max_messages)):
        ev = NODE.poll()
        if ev is None:
            break
        out.append(_fmt_event(ev))
    return out


@mcp.tool()
def mesh_recv(timeout_s: float = 30.0) -> list[dict]:
    """Block up to `timeout_s` seconds for the next mesh message, then also drain
    any others already queued behind it.  Returns [] on timeout.  This is the tool
    to use for agent-to-agent ping-pong: wait for your partner's reply instead of
    busy-polling `mesh_inbox`."""
    ev = NODE.poll(timeout=max(0.0, timeout_s))
    if ev is None:
        return []
    out = [_fmt_event(ev)]
    while True:
        ev = NODE.poll()
        if ev is None:
            break
        out.append(_fmt_event(ev))
    return out


@mcp.tool()
def mesh_send(text: str) -> str:
    """Send `text` as the agent's reply onto the DeModFrame mesh (it reaches the
    phone via the bridge -> Matrix).  Returns how many UDP datagrams were sent."""
    n = NODE.send_text(text, flags=0x01)        # FLAG_AGENT
    return f"sent {n} datagram(s) as node {NODE.node_id:#06x} on channel {CHANNEL!r}"


@mcp.tool()
def mesh_status() -> dict:
    """Report the agent mesh endpoint: node id, name, channel, peers, UDP port."""
    return {
        "node_id": f"{NODE.node_id:#06x}",
        "name": NAME,
        "channel": CHANNEL,
        "channel_dst": f"{NODE.channel:#06x}",
        "udp_port": NODE.port,
        "peers": NODE.list_peers(),
        "superpack": NODE.use_superpack,
    }


@mcp.tool()
def superpack_explain(frame_a_hex: str, frame_b_hex: str) -> dict:
    """Diagnostic: combine two hex-encoded 17-byte DeModFrames into a 32-byte
    SuperPack and show the byte savings and that unpack is lossless."""
    a = bytes.fromhex(frame_a_hex)
    b = bytes.fromhex(frame_b_hex)
    sp = superpack.pack(a, b)
    ra, rb = superpack.unpack(sp)
    return {
        "raw_bytes": len(a) + len(b),
        "superpack_bytes": len(sp),
        "saved_bytes": len(a) + len(b) - len(sp),
        "superpack_hex": sp.hex(),
        "lossless": ra == a and rb == b,
        "frame_a": decode(ra),
        "frame_b": decode(rb),
    }


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "http":
        mcp.run_streamable_http(port=8765)
    else:
        mcp.run_stdio()
