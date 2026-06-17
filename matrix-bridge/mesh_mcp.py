"""DCF mesh MCP server — lets an LLM agent participate on the DeModFrame mesh.

This is the agent's side of the setup.  An MCP-capable agent (e.g. Claude Code /
Claude Desktop) connects to this server and uses its tools to read chat that
arrived from the phone (over Matrix -> bridge -> mesh) and to reply back onto the
mesh (-> bridge -> Matrix -> phone).  The agent therefore speaks only DeModFrame,
never Matrix: the 17-byte wire quantum is the whole contract.

Tools:
  mesh_inbox(max_messages)  drain pending inbound mesh messages (from the human)
  mesh_send(text)           send a reply onto the mesh
  mesh_status()             node id, channel, peers, UDP port
  superpack_explain(a,b)    show how two frames combine into a 32-byte SuperPack

Config via env (or matrix-bridge/config.json `agent` block):
  DCF_AGENT_NODE_ID   default 0x00A6
  DCF_AGENT_UDP_PORT  default 7788
  DCF_CHANNEL         default "agent"
  DCF_BRIDGE_HOST     default 127.0.0.1
  DCF_BRIDGE_PORT     default 7777   (the bridge node's udp_port)

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
NODE = DcfTextNode(
    node_id=int(os.environ.get("DCF_AGENT_NODE_ID", "0x00A6"), 0),
    port=int(os.environ.get("DCF_AGENT_UDP_PORT", "7788")),
    channel=CHANNEL,
    use_superpack=True,
)
NODE.add_peer("bridge",
              os.environ.get("DCF_BRIDGE_HOST", "127.0.0.1"),
              int(os.environ.get("DCF_BRIDGE_PORT", "7777")))
NODE.start()

mcp = FastMCP(name="DCF Mesh Agent")


@mcp.tool()
def mesh_inbox(max_messages: int = 10) -> list[dict]:
    """Drain up to `max_messages` chat messages that arrived from the mesh
    (i.e. from the human on Matrix).  Returns [] when nothing is pending."""
    out = []
    for _ in range(max(1, max_messages)):
        ev = NODE.poll()
        if ev is None:
            break
        _, src, dst, text, flags, ts = ev
        out.append({"src": f"{src:#06x}", "channel": f"{dst:#06x}",
                    "text": text, "flags": flags, "ts_us": ts})
    return out


@mcp.tool()
def mesh_send(text: str) -> str:
    """Send `text` as the agent's reply onto the DeModFrame mesh (it reaches the
    phone via the bridge -> Matrix).  Returns how many UDP datagrams were sent."""
    n = NODE.send_text(text, flags=0x01)        # FLAG_AGENT
    return f"sent {n} datagram(s) as node {NODE.node_id:#06x} on channel {CHANNEL!r}"


@mcp.tool()
def mesh_status() -> dict:
    """Report the agent mesh endpoint: node id, channel, peers, UDP port."""
    return {
        "node_id": f"{NODE.node_id:#06x}",
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
