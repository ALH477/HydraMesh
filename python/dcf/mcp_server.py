# python/dcf/mcp_server.py
# Robustified MCP Server for DCF Python SDK: Exposes tools, resources,
# prompts via FastMCP, with session persistence and HTTP support.
# Version 1.2.0 | License: GPL-3.0 | For Mono Repo SDKs

import logging
from typing import Any, Dict, Optional

from mcp.server.fastmcp import Context, FastMCP
from mcp.server.session import ServerSession

from dcf.master import (  # Use mocks if real APIs unavailable
    assign_role,
    collect_metrics,
    group_peers,
    heal_peer,
    health_check,
    list_peers,
    load_plugin,
    optimize_network,
    set_log_level,
    simulate_failure,
    update_config,
)

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)

mcp = FastMCP(name="DCF MCP Server")


# Session persistence example (store per-session data)
@mcp.tool()
def assign_role(peer: str, role: str, ctx: Optional[Context[ServerSession, None]] = None) -> str:
    """Assign a role (p2p, client, server, auto) to a peer."""
    try:
        mode_map: Dict[str, str] = {
            "p2p": "P2P_MODE",
            "client": "CLIENT_MODE",
            "server": "SERVER_MODE",
            "auto": "AUTO_MODE",
        }
        if role not in mode_map:
            raise ValueError("Invalid role")
        success = assign_role(peer, mode_map[role])
        if ctx and ctx.session:
            ctx.session.data["last_role"] = role  # Persist in session
            logger.info(f"Session {ctx.session.id}: Assigned role {role}")
        return "Success" if success else "Failed"
    except ValueError:
        logger.exception("Invalid args for assign_role")
        return "Invalid args"
    except Exception:
        logger.exception("Unexpected error in assign_role")
        return "Internal error"


# ... (Similar refinements for other 10 tools, adding ctx for session persistence where useful)


# Added resource for extensibility
@mcp.resource("network://peers/{group}")
def get_peers_in_group(group: str) -> Dict[str, Any]:
    """Get peers in a specific group (mock)."""
    try:
        return {"peers": [f"peer in {group}"]}
    except Exception:
        logger.exception("Failed to get peers")
        return {"error": "Failed"}


# Added prompt for CLI-like interactions
@mcp.prompt()
def network_status_prompt(style: str = "detailed") -> str:
    """Generate a prompt for network status."""
    styles = {
        "detailed": "Provide a detailed network status report",
        "summary": "Provide a summary of network status",
    }
    return f"{styles.get(style, styles['detailed'])} including RTT and health."


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1 and sys.argv[1] == "http":
        mcp.run_streamable_http(port=8080)  # Robust HTTP option
    else:
        mcp.run_stdio()  # Default STDIO
