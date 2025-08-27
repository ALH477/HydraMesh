# python/dcf/mcp_client.py
# Refined MCP Client for DCF Python SDK: Invokes all DeMoD tools via
# official MCP SDK's ClientSession.
# Version 1.1.0 | License: GPL-3.0 | For Mono Repo SDKs

import asyncio
import logging
import uuid
from typing import Any, Dict, Optional

from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)


class DcfMcpClient:
    """Client for invoking MCP tools on DCF servers."""

    def __init__(self, server_params: StdioServerParameters) -> None:
        self.session_id: str = str(uuid.uuid4())
        self.server_params: StdioServerParameters = server_params
        logger.info(f"Initialized MCP client with session {self.session_id}")

    async def call_tool(
        self, name: str, args: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """Call a specific MCP tool with arguments.

        Args:
            name: Tool name (e.g., "assign_role").
            args: Dictionary of arguments for the tool.

        Returns:
            Result from the tool call, or None on failure.
        """
        async with stdio_client(self.server_params) as (read, write):
            async with ClientSession(read, write) as session:
                try:
                    await session.initialize()
                    result = await session.call_tool(name, args)
                    logger.info(f"Session {self.session_id}: Called {name}")
                    return result
                except ValueError:
                    logger.exception("Invalid tool call")
                    return None
                except Exception:
                    logger.exception("Unexpected MCP error")
                    return None


async def main() -> None:
    """Example usage of DcfMcpClient."""
    server_params = StdioServerParameters(
        command="uv",
        args=["run", "mcp_server.py", "stdio"],
    )
    client = DcfMcpClient(server_params)
    result = await client.call_tool(
        "assign_role", {"peer": "peer1", "role": "p2p"}
    )
    print(result)


if __name__ == "__main__":
    asyncio.run(main())
