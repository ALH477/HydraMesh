# python/dcf/mcp_client.py
# Robustified MCP Client for DCF Python SDK: Invokes tools with retries,
# parsing, session persistence, and OAuth stub.
# Version 1.2.0 | License: GPL-3.0 | For Mono Repo SDKs

import asyncio
import logging
import time
import uuid
from typing import Any, Dict, Optional

import httpx  # For HTTP retries
from mcp import ClientSession, StdioServerParameters, types
from mcp.client.auth import OAuthClientProvider  # For OAuth stub
from mcp.client.stdio import stdio_client
from mcp.client.streamable_http import streamablehttp_client
from mcp.shared.metadata_utils import get_display_name

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)


class DcfMcpClient:
    """Robust client for invoking MCP tools on DCF servers."""

    def __init__(
        self,
        server_params: Optional[StdioServerParameters] = None,
        http_url: Optional[str] = None,
        max_retries: int = 3,
        use_oauth: bool = False,
    ) -> None:
        self.session_id: str = str(uuid.uuid4())
        self.server_params = server_params
        self.http_url = http_url
        self.max_retries = max_retries
        self.use_oauth = use_oauth  # Stub; configure OAuthProvider as needed
        logger.info(f"Initialized MCP client with session {self.session_id}")

    async def _send_with_retry(
        self, name: str, args: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """Send tool call with exponential backoff retries."""
        for attempt in range(self.max_retries):
            try:
                if self.http_url:
                    async with streamablehttp_client(self.http_url) as (read, write, _):
                        async with ClientSession(read, write) as session:
                            await session.initialize()
                            return await session.call_tool(name, args)
                else:
                    async with stdio_client(self.server_params) as (read, write):
                        async with ClientSession(read, write) as session:
                            await session.initialize()
                            return await session.call_tool(name, args)
            except (httpx.HTTPError, ValueError) as e:
                logger.exception(f"Retry {attempt + 1}/{self.max_retries} failed")
                if attempt == self.max_retries - 1:
                    return None
                await asyncio.sleep(2**attempt)  # Exponential backoff
        return None

    async def assign_role(self, peer: str, role: str) -> Optional[str]:
        """Assign role with arg validation."""
        if role not in ["p2p", "client", "server", "auto"]:
            raise ValueError("Invalid role")
        result = await self._send_with_retry("assign_role", {"peer": peer, "role": role})
        return self._parse_result(result)

    # ... (Add similar methods for other 10 tools, with validation)

    def _parse_result(self, result: Optional[Any]) -> Optional[str]:
        """Parse MCP result with type checking (e.g., TextContent)."""
        if not result:
            return None
        if hasattr(result, "content"):
            for content in result.content:
                if isinstance(content, types.TextContent):
                    return content.text
                # Add handling for ImageContent, EmbeddedResource, etc.
        return str(result)  # Fallback

    async def list_tools(self) -> List[str]:
        """List available tools with display names."""
        async with stdio_client(self.server_params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                tools_response = await session.list_tools()
                return [get_display_name(tool) for tool in tools_response.tools]


async def main() -> None:
    """Example usage with retry and parsing."""
    server_params = StdioServerParameters(
        command="uv",
        args=["run", "mcp_server.py", "stdio"],
    )
    client = DcfMcpClient(server_params=server_params, max_retries=3)
    result = await client.assign_role("peer1", "p2p")
    print(result)


if __name__ == "__main__":
    asyncio.run(main())
