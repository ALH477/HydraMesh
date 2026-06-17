"""DCF mesh HTTP/SSE endpoint resolution — a dependency-free helper.

Factored out of ``mesh_mcp.py`` so the transport (host/port/mode) resolution is
unit-testable **without** importing the ``mcp`` package (which ``mesh_mcp.py`` needs
at module load).  Pure stdlib.

Precedence (highest first):
  port  : positional ``http PORT`` / ``--port N``  >  env DCF_MCP_HTTP_PORT  >  8765
  host  : env DCF_MCP_HTTP_HOST  >  127.0.0.1   (never expose the plaintext mesh; VPN-only)
  mode  : argv[1] in {"http","sse"} else "stdio"
"""
DEFAULT_HTTP_PORT = 8765
DEFAULT_HTTP_HOST = "127.0.0.1"
HTTP_MODES = ("http", "sse")


def resolve_mode(argv):
    """Return "http", "sse", or "stdio" from argv[1]."""
    return argv[1] if len(argv) > 1 and argv[1] in HTTP_MODES else "stdio"


def resolve_http_port(argv, env):
    """Port for the streamable-HTTP / SSE transport.  Default 8765 is preserved so
    ``mesh_mcp.py http`` with no extra args is behaviour-identical to before."""
    rest = argv[2:] if (len(argv) > 1 and argv[1] in HTTP_MODES) else []
    i = 0
    while i < len(rest):
        if rest[i] == "--port" and i + 1 < len(rest):
            return int(rest[i + 1])
        if rest[i].isdigit():
            return int(rest[i])
        i += 1
    return int(env.get("DCF_MCP_HTTP_PORT", str(DEFAULT_HTTP_PORT)))


def resolve_http_host(env):
    """Bind host; defaults to loopback (the repo's VPN-only rule — never bind the
    plaintext mesh to a public interface)."""
    return env.get("DCF_MCP_HTTP_HOST", DEFAULT_HTTP_HOST)


def _selftest():
    assert resolve_mode(["mesh_mcp.py"]) == "stdio"
    assert resolve_mode(["mesh_mcp.py", "http"]) == "http"
    assert resolve_mode(["mesh_mcp.py", "sse"]) == "sse"
    # default preserved
    assert resolve_http_port(["mesh_mcp.py", "http"], {}) == 8765
    # positional + flag override env; env overrides default
    assert resolve_http_port(["mesh_mcp.py", "http", "9001"], {}) == 9001
    assert resolve_http_port(["mesh_mcp.py", "http", "--port", "9002"], {}) == 9002
    assert resolve_http_port(["mesh_mcp.py", "http"], {"DCF_MCP_HTTP_PORT": "9003"}) == 9003
    assert resolve_http_port(["mesh_mcp.py", "http", "9004"],
                             {"DCF_MCP_HTTP_PORT": "9003"}) == 9004
    # stdio mode never reads a port from argv
    assert resolve_http_port(["mesh_mcp.py"], {}) == 8765
    assert resolve_http_host({}) == "127.0.0.1"
    assert resolve_http_host({"DCF_MCP_HTTP_HOST": "100.64.0.7"}) == "100.64.0.7"
    print("a2a_endpoint selftest: CERTIFIED")


if __name__ == "__main__":
    _selftest()
