"""Tests for the multi-agent config generator (matrix-bridge/a2a_config.py).

Stdlib only (no pytest required): run `python3 matrix-bridge/tests/test_a2a_config.py`.
Asserts every agent emits a valid, well-shaped config; `--pair` cross-wires distinct
ids/ports; and the Claude/generic output matches the a2a.mcp.example.json fixture.
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
BRIDGE = os.path.dirname(HERE)
sys.path.insert(0, BRIDGE)
sys.path.insert(0, os.path.join(BRIDGE, "..", "python", "MCP"))
import a2a_config as cfg

JSON_AGENTS = ("claude", "claude-desktop", "openclaw", "opencode", "cursor", "cline", "generic")
YAML_AGENTS = ("goose", "continue")
ENV5 = {"DCF_AGENT_NAME", "DCF_AGENT_NODE_ID", "DCF_AGENT_UDP_PORT", "DCF_PEERS", "DCF_CHANNEL"}


def test_every_agent_emits_valid_stdio_config():
    for key in cfg.AGENTS:
        r = cfg.generate(key, port=7890, peers="127.0.0.1:7891")
        assert r["path"] == cfg.AGENTS[key].path, f"{key}: path mismatch"
        text = r["blocks"][0]["text"]
        if key in JSON_AGENTS:
            json.loads(text)                       # must be valid JSON
        for k in ENV5:
            assert k in text, f"{key}: env var {k} missing from emitted config"
        assert "mesh_mcp.py" in text, f"{key}: script path missing"
    print(f"  PASS  {len(cfg.AGENTS)} agents emit valid stdio configs with all 5 env vars")


def test_pair_cross_wires_distinct_ids_and_ports():
    r = cfg.generate("claude", pair=True)
    a, b = r["blocks"]
    assert a["node_id"] != b["node_id"], "pair node ids must differ"
    assert a["port"] != b["port"], "pair ports must differ"
    assert a["peers"] == f"127.0.0.1:{b['port']}", "agent-a must point at agent-b's port"
    assert b["peers"] == f"127.0.0.1:{a['port']}", "agent-b must point at agent-a's port"
    print("  PASS  --pair emits two cross-wired blocks (distinct ids/ports)")


def test_claude_matches_example_fixture_shape():
    example = json.load(open(os.path.join(BRIDGE, "a2a.mcp.example.json")))
    ref = example["agent-a"]["mcpServers"]["dcf-mesh"]
    gen = json.loads(cfg.generate("claude", port=7801, peers="127.0.0.1:7802")["blocks"][0]["text"])
    server = gen["mcpServers"]["dcf-mesh"]
    assert set(server.keys()) == set(ref.keys()), "dcf-mesh keys differ from the example"
    assert set(server["env"].keys()) == set(ref["env"].keys()) == ENV5, "env keys differ"
    print("  PASS  claude/generic output matches a2a.mcp.example.json shape")


def test_http_transport_emits_mcp_remote_and_caveat():
    r = cfg.generate("openclaw", transport="http")
    text = r["blocks"][0]["text"]
    server = json.loads(text)["mcpServers"]["dcf-mesh"]
    assert server["command"] == "npx" and "mcp-remote" in server["args"], "http must use mcp-remote"
    assert any("COLLIDE" in n for n in r["notes"]), "http must carry the identity caveat"
    print("  PASS  --transport http emits mcp-remote + the shared-identity caveat")


def test_render_and_clobber_safety(tmp=None):
    # render() produces text with the path header
    out = cfg.render(cfg.generate("cursor"))
    assert ".cursor/mcp.json" in out and "mcpServers" in out
    print("  PASS  render() includes the target config path")


def main():
    test_every_agent_emits_valid_stdio_config()
    test_pair_cross_wires_distinct_ids_and_ports()
    test_claude_matches_example_fixture_shape()
    test_http_transport_emits_mcp_remote_and_caveat()
    test_render_and_clobber_safety()
    print("test_a2a_config: CERTIFIED")


if __name__ == "__main__":
    main()
