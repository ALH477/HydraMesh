#!/usr/bin/env python3
"""Interactive agent-to-agent test harness — guided setup, then run + PASS/FAIL.

A friendly front-end to a2a_runner.py: it walks you through a short setup phase
(mode, topology, channel, turns, …), echoes a review, then runs two endpoints
talking over the DeModFrame mesh and reports the result.

Named a2a_interactive.py (not *_test.py) on purpose: pytest must not auto-collect
an input()-driven script. The automated unit test stays tests/test_a2a.py.

Modes:    demo  (stdlib ping-pong, no API key)   |  llm  (two Anthropic models)
Topology: local (both nodes in this process)     |  remote (peer on another host/VPN)

Run:  python3 a2a_interactive.py        # answer the prompts (Enter = default)
      printf '' | python3 a2a_interactive.py   # non-tty: all defaults, local demo
"""
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import a2a_runner
from dcf_node import DcfTextNode


# ── setup phase ───────────────────────────────────────────────────────────────
def prompt(label, default):
    """Ask one question showing its default. Empty input / EOF / non-tty -> default."""
    suffix = f" [{default}]" if default != "" else ""
    try:
        ans = input(f"  {label}{suffix}: ").strip()
    except EOFError:
        print(f"  {label}{suffix}:  (no input — using default)")
        return default
    return ans or default


def choose(label, options, default):
    """Prompt until the answer is one of `options` (case-insensitive)."""
    opts = "/".join(options)
    while True:
        ans = prompt(f"{label} ({opts})", default).lower()
        if ans in options:
            return ans
        print(f"    please answer one of: {opts}")


def _int(label, default):
    while True:
        raw = prompt(label, str(default))
        try:
            return int(raw)
        except ValueError:
            print("    please enter a whole number")


def setup():
    """Run the interactive setup phase; return a config dict."""
    print("\n=== DCF agent-to-agent — setup ===")
    cfg = {}

    cfg["mode"] = choose("Mode: demo (no key) or live LLMs", ["demo", "llm"], "demo")
    if cfg["mode"] == "llm" and not os.environ.get("ANTHROPIC_API_KEY"):
        print("  ! ANTHROPIC_API_KEY is not set — falling back to demo mode.")
        cfg["mode"] = "demo"

    cfg["topology"] = choose("Topology: local (both here) or remote (peer elsewhere)",
                             ["local", "remote"], "local")
    cfg["channel"] = prompt("Rendezvous channel (both sides must match)", "duet")
    cfg["turns"] = _int("Turns (exchanges to run)", 4)

    if cfg["mode"] == "llm":
        cfg["model"] = prompt("LLM model id", "claude-opus-4-8")
        cfg["topic"] = prompt("Opening topic",
                              "how to certify a wire protocol across languages")
        cfg["max_tokens"] = _int("max_tokens per turn", 400)

    if cfg["topology"] == "remote":
        cfg["role"] = choose("This node's role: initiator (opens) or responder (replies)",
                             ["initiator", "responder"], "initiator")
        default_local = 7801 if cfg["role"] == "initiator" else 7802
        default_peer = 7802 if cfg["role"] == "initiator" else 7801
        default_id = "0x00A1" if cfg["role"] == "initiator" else "0x00B2"
        cfg["local_port"] = _int("Local UDP port to bind", default_local)
        cfg["peer_host"] = prompt("Peer host (IP/hostname; VPN IP for two machines)",
                                  "127.0.0.1")
        cfg["peer_port"] = _int("Peer UDP port", default_peer)
        cfg["node_id"] = int(prompt("Local node id", default_id), 0)

    return cfg


def review(cfg):
    """Echo the chosen config and gate on Enter (Ctrl-C aborts)."""
    print("\n=== review ===")
    for k in ("mode", "topology", "channel", "turns", "model", "topic", "max_tokens",
              "role", "local_port", "peer_host", "peer_port"):
        if k in cfg:
            v = f"{cfg[k]:#06x}" if k == "node_id" else cfg[k]
            print(f"  {k:11} {v}")
    if "node_id" in cfg:
        print(f"  {'node_id':11} {cfg['node_id']:#06x}")
    try:
        input("\nPress Enter to start (Ctrl-C to abort)... ")
    except EOFError:
        print("(no tty — starting)")


# ── run phase ─────────────────────────────────────────────────────────────────
def run_local(cfg):
    """Reuse a2a_runner for the in-process pair; treat a clean return as PASS."""
    if cfg["mode"] == "demo":
        a2a_runner.run_demo(cfg["channel"], cfg["turns"])
    else:
        a2a_runner.run_llm(cfg["channel"], cfg["turns"], cfg["model"],
                           cfg["topic"], cfg["max_tokens"])
    return True


def _one_node(cfg):
    node = DcfTextNode(node_id=cfg["node_id"], port=cfg["local_port"],
                       channel=cfg["channel"], use_superpack=True)
    node.add_peer("peer", cfg["peer_host"], cfg["peer_port"])
    node.start()
    print(f"  bound :{node.port}  peer {cfg['peer_host']}:{cfg['peer_port']}  "
          f"channel {cfg['channel']!r}  id {node.node_id:#06x}")
    return node


def run_remote_demo(cfg):
    """One local node + a remote peer; echo round-trip is the pass criterion."""
    node = _one_node(cfg)
    ok = True
    try:
        if cfg["role"] == "initiator":
            line = "ping over DeModFrame — fragment me \U0001f680 " * 3
            for i in range(cfg["turns"]):
                msg = f"[turn {i}] {line}"
                node.send_text(msg, to="peer")
                ev = node.poll(timeout=10.0)
                if ev is None or ev[3] != msg:
                    print(f"  turn {i}: FAIL (no/!= echo back from peer)")
                    ok = False
                    break
                print(f"  turn {i}: round-trip OK (src {ev[1]:#06x})")
        else:  # responder: echo whatever arrives, back to the peer
            print("  responder: waiting for pings to echo (Ctrl-C to stop)...")
            for _ in range(cfg["turns"]):
                ev = node.poll(timeout=30.0)
                if ev is None:
                    print("  responder: idle timeout")
                    break
                node.send_text(ev[3], to="peer")
                print(f"  echoed {len(ev[3])}B back to peer (src {ev[1]:#06x})")
    finally:
        node.stop()
    return ok


def run_remote_llm(cfg):
    """One local LLM persona talking to whatever is on the other end of the mesh."""
    try:
        import anthropic
    except ImportError:
        print("  FAIL: --llm needs the anthropic SDK (pip install anthropic)")
        return False
    client = anthropic.Anthropic()
    system = a2a_runner.SYSTEM_A if cfg["role"] == "initiator" else a2a_runner.SYSTEM_B
    node = _one_node(cfg)
    history = []

    def speak(incoming):
        if incoming is not None:
            history.append({"role": "user", "content": incoming})
        resp = client.messages.create(model=cfg["model"], max_tokens=cfg["max_tokens"],
                                      system=system, messages=history)
        out = a2a_runner._text_of(resp)
        history.append({"role": "assistant", "content": out})
        return out

    try:
        if cfg["role"] == "initiator":
            opener = speak(f"Open the conversation about {cfg['topic']}.")
            print(f"\n  me: {opener}\n")
            node.send_text(opener, to="peer")
        for _ in range(cfg["turns"]):
            ev = node.poll(timeout=120.0)
            if ev is None:
                print("  (mesh timeout — partner silent or fragment dropped)")
                break
            reply = speak(ev[3])
            print(f"\n  me: {reply}\n")
            node.send_text(reply, to="peer")
    finally:
        node.stop()
    return True


def run(cfg):
    if cfg["topology"] == "local":
        return run_local(cfg)
    if cfg["mode"] == "demo":
        return run_remote_demo(cfg)
    return run_remote_llm(cfg)


def main():
    cfg = setup()
    review(cfg)
    print("\n=== run ===")
    try:
        ok = run(cfg)
    except KeyboardInterrupt:
        print("\nFAIL: aborted")
        return 130
    except AssertionError as e:
        print(f"\nFAIL: {e}")
        return 1
    print("\nPASS" if ok else "\nFAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print("\naborted")
        sys.exit(130)
