#!/usr/bin/env python3
"""Agent-to-agent over DCF — drive two endpoints talking across the DeModFrame mesh.

This is the self-driving counterpart to running two ``mesh_mcp.py`` servers by
hand (see AGENT_TO_AGENT.md): it wires two ``DcfTextNode`` UDP endpoints on one
host, peers them to each other on a shared channel, and runs a conversation over
the 17-byte wire quantum — no Matrix, no human, no MCP client.

Two modes:

  --demo  (default, stdlib only)  two in-process nodes ping-pong canned lines;
          asserts each side reassembles the other's multi-fragment messages and
          prints ``a2a demo: CERTIFIED``.  Doubles as a smoke test of the whole
          A2A path; needs no API key.

  --llm   each side is an LLM (Anthropic API, default model claude-opus-4-8).
          One agent is the initiator; thereafter both loop recv -> think -> send,
          every message crossing the mesh as DeModFrame DATA frames.  Gated on
          ANTHROPIC_API_KEY.

Run:
  python3 a2a_runner.py --demo
  python3 a2a_runner.py --demo --turns 6 --channel duet
  ANTHROPIC_API_KEY=... python3 a2a_runner.py --llm --turns 4 \
      --topic "the cleanest way to test a UDP protocol"
"""
import argparse
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from dcf_node import DcfTextNode


def _make_pair(channel, superpack=True):
    """Two DcfTextNodes on localhost, peered to each other on `channel`."""
    a = DcfTextNode(node_id=0x00A1, port=0, channel=channel, use_superpack=superpack)
    b = DcfTextNode(node_id=0x00B2, port=0, channel=channel, use_superpack=superpack)
    a.add_peer("b", "127.0.0.1", b.port)
    b.add_peer("a", "127.0.0.1", a.port)
    a.start()
    b.start()
    return a, b


def _recv(node, timeout=5.0):
    """Block for one inbound message; return (src, text) or None on timeout."""
    ev = node.poll(timeout=timeout)
    if ev is None:
        return None
    _, src, _dst, text, _flags, _ts = ev
    return src, text


# ── demo mode (stdlib, no API key) ────────────────────────────────────────────
def run_demo(channel, turns):
    a, b = _make_pair(channel)
    try:
        # A multi-fragment opener so reassembly is genuinely exercised each way.
        line = "ping/pong over DeModFrame — fragment me \U0001f680 " * 3
        sender, receiver, label = a, b, "A->B"
        for i in range(turns):
            msg = f"[turn {i}] {line}"
            sender.send_text(msg, to=("b" if sender is a else "a"))
            got = _recv(receiver, timeout=3.0)
            assert got is not None, f"{label}: no message received on turn {i}"
            src, text = got
            assert text == msg, f"{label}: payload mismatch on turn {i}"
            expect_src = 0x00A1 if sender is a else 0x00B2
            assert src == expect_src, f"{label}: src {src:#06x} != {expect_src:#06x}"
            print(f"  {label}  src={src:#06x}  {text[:48]}...")
            # swap roles for the next turn
            if sender is a:
                sender, receiver, label = b, a, "B->A"
            else:
                sender, receiver, label = a, b, "A->B"
        print("a2a demo: CERTIFIED "
              f"({turns} turns over UDP, channel {channel!r}, SuperPack on)")
    finally:
        a.stop()
        b.stop()


# ── llm mode (Anthropic API) ──────────────────────────────────────────────────
SYSTEM_A = ("You are Ada, an engineer on a mesh-protocol team. You are chatting "
            "with a peer agent over a DCF mesh link. Keep each turn under 60 "
            "words, be concrete, and end with a question that moves the "
            "discussion forward.")
SYSTEM_B = ("You are Boole, a skeptical systems reviewer chatting with a peer "
            "agent over a DCF mesh link. Keep each turn under 60 words, "
            "challenge assumptions, and propose a concrete next step.")


def _text_of(resp):
    return "".join(b.text for b in resp.content if b.type == "text").strip()


def run_llm(channel, turns, model, topic, max_tokens):
    try:
        import anthropic
    except ImportError:
        sys.exit("--llm needs the anthropic SDK: pip install anthropic")
    if not os.environ.get("ANTHROPIC_API_KEY"):
        sys.exit("--llm needs ANTHROPIC_API_KEY in the environment")

    client = anthropic.Anthropic()
    a, b = _make_pair(channel)
    # Per-agent conversation history (each sees its own turns as assistant).
    hist_a, hist_b = [], []

    def speak(system, history, incoming):
        if incoming is not None:
            history.append({"role": "user", "content": incoming})
        resp = client.messages.create(model=model, max_tokens=max_tokens,
                                       system=system, messages=history)
        out = _text_of(resp)
        history.append({"role": "assistant", "content": out})
        return out

    try:
        # Ada opens; the kickoff topic is her first "user" prompt.
        opener = speak(SYSTEM_A, hist_a, f"Open the conversation about {topic}.")
        print(f"\nAda (0x00A1): {opener}\n")
        a.send_text(opener, to="b")

        # Alternate Boole/Ada for `turns` exchanges, carrying text over the mesh.
        for i in range(turns):
            got = _recv(b, timeout=120.0)
            if got is None:
                print("Boole: (mesh timeout — fragment dropped?)")
                break
            reply = speak(SYSTEM_B, hist_b, got[1])
            print(f"Boole (0x00B2): {reply}\n")
            b.send_text(reply, to="a")

            got = _recv(a, timeout=120.0)
            if got is None:
                print("Ada: (mesh timeout — fragment dropped?)")
                break
            reply = speak(SYSTEM_A, hist_a, got[1])
            print(f"Ada (0x00A1): {reply}\n")
            a.send_text(reply, to="b")
        print(f"a2a llm: done ({model}, channel {channel!r})")
    finally:
        a.stop()
        b.stop()


def main(argv=None):
    p = argparse.ArgumentParser(description="Two agents talking over the DCF mesh.")
    mode = p.add_mutually_exclusive_group()
    mode.add_argument("--demo", action="store_true",
                      help="stdlib loopback ping-pong (default)")
    mode.add_argument("--llm", action="store_true",
                      help="two Anthropic LLMs converse over the mesh")
    p.add_argument("--turns", type=int, default=4, help="exchanges to run")
    p.add_argument("--channel", default="duet", help="shared rendezvous channel")
    p.add_argument("--model", default="claude-opus-4-8", help="LLM model id")
    p.add_argument("--topic", default="how to certify a wire protocol across languages",
                   help="opening topic for --llm")
    p.add_argument("--max-tokens", type=int, default=400, help="LLM max_tokens/turn")
    args = p.parse_args(argv)

    if args.llm:
        run_llm(args.channel, args.turns, args.model, args.topic, args.max_tokens)
    else:
        run_demo(args.channel, args.turns)


if __name__ == "__main__":
    main()
