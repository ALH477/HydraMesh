"""Non-MCP CLI bridge for the DCF agent mesh: ``a2a send`` / ``a2a recv``.

Lets Aider-style agents, shell scripts, and shell-out skills participate on the mesh
**without** speaking MCP.  Pure stdlib: ``send`` fragments over ``DcfTextNode``;
``recv`` mirrors the ``a2a_listen.py`` receive/reassembly path (and its JSONL schema).

Both are importable functions (``send`` / ``recv``) for tests, plus argparse glue so
the ``a2a.py`` umbrella and a standalone ``python3 a2a_cli.py send|recv`` both work.
"""
import argparse
import json
import os
import socket
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "python", "MCP"))
from dcf_node import DcfTextNode
import dcf_text
import superpack

FLAG_AGENT = getattr(dcf_text, "FLAG_AGENT", 0x01)
FLAG_RELIABLE = getattr(dcf_text, "FLAG_RELIABLE", 0x04)


def parse_peers(spec):
    """``"host:port,host:port"`` -> ``[(host, port), ...]``.  Empty -> the single
    DCF_BRIDGE_HOST/PORT fallback (mirrors ``mesh_mcp._add_peers``)."""
    spec = (spec or "").strip()
    out = []
    if spec:
        for item in (p.strip() for p in spec.split(",") if p.strip()):
            host, _, port = item.rpartition(":")
            if not host or not port.isdigit():
                raise ValueError(f"peer {item!r} must be host:port")
            out.append((host, int(port)))
    else:
        out.append((os.environ.get("DCF_BRIDGE_HOST", "127.0.0.1"),
                    int(os.environ.get("DCF_BRIDGE_PORT", "7777"))))
    return out


# ── send ──────────────────────────────────────────────────────────────────────
def send(text, channel="duet", peers=None, node_id=0x00A1, port=0,
         flags=FLAG_AGENT, reliable=False, json_out=False, hold=0.0):
    """Fragment ``text`` and unicast it to each peer.  Returns 0 if any datagram
    went out, else 1.  ``reliable`` sets FLAG_RELIABLE (a receiver running parent
    Phase 3 will ACK; until then it degrades to fire-and-forget)."""
    if reliable:
        flags |= FLAG_RELIABLE
    targets = parse_peers(peers if peers is not None else os.environ.get("DCF_PEERS", ""))
    node = DcfTextNode(node_id=node_id, port=port, channel=channel, use_superpack=True)
    for i, (host, p) in enumerate(targets):
        node.add_peer(f"peer{i}", host, p)
    node.start()
    try:
        n = node.send_text(text, flags=flags)
        if hold > 0:
            time.sleep(hold)        # window for a reliable ACK once Phase 3 lands
    finally:
        node.stop()
    result = {"sent_datagrams": n, "node_id": f"{node_id:#06x}", "channel": channel,
              "flags": flags, "peers": [f"{h}:{p}" for h, p in targets]}
    if json_out:
        print(json.dumps(result), flush=True)
    else:
        print(f"sent {n} datagram(s) as {node_id:#06x} on channel {channel!r} "
              f"to {', '.join(result['peers'])}", flush=True)
    return 0 if n > 0 else 1


# ── recv ──────────────────────────────────────────────────────────────────────
def recv(channel="duet", port=7801, bind="0.0.0.0", timeout=30.0, count=1,
         inbox=None, json_out=False, follow=False):
    """Bind and reassemble inbound text on ``channel``.  One-shot by default: block
    up to ``timeout`` for ``count`` messages, print them, return 0 (or 1 on timeout
    so ``send`` / ``recv`` shell pipelines and supervisors see a clean exit code).
    ``follow`` streams forever (the classic ``a2a_listen`` behaviour)."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind((bind, port))
    dst = dcf_text.channel_id(channel)
    rx = dcf_text.TextReassembler(accept_dst=dst)
    got = 0
    deadline = None if follow else time.time() + timeout
    try:
        while follow or got < count:
            if deadline is not None:
                remaining = deadline - time.time()
                if remaining <= 0:
                    break
                sock.settimeout(remaining)
            else:
                sock.settimeout(0.5)
            try:
                data, addr = sock.recvfrom(2048)
            except socket.timeout:
                if follow:
                    continue
                break
            if superpack.is_superpack(data):
                try:
                    frames = superpack.unpack(data)
                except ValueError:
                    continue
            else:
                frames = (data,)
            for f in frames:
                for _, _pid, _ts, src, _dst, text, flags in rx.push(f):
                    rec = {"src": f"{src:#06x}", "from": f"{addr[0]}:{addr[1]}",
                           "channel": channel, "flags": flags, "text": text}
                    if json_out:
                        print(json.dumps(rec), flush=True)
                    else:
                        print(f"  {src:#06x} @ {addr[0]}:{addr[1]}  {text}", flush=True)
                    if inbox:
                        with open(inbox, "a") as fh:
                            fh.write(json.dumps({"recv_at": time.time(), **rec}) + "\n")
                    got += 1
    finally:
        sock.close()
    return 0 if got > 0 else 1


# ── argparse glue (shared by a2a.py and standalone) ─────────────────────────────
def _int0(s):
    return int(s, 0)        # accept 0x00A1 and decimal


def add_arguments(sub):
    """Register the `send` and `recv` subcommands onto an argparse subparsers obj."""
    s = sub.add_parser("send", help="fragment a message onto the mesh (no MCP needed)")
    s.add_argument("text")
    s.add_argument("--channel", default=os.environ.get("DCF_CHANNEL", "duet"))
    s.add_argument("--peers", default=None, help="host:port,... (default: $DCF_PEERS)")
    s.add_argument("--node-id", type=_int0,
                   default=_int0(os.environ.get("DCF_AGENT_NODE_ID", "0x00A1")))
    s.add_argument("--port", type=int, default=0, help="local UDP port (0 = ephemeral)")
    s.add_argument("--flags", type=_int0, default=FLAG_AGENT)
    s.add_argument("--reliable", action="store_true")
    s.add_argument("--hold", type=float, default=0.0, help="seconds to wait for an ACK")
    s.add_argument("--json", action="store_true", dest="json_out")
    s.set_defaults(func=cmd_send)

    r = sub.add_parser("recv", help="receive mesh text without MCP (one-shot or --follow)")
    r.add_argument("--channel", default=os.environ.get("DCF_CHANNEL", "duet"))
    r.add_argument("--port", type=int,
                   default=int(os.environ.get("DCF_AGENT_UDP_PORT", "7801")))
    r.add_argument("--bind", default=os.environ.get("DCF_BIND_HOST", "0.0.0.0"))
    r.add_argument("--timeout", type=float, default=30.0)
    r.add_argument("--count", type=int, default=1)
    r.add_argument("--inbox", default=os.environ.get("A2A_INBOX"))
    r.add_argument("--follow", action="store_true", help="stream forever")
    r.add_argument("--json", action="store_true", dest="json_out")
    r.set_defaults(func=cmd_recv)


def cmd_send(args):
    return send(args.text, channel=args.channel, peers=args.peers, node_id=args.node_id,
                port=args.port, flags=args.flags, reliable=args.reliable,
                json_out=args.json_out, hold=args.hold)


def cmd_recv(args):
    return recv(channel=args.channel, port=args.port, bind=args.bind,
                timeout=args.timeout, count=args.count, inbox=args.inbox,
                json_out=args.json_out, follow=args.follow)


def main(argv=None):
    p = argparse.ArgumentParser(prog="a2a_cli",
                                description="Non-MCP send/recv on the DCF mesh.")
    add_arguments(p.add_subparsers(dest="cmd", required=True))
    args = p.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
