#!/usr/bin/env python3
"""a2a_listen.py — bind a DeModFrame mesh listener and print/log received text.

A standalone receiver for the agent-to-agent mesh: it binds a UDP socket on a
channel, reassembles incoming DeModFrame DATA frames, prints each message (sender
node id + source address), and optionally appends them as JSON lines to an inbox
file. Pure stdlib — no install needed.

Use it to confirm a peer's messages are actually arriving, or as the "listener"
half of the event-driven watcher pattern in AGENT_TO_AGENT.md. To reply, fire a
one-shot sender (see a2a_runner.py / the docs) at the peer's port on the same
channel.

Config via flags or env:
  --channel / DCF_CHANNEL          rendezvous channel (default "duet"; must match the peer)
  --port    / DCF_AGENT_UDP_PORT   local UDP port to bind (default 7801)
  --bind    / DCF_BIND_HOST        interface to bind (default 0.0.0.0)
  --inbox   / A2A_INBOX            if set, also append each message as a JSON line here

Run:
  python3 matrix-bridge/a2a_listen.py
  DCF_CHANNEL=duet DCF_AGENT_UDP_PORT=7801 python3 matrix-bridge/a2a_listen.py
  python3 matrix-bridge/a2a_listen.py --port 7802 --inbox /tmp/a2a_inbox.jsonl
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
import dcf_text
import superpack


def main(argv=None):
    p = argparse.ArgumentParser(
        description="Listen for DeModFrame text on a DCF mesh channel.")
    p.add_argument("--channel", default=os.environ.get("DCF_CHANNEL", "duet"))
    p.add_argument("--port", type=int,
                   default=int(os.environ.get("DCF_AGENT_UDP_PORT", "7801")))
    p.add_argument("--bind", default=os.environ.get("DCF_BIND_HOST", "0.0.0.0"))
    p.add_argument("--inbox", default=os.environ.get("A2A_INBOX"))
    args = p.parse_args(argv)

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind((args.bind, args.port))
    dst = dcf_text.channel_id(args.channel)
    rx = dcf_text.TextReassembler(accept_dst=dst)
    print(f"a2a_listen: bound {args.bind}:{args.port} channel {args.channel!r} "
          f"(dst={dst:#06x})" + (f" -> {args.inbox}" if args.inbox else "")
          + "  (Ctrl-C to stop)", flush=True)
    try:
        while True:
            data, addr = sock.recvfrom(2048)
            if superpack.is_superpack(data):
                try:
                    frames = superpack.unpack(data)
                except ValueError:
                    continue
            else:
                frames = (data,)
            for f in frames:
                for _, _pid, _ts, src, _dst, text, flags in rx.push(f):
                    print(f"  {src:#06x} @ {addr[0]}:{addr[1]}  {text}", flush=True)
                    if args.inbox:
                        with open(args.inbox, "a") as fh:
                            fh.write(json.dumps({
                                "recv_at": time.time(), "src": f"{src:#06x}",
                                "from_host": addr[0], "from_port": addr[1],
                                "flags": flags, "text": text}) + "\n")
    except KeyboardInterrupt:
        print("\na2a_listen: stopped", flush=True)
    finally:
        sock.close()


if __name__ == "__main__":
    main()
