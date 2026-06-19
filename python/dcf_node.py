#!/usr/bin/env python3
# SPDX-License-Identifier: LGPL-3.0-only
"""dcf_node — the stdlib UDP self-healing mesh node CLI for Python.

Mirrors ``go/cmd/dcfnode`` and the Rust ``dcf mesh`` subcommand, speaking the binary
ProtoMessage wire so it meshes with the Go/C/Rust nodes::

    python3 python/dcf_node.py start --bind 0.0.0.0:7002 --mode auto --node-id 2 \\
        --master 0 --peer 0@localhost:7000 --peer 1@localhost:7001

A passive eavesdropper for the (encryption-free) wire — demonstrates the data-leak
exposure that motivates a WireGuard tunnel (see Documentation/DCF_SECURITY_EXPOSURE.md)::

    python3 python/dcf_node.py wiretap --bind 0.0.0.0:7100 --forward localhost:7000
"""

import argparse
import logging
import sys
import time

# Allow running as a script from anywhere (python3 python/dcf_node.py ...).
sys.path.insert(0, __file__.rsplit("/", 1)[0])

from dcf.udp_node import DcfNode
from dcf.mesh_runtime import MeshRuntime
from dcf.wiretap import Wiretap


def _split_hostport(s, what):
    host, _, port = s.rpartition(":")
    if not host or not port:
        raise SystemExit("error: %s expects host:port, got %r" % (what, s))
    return host, int(port)


def _parse_peer(spec):
    if "@" not in spec:
        raise SystemExit("error: --peer expects id@host:port, got %r" % spec)
    pid, addr = spec.split("@", 1)
    host, port = _split_hostport(addr, "--peer")
    return pid, host, port


def cmd_start(args):
    host, port = _split_hostport(args.bind, "--bind")
    node = DcfNode(host=host, port=port, node_id=args.node_id)
    for spec in args.peer or []:
        node.add_peer(*_parse_peer(spec))
    if args.mode != "p2p" or args.peer:
        node.mesh = MeshRuntime(args.mode, args.node_id, args.master, args.group_threshold)
    logging.info("self-healing mesh: mode=%s node-id=%s master=%r listening on %s:%d",
                 args.mode, args.node_id, args.master, host, port)
    node.start()
    logging.info("dcf mesh node ready (Ctrl-C to stop)")
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        logging.info("shutting down")
    finally:
        node.stop()


def cmd_wiretap(args):
    bind = _split_hostport(args.bind, "--bind")
    forward = _split_hostport(args.forward, "--forward")
    tap = Wiretap(bind, forward, label="wiretap")
    logging.info("wiretap: capturing %s -> %s (plaintext DCF wire; Ctrl-C to stop)",
                 args.bind, args.forward)
    tap.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        logging.info("captured %d datagrams", len(tap.captures))
    finally:
        tap.stop()


def main(argv=None):
    p = argparse.ArgumentParser(prog="dcf_node", description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="command", required=True)

    s = sub.add_parser("start", help="run a self-healing mesh node")
    s.add_argument("--bind", default="0.0.0.0:7777", help="bind address host:port")
    s.add_argument("--mode", default="p2p", choices=["p2p", "auto", "master"])
    s.add_argument("--node-id", default="0", help="this node's numeric mesh id")
    s.add_argument("--master", default="", help="peer id to REPORT to (auto mode)")
    s.add_argument("--peer", action="append", metavar="ID@HOST:PORT",
                   help="peer (repeatable)")
    s.add_argument("--group-threshold", type=int, default=50,
                   help="RTT threshold (ms) for status-line grouping")
    s.set_defaults(func=cmd_start)

    w = sub.add_parser("wiretap", help="passively capture + forward the plaintext wire")
    w.add_argument("--bind", required=True, help="listen address host:port")
    w.add_argument("--forward", required=True, help="real destination host:port")
    w.set_defaults(func=cmd_wiretap)

    args = p.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(name)s %(message)s")
    args.func(args)


if __name__ == "__main__":
    main()
