#!/usr/bin/env python3
"""a2a.py — the DCF agent-to-agent umbrella CLI (one front door for the mesh).

This file OWNS the multi-agent integration subcommands:
  config   generate an MCP config block for any agent client (a2a_config.py)
  send     fragment a message onto the mesh without MCP            (a2a_cli.py)
  recv     receive mesh text without MCP (one-shot or --follow)    (a2a_cli.py)

and thinly delegates the rest to the existing scripts:
  serve    -> mesh_mcp.py            (the MCP server; needs `mcp`)
  demo     -> a2a_runner.py --demo   (stdlib loopback)
  llm      -> a2a_runner.py --llm    (two LLMs over the mesh)
  listen   -> a2a_listen.py          (standalone listener)
  viz      -> mesh_viz.py            (live web dashboard)
  doctor / init  — placeholders filled by the parent plan's Phase 5.

Run:  python3 matrix-bridge/a2a.py <command> [args...]
"""
import argparse
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import a2a_cli
import a2a_config

HERE = os.path.dirname(os.path.abspath(__file__))


def _delegate(script, prefix, rest):
    """Run a sibling script with this interpreter, passing prefix + remaining args."""
    return subprocess.call([sys.executable, os.path.join(HERE, script)] + prefix + list(rest))


def _add_delegator(sub, name, script, help_text, prefix=None):
    sp = sub.add_parser(name, help=help_text,
                        description=f"Delegates to {script}. Args after `{name}` pass through.")
    sp.add_argument("rest", nargs=argparse.REMAINDER)
    sp.set_defaults(func=lambda a, s=script, pre=prefix or []: _delegate(s, pre, a.rest))


def _stub(name):
    def f(_args):
        print(f"`a2a {name}` is part of the parent plan's Phase 5 (front door / config "
              f"init / doctor) and is not implemented yet.", file=sys.stderr)
        return 2
    return f


def build_parser():
    p = argparse.ArgumentParser(prog="a2a",
                                description="DCF agent-to-agent mesh — one front door.")
    sub = p.add_subparsers(dest="cmd", required=True)
    a2a_config.add_arguments(sub)        # config
    a2a_cli.add_arguments(sub)           # send, recv
    _add_delegator(sub, "serve", "mesh_mcp.py", "run the MCP server (stdio / http [PORT])")
    _add_delegator(sub, "demo", "a2a_runner.py", "stdlib loopback demo", prefix=["--demo"])
    _add_delegator(sub, "llm", "a2a_runner.py", "two LLMs converse over the mesh", prefix=["--llm"])
    _add_delegator(sub, "listen", "a2a_listen.py", "standalone mesh listener")
    _add_delegator(sub, "viz", "mesh_viz.py", "live web dashboard of the mesh")
    for name in ("doctor", "init"):
        sp = sub.add_parser(name, help=f"(Phase 5) {name}")
        sp.set_defaults(func=_stub(name))
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
