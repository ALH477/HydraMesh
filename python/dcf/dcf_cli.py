# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import click
import json
import asyncio
from .client import DCFClient
from .master import DCFMaster

@click.group()
def cli():
    pass

@cli.command()
@click.argument("config_path", default="config.json")
@click.option("--json", is_flag=True)
def init(config_path, json_flag):
    DCFClient(config_path)
    output = {"status": "initialized"}
    print(json.dumps(output) if json_flag else "Initialized")

@cli.command()
@click.option("--json", is_flag=True)
def start(json_flag):
    client = DCFClient()
    client.start()
    output = {"status": "started", "mode": client.mode.value}
    print(json.dumps(output) if json_flag else f"Started in {client.mode.value} mode")

@cli.command()
@click.argument("data")
@click.argument("recipient")
@click.option("--json", is_flag=True)
def send(data, recipient, json_flag):
    client = DCFClient()
    loop = asyncio.get_event_loop()
    resp = loop.run_until_complete(client.send_message(data, recipient))
    output = {"response": resp}
    print(json.dumps(output) if json_flag else resp)

@cli.command()
@click.argument("peer")
def health_check(peer):
    client = DCFClient()
    loop = asyncio.get_event_loop()
    resp = loop.run_until_complete(client.net.health_check(peer))
    print(f"Healthy: {resp.healthy}")

@cli.command()
@click.argument("peer")
@click.argument("role")
def assign_role(peer, role):
    master = DCFMaster(DCFClient().config)
    master.assign_role(peer, role)
    print(f"Assigned {role} to {peer}")

@cli.command()
def tui():
    from .tui import run_tui
    run_tui()

if __name__ == "__main__":
    cli()