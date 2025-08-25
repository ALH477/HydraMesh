# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import json
import jsonschema
import os

SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "properties": {
        "transport": {"type": "string", "enum": ["gRPC", "WebSocket", "UDP", "TCP"]},
        "host": {"type": "string"},
        "port": {"type": "integer", "minimum": 1, "maximum": 65535},
        "mode": {"type": "string", "enum": ["client", "server", "p2p", "auto", "master"]},
        "node_id": {"type": "string"},
        "peers": {"type": "array", "items": {"type": "string"}},
        "group_rtt_threshold": {"type": "integer", "minimum": 0},
        "plugins": {"type": "object", "properties": {"transport": {"type": "string"}}}
    },
    "required": ["transport", "host", "port", "mode"]
}

def load_config(path="config.json"):
    abs_path = os.path.abspath(path)
    if not os.path.exists(abs_path):
        raise FileNotFoundError(f"Config not found: {abs_path}")
    with open(abs_path, "r") as f:
        config = json.load(f)
    jsonschema.validate(config, SCHEMA)
    return config