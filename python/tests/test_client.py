# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import pytest
from dcf.config import load_config

def test_load_config(tmp_path):
    config_path = tmp_path / "config.json"
    config_path.write_text('{"transport": "gRPC", "host": "localhost", "port": 50051, "mode": "auto"}')
    config = load_config(str(config_path))
    assert config["mode"] == "auto"
