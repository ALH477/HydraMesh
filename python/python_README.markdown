# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).

# Python SDK for DeMoD Communications Framework (DCF)

**Version 5.0.0 | August 25, 2025**

## Overview
The Python SDK for DCF provides a modular, low-latency implementation for client, server, P2P, AUTO, and master modes. It uses gRPC/Protobuf for interoperability, supports self-healing P2P with RTT-based grouping, dynamic plugins, CLI, and a curses-based TUI. Designed for the mono-repo at https://github.com/ALH477/DeMoD-Communication-Framework.

## Installation
1. Clone repo: `git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git`
2. Install dependencies: `pip install -r python/requirements.txt`
3. Generate Protobuf/gRPC stubs:
   ```bash
   python -m grpc_tools.protoc -I python/dcf/proto --python_out=python/dcf --grpc_python_out=python/dcf python/dcf/proto/messages.proto python/dcf/proto/services.proto
   ```
4. Install as package: `pip install -e python/`

## Requirements
- Python 3.8+
- Dependencies: `grpcio`, `grpcio-tools`, `protobuf`, `jsonschema`, `importlib-resources`, `click`, `networkx`, `pytest`, `pytest-cov`
- Optional: `windows-curses` (Windows TUI)

## Usage
### CLI
```bash
python python/dcf.py init config.json
python python/dcf.py start --json
python python/dcf.py send "Hello" "localhost:50052" --json
python python/dcf.py health-check "localhost:50052"
python python/dcf.py assign-role "localhost:50052" p2p
python python/dcf.py tui
```

### Example: P2P Client
```python
from dcf import DCFClient
import asyncio

async def main():
    client = DCFClient("config.json")
    client.start()
    print(await client.send_message("Hello", "localhost:50052"))
    client.stop()

asyncio.run(main())
```

### Example: Master Node
```python
from dcf import DCFMaster
master = DCFMaster({"peers": ["localhost:50052"], "group_rtt_threshold": 50})
master.assign_role("localhost:50052", "p2p")
print(master.collect_metrics())
```

## Configuration
See `config.json.example`. Required fields: `transport`, `host`, `port`, `mode`.

## Testing
Run: `pytest python/tests/`

## Contributing
Fork, create a feature branch, add tests, and submit a PR. Use `black` for formatting. Ensure GPL-3.0 compliance.

## Documentation
See `docs/dcf_design_spec.md` for architecture and guidelines.