# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
__version__ = "5.0.0"

# The gRPC client/master are optional — they need grpc + the generated stubs. The
# stdlib UDP mesh node below has no such deps, so keep these imports tolerant.
try:
    from .client import DCFClient, Mode
    from .master import DCFMaster
except ImportError:  # pragma: no cover - grpc not installed
    DCFClient = Mode = DCFMaster = None

# Stdlib UDP node + self-healing mesh runtime (the interoperable binary-ProtoMessage
# node, mirroring the Go/C/Rust nodes). No third-party dependencies.
from .proto import ProtoMessage
from .udp_node import DcfNode
from .mesh_runtime import MeshRuntime
from .wiretap import Wiretap
