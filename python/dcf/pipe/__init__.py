# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Pipe runtime — lossless bulk transfer driven by DCF control frames.

The control-message codec is the certified contract (python/MCP/pipelab_core.py);
this package is the reference runtime that drives it: a receiver-driven credit
sender/receiver pair with FEC-wrapped chunks (forward loss recovery) and
NACK-driven retransmit (the fallback past the FEC budget). See
Documentation/DCF_PIPE_SPEC.md.
"""
from .protocol import PipeSender, PipeReceiver, run_transfer, profile, PROFILES
from .invariant import deficit, check_transfer, PipeInvariantMonitor

__all__ = [
    "PipeSender", "PipeReceiver", "run_transfer", "profile", "PROFILES",
    "deficit", "check_transfer", "PipeInvariantMonitor",
]
