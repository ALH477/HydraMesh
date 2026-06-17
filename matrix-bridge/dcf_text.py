"""DCF text adapter — compatibility shim over the canonical reference.

The DCF-Text L2 reference now lives in ``python/MCP/textlab_core.py`` so the C and Rust
ports can certify against one source of truth via ``Documentation/text_vectors.json``
(the same pattern as audio/game).  This module re-exports it unchanged for the
matrix-bridge callers (``dcf_node``, ``a2a_listen``, ``mesh_viz``, ``bridge`` and the
tests) and the historical ``import dcf_text`` path — every previous symbol
(``packetize``, ``TextReassembler``, ``channel_id``, ``MAX_PACKET_ID``, ``BROADCAST``,
``FLAG_AGENT``, ``FLAG_MORE`` …) is preserved, with ``FLAG_RELIABLE`` and
``TextReassembler.finalize`` added.
"""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "python", "MCP"))
from textlab_core import *  # noqa: F401,F403
from textlab_core import _ceil_div, _selftest, encode, decode, crc16_ccitt  # noqa: F401


if __name__ == "__main__":
    _selftest()
