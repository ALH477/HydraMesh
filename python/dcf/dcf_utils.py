# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
def parse_peer(peer_str):
    parts = peer_str.split(":")
    if len(parts) != 2:
        raise ValueError(f"Invalid peer: {peer_str}")
    return parts[0], parts[1]