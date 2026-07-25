# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-SPA: single-packet port authorization (client side).

Authentication only — the token proves device identity so an authorizer can
open a mesh port. No confidentiality, no key exchange (EAR99; see
Documentation/DCF_SPA_SPEC.md §3).
"""
from .knock import (
    MAGIC,
    VERSION,
    HDR_LEN,
    build_header,
    token_hmac,
    token_ed25519,
    send_token,
)

__all__ = [
    "MAGIC",
    "VERSION",
    "HDR_LEN",
    "build_header",
    "token_hmac",
    "token_ed25519",
    "send_token",
]
