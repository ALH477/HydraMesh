#!/usr/bin/env python3
# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-SPA knock sender — builds and sends one SPA token (spec §5/§9).

HMAC-SHA256 mode uses only the standard library. Ed25519 mode needs PyNaCl
(``pip install pynacl``); it is imported lazily so this module — and the test
suite — work without it.
"""
import hashlib
import hmac
import os
import socket
import struct
import sys
import time

MAGIC = 0x53
VERSION = 0x01
HDR_LEN = 31  # tag covers bytes [0..31)


def build_header(device_id: int, port: int, *, timestamp_ms: int | None = None,
                 nonce: bytes | None = None, flags: int = 0) -> bytes:
    """The 31-byte big-endian header (magic|ver|dev|ts|nonce|port|flags)."""
    ts_ms = timestamp_ms if timestamp_ms is not None else int(time.time() * 1000)
    if nonce is None:
        nonce = os.urandom(16)
    if len(nonce) != 16:
        raise ValueError("nonce must be 16 bytes")
    hdr = struct.pack(">BBHQ16sHB", MAGIC, VERSION, device_id, ts_ms, nonce, port, flags)
    assert len(hdr) == HDR_LEN, len(hdr)
    return hdr


def token_hmac(key: bytes, device_id: int, port: int, **hdr_kw) -> bytes:
    """A 63-byte HMAC-SHA256 token. `key` is the 32-byte per-device PSK."""
    hdr = build_header(device_id, port, **hdr_kw)
    tag = hmac.new(key, hdr, hashlib.sha256).digest()  # 32 bytes
    return hdr + tag


def token_ed25519(signing_key_hex: str, device_id: int, port: int, **hdr_kw) -> bytes:
    """A 95-byte Ed25519 token. Requires PyNaCl (imported here, not at module load)."""
    from nacl.signing import SigningKey  # lazy: keeps HMAC mode dependency-free

    sk = SigningKey(bytes.fromhex(signing_key_hex))
    hdr = build_header(device_id, port, **hdr_kw)
    sig = sk.sign(hdr).signature  # 64 bytes
    return hdr + sig


def send_token(token: bytes, host: str, knock_port: int) -> None:
    """Fire the token at host:knock_port. No reply is expected (silent authorizer)."""
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        s.sendto(token, (host, knock_port))
    finally:
        s.close()


def _main(argv: list[str]) -> int:
    # knock.py HOST KNOCK_PORT DEVICE_ID [MESH_PORT]
    # env: DCF_SPA_KEY = hex signing key (ed25519) or hex PSK (hmac);
    #      DCF_SPA_MODE = ed25519 (default) | hmac
    if len(argv) < 4:
        print("usage: knock.py HOST KNOCK_PORT DEVICE_ID [MESH_PORT]", file=sys.stderr)
        return 2
    host, knock_port, device_id = argv[1], int(argv[2]), int(argv[3])
    mesh_port = int(argv[4]) if len(argv) > 4 else 0
    key_hex = os.environ["DCF_SPA_KEY"]
    mode = os.environ.get("DCF_SPA_MODE", "ed25519")
    if mode == "hmac":
        token = token_hmac(bytes.fromhex(key_hex), device_id, mesh_port)
    else:
        token = token_ed25519(key_hex, device_id, mesh_port)
    send_token(token, host, knock_port)
    print(f"sent {len(token)}-byte token: device={device_id} -> {host}:{knock_port}")
    return 0


if __name__ == "__main__":
    raise SystemExit(_main(sys.argv))
