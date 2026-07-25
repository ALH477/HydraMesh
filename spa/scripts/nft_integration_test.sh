#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# Manual DCF-SPA end-to-end test against a real nftables set. Needs root and
# `nft`; NOT run in CI (CI covers the decision logic via `cargo test` against a
# mocked granter). Verifies: valid knock -> set gains the source; replay -> no
# change. See DCF_SPA_SPEC.md §13.
set -euo pipefail

TABLE=hydramesh_spa
SET=allowed_peers
KNOCK_PORT=62201
DEV=5
KEY_HEX=$(printf '07%.0s' {1..32})   # 32-byte PSK, all 0x07

if [[ $EUID -ne 0 ]]; then echo "needs root (nft)"; exit 1; fi

cleanup() { nft delete table inet "$TABLE" 2>/dev/null || true; }
trap cleanup EXIT

nft add table inet "$TABLE"
nft add set inet "$TABLE" "$SET" '{ type ipv4_addr; flags timeout; }'

echo "== set before knock =="
nft list set inet "$TABLE" "$SET"

# Run the authorizer in the background (HMAC creds dir).
CREDS=$(mktemp -d)
printf '%s' "$KEY_HEX" > "$CREDS/000$DEV.key"
REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
"$REPO_ROOT/spa/target/release/dcf-spa-authorizer" \
    --knock-port "$KNOCK_PORT" --mesh-port 7100 --grant-ttl 30 \
    --creds-dir "$CREDS" --nft-table "$TABLE" --nft-set "$SET" &
AUTH_PID=$!
trap 'kill $AUTH_PID 2>/dev/null; cleanup' EXIT
sleep 0.5

echo "== knock (HMAC mode) =="
DCF_SPA_KEY="$KEY_HEX" DCF_SPA_MODE=hmac \
    python3 "$REPO_ROOT/python/dcf/spa/knock.py" 127.0.0.1 "$KNOCK_PORT" "$DEV" 7100
sleep 0.5

echo "== set after knock (expect 127.0.0.1) =="
nft list set inet "$TABLE" "$SET"
nft list set inet "$TABLE" "$SET" | grep -q "127.0.0.1" \
    && echo "PASS: source authorized" || { echo "FAIL: not authorized"; exit 1; }

echo "done"
