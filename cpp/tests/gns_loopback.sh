#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# Integration test for the DCF Steam-compatible (GameNetworkingSockets) transport:
# start a `serve-gns` hub, connect two clients, and assert that a certified DCF frame
# sent reliably by client A is forwarded by the hub and received by client B intact.
#
#   gns_loopback.sh /path/to/dcfcpp [port]
set -euo pipefail

BIN="${1:?usage: gns_loopback.sh <dcfcpp> [port]}"
PORT="${2:-27620}"
FRAME="d31312340001ffffdeadbeefab12cd24c0"   # certified golden frame (dst=0xFFFF)

WORK="$(mktemp -d)"
trap 'kill "${HUB:-}" 2>/dev/null || true; rm -rf "$WORK"' EXIT

"$BIN" serve-gns --port "$PORT" --node-id hub >"$WORK/hub.log" 2>&1 &
HUB=$!
sleep 0.6

"$BIN" connect-gns --peer "127.0.0.1:$PORT" --node-id B --listen 3 >"$WORK/b.log" 2>&1 &
sleep 0.6

"$BIN" connect-gns --peer "127.0.0.1:$PORT" --node-id A --send-hex "$FRAME" --reliable --listen 2 \
    >"$WORK/a.log" 2>&1
sleep 2.2

echo "--- hub ---"; cat "$WORK/hub.log"
echo "--- A ---";   cat "$WORK/a.log"
echo "--- B ---";   cat "$WORK/b.log"

if grep -qi "$FRAME" "$WORK/b.log"; then
    echo "PASS: client B received client A's frame via the hub"
    exit 0
fi
echo "FAIL: frame not forwarded to B"
exit 1
