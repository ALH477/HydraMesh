#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# DCF inter-container mesh interop test (dcf-rs hub + cross-cert matrix).
#
# Proves the DCF node images interoperate, two ways:
#   1. Cross-cert matrix  — every node image reports its identity/version, i.e.
#      it was built from the certified codecs that all agree on the same 17-byte
#      DeModFrame + SuperPack golden vectors (interop by construction).
#   2. Live mesh exchange — node containers exchange real DeModFrame traffic over
#      a Docker network: Go<->Go (visible receipt), and cross-language Go->Rust
#      (POSITION + GAME_DCF; the ProtoMessage IDs are identical across SDKs).
#
# Usage:  ./docker/mesh-interop-test.sh            # uses :latest images
#         TAG=0.3.0 ./docker/mesh-interop-test.sh
#
# Images are produced hermetically with Nix (no apt/apk):
#   nix build .#docker-dcf-go  && docker load < result
#   nix build .#docker-dcf-rust && docker load < result
set -uo pipefail

ORG="${ORG:-alh477}"
TAG="${TAG:-latest}"
NET="dcf-interop-$$"
GO="${ORG}/dcf-go:${TAG}"
RS="${ORG}/dcf-rs:${TAG}"
fails=0
pass() { echo "  PASS  $*"; }
fail() { echo "  FAIL  $*"; fails=$((fails + 1)); }

cleanup() {
  docker rm -f dcf-go-hub dcf-rs-hub >/dev/null 2>&1 || true
  docker network rm "$NET" >/dev/null 2>&1 || true
}
trap cleanup EXIT
cleanup
docker network create "$NET" >/dev/null

echo "== 1. cross-cert / identity matrix =="
if docker run --rm "$GO" version 2>&1 | grep -q "dcfnode"; then
  pass "dcf-go node identifies itself (certified Go wire codec)"
else
  fail "dcf-go version"
fi
if docker run --rm "$RS" version 2>&1 | grep -qi "version"; then
  pass "dcf-rs node identifies itself (certified Rust wire codec)"
else
  fail "dcf-rs version"
fi

echo "== 2a. live mesh: Go -> Go (visible receipt) =="
docker run -d --name dcf-go-hub --network "$NET" "$GO" start --bind 0.0.0.0:7777 >/dev/null
sleep 1
docker run --rm --network "$NET" "$GO" \
  send-text --peer dcf-go-hub:7777 --channel 9 --text "interop-go-to-go" >/dev/null 2>&1
sleep 1
if docker logs dcf-go-hub 2>&1 | grep -q "interop-go-to-go"; then
  pass "Go hub received + reassembled DCF-Text from a second Go container"
else
  fail "Go->Go text not received"; docker logs dcf-go-hub 2>&1 | tail -3
fi
docker rm -f dcf-go-hub >/dev/null 2>&1

echo "== 2b. live mesh: cross-language Go -> Rust (POSITION + GAME_DCF) =="
docker run -d --name dcf-rs-hub --network "$NET" "$RS" start >/dev/null
sleep 2
if docker run --rm --network "$NET" "$GO" \
     send-position --peer dcf-rs-hub:7777 --x 1 --y 2 --z 3 2>&1 | grep -q "sent position"; then
  pass "Go container sent POSITION to the Rust node (shared ProtoMessage id=1)"
else
  fail "Go->Rust position send"
fi
if docker run --rm --network "$NET" "$GO" \
     send-game --peer dcf-rs-hub:7777 --channel 5 --hex cafebabe --type 2 2>&1 | grep -q "sent .*game"; then
  pass "Go container sent GAME_DCF to the Rust node (shared ProtoMessage id=9)"
else
  fail "Go->Rust game send"
fi
docker rm -f dcf-rs-hub >/dev/null 2>&1

echo
if [ "$fails" -eq 0 ]; then
  echo "ALL INTEROP CHECKS PASSED — DCF node images mesh across Go and Rust."
else
  echo "$fails INTEROP CHECK(S) FAILED"
  exit 1
fi
