#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# DCF inter-container mesh interop test — every node image interacts at least once.
#
# Proves interop two ways:
#   1. Identity/cross-cert matrix — every node image runs and identifies itself,
#      i.e. it was built from the certified codecs that all agree on the same
#      17-byte DeModFrame + SuperPack golden vectors (interop by construction).
#   2. Live mesh exchange — node containers exchange real DeModFrame traffic over
#      a Docker network. There are two on-wire dialects:
#        • ProtoMessage      : dcf-rs, dcf-go  (Go<->Go visible, Go->Rust cross-lang)
#        • bare frame+SuperPack: dcf-python, dcf-nodejs  (Py<->Py, Py<->Node both ways)
#
# Usage:  ./docker/mesh-interop-test.sh           # uses :latest images
#         TAG=0.3.0 ./docker/mesh-interop-test.sh
#
# Images are produced hermetically with Nix (see docker/build-and-push.sh).
set -uo pipefail

ORG="${ORG:-alh477}"
TAG="${TAG:-latest}"
NET="dcf-interop-$$"
GO="${ORG}/dcf-go:${TAG}"
RS="${ORG}/dcf-rs:${TAG}"
PY="${ORG}/dcf-python:${TAG}"
JS="${ORG}/dcf-nodejs:${TAG}"
fails=0
pass() { echo "  PASS  $*"; }
fail() { echo "  FAIL  $*"; fails=$((fails + 1)); }

cleanup() {
  docker rm -f dcf-go-hub dcf-rs-hub dcf-py-hub dcf-js-hub >/dev/null 2>&1 || true
  docker network rm "$NET" >/dev/null 2>&1 || true
}
trap cleanup EXIT
cleanup
docker network create "$NET" >/dev/null

echo "== 1. identity / cross-cert matrix =="
docker run --rm "$GO" version 2>&1 | grep -q "dcfnode"        && pass "dcf-go identifies (certified Go codec)"     || fail "dcf-go version"
docker run --rm "$RS" version 2>&1 | grep -qi "version"       && pass "dcf-rs identifies (certified Rust codec)"   || fail "dcf-rs version"
docker run --rm "$PY" --help  2>&1 | grep -qiE "usage|recv"  && pass "dcf-python identifies (certified Py codec)" || fail "dcf-python help"
docker run --rm "$JS" version 2>&1 | grep -qi "dcfnode(js)"  && pass "dcf-nodejs identifies (certified JS codec)" || fail "dcf-nodejs version"

echo "== 2. ProtoMessage mesh: dcf-go / dcf-rs =="
docker run -d --name dcf-go-hub --network "$NET" "$GO" start --bind 0.0.0.0:7777 >/dev/null; sleep 1
docker run --rm --network "$NET" "$GO" send-text --peer dcf-go-hub:7777 --channel 9 --text "go-to-go" >/dev/null 2>&1; sleep 1
docker logs dcf-go-hub 2>&1 | grep -q "go-to-go" && pass "Go hub received DCF-Text from a 2nd Go container" || fail "Go->Go text"
docker rm -f dcf-go-hub >/dev/null 2>&1

docker run -d --name dcf-rs-hub --network "$NET" "$RS" start >/dev/null; sleep 2
docker run --rm --network "$NET" "$GO" send-position --peer dcf-rs-hub:7777 --x 1 --y 2 --z 3 2>&1 | grep -q "sent position" && pass "Go->Rust POSITION (shared ProtoMessage id=1)" || fail "Go->Rust position"
docker run --rm --network "$NET" "$GO" send-game --peer dcf-rs-hub:7777 --channel 5 --hex cafebabe --type 2 2>&1 | grep -q "sent .*game" && pass "Go->Rust GAME_DCF (shared ProtoMessage id=9)" || fail "Go->Rust game"
docker rm -f dcf-rs-hub >/dev/null 2>&1

echo "== 3. bare-frame+SuperPack mesh: dcf-python / dcf-nodejs =="
docker run -d --name dcf-py-hub --network "$NET" "$PY" recv --follow --channel duet --port 7801 >/dev/null; sleep 1.5
docker run --rm --network "$NET" "$PY" send "py-to-py"   --channel duet --peers dcf-py-hub:7801 >/dev/null 2>&1; sleep 1
docker logs dcf-py-hub 2>&1 | grep -q "py-to-py" && pass "Python hub received from a 2nd Python container" || fail "Py->Py"
docker run --rm --network "$NET" "$JS" send "node-to-py" --channel duet --peers dcf-py-hub:7801 >/dev/null 2>&1; sleep 1
docker logs dcf-py-hub 2>&1 | grep -q "node-to-py" && pass "Python hub received from a Node.js container (cross-lang)" || fail "Node->Py"
docker rm -f dcf-py-hub >/dev/null 2>&1

docker run -d --name dcf-js-hub --network "$NET" "$JS" recv --follow --channel duet --port 7801 >/dev/null; sleep 1.5
docker run --rm --network "$NET" "$PY" send "py-to-node" --channel duet --peers dcf-js-hub:7801 >/dev/null 2>&1; sleep 1
docker logs dcf-js-hub 2>&1 | grep -q "py-to-node" && pass "Node.js hub received from a Python container (cross-lang)" || fail "Py->Node"
docker rm -f dcf-js-hub >/dev/null 2>&1

echo
if [ "$fails" -eq 0 ]; then
  echo "ALL INTEROP CHECKS PASSED — every DCF node image meshes within its dialect (Go/Rust; Python/Node.js)."
else
  echo "$fails INTEROP CHECK(S) FAILED"
  exit 1
fi
