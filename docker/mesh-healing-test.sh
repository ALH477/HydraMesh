#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# DCF self-healing demo — a multi-node mesh that detects failures and recovers.
# Cross-language (Go master + C auto + Go auto), over a Docker network, driven by
# the certified mesh algorithms + the DCF-Mesh control adapter (MsgMesh).
#
# Asserts:
#   1. roles converge       — auto nodes get a role + agree on master 1 (the master).
#   2. peer-failure detect   — kill node 3; a survivor marks it Unreachable.
#   3. master failover       — kill the master (node 1); a survivor re-elects.
#
# Usage:  ./docker/mesh-healing-test.sh            # uses :latest images
set -uo pipefail
ORG="${ORG:-alh477}"; TAG="${TAG:-latest}"
NET="dcf-heal-$$"
GO="$ORG/dcf-go:$TAG"; C="$ORG/dcf-c:$TAG"
fails=0
pass() { echo "  PASS  $*"; }
fail() { echo "  FAIL  $*"; fails=$((fails + 1)); }
cleanup() { docker rm -f n1 n2 n3 >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap cleanup EXIT
cleanup; docker network create "$NET" >/dev/null

# n1 = master (Go), n2 = auto (C), n3 = auto (Go). Each binds :7777 in its container.
docker run -d --name n1 --network "$NET" "$GO" start --bind 0.0.0.0:7777 --mode master --node-id 1 --peer 2@n2:7777 --peer 3@n3:7777 >/dev/null
docker run -d --name n2 --network "$NET" "$C"  start --bind 0.0.0.0:7777 --mode auto   --node-id 2 --master 1 --peer 1@n1:7777 --peer 3@n3:7777 >/dev/null
docker run -d --name n3 --network "$NET" "$GO" start --bind 0.0.0.0:7777 --mode auto   --node-id 3 --master 1 --peer 1@n1:7777 --peer 2@n2:7777 >/dev/null

echo "== 1. roles converge =="
sleep 8
if docker logs n2 2>&1 | grep -q "role assigned"; then pass "C auto node (2) received a ROLE from the Go master"; else fail "node 2 role"; fi
if docker logs n3 2>&1 | grep -qE "master=1"; then pass "Go auto node (3) agrees master=1"; else fail "node 3 master"; fi
docker logs n2 2>&1 | grep "mesh-status" | tail -1 | sed 's/^/    n2: /'

echo "== 2. peer-failure detection: kill node 3 =="
docker rm -f n3 >/dev/null 2>&1
sleep 8
if docker logs n2 2>&1 | grep -qE "3:unreachable"; then pass "node 2 marked peer 3 Unreachable (liveness FSM)"; else fail "node 2 detect peer 3"; fi
docker logs n2 2>&1 | grep "mesh-status" | tail -1 | sed 's/^/    n2: /'

echo "== 3. master failover: kill the master (node 1) =="
docker rm -f n1 >/dev/null 2>&1
sleep 9
if docker logs n2 2>&1 | grep -qE "re-election"; then pass "node 2 detected master down and re-elected (self-healing)"; else fail "node 2 failover"; fi
docker logs n2 2>&1 | grep -E "re-election|mesh-status" | tail -2 | sed 's/^/    n2: /'

echo
if [ "$fails" -eq 0 ]; then
  echo "ALL HEALING CHECKS PASSED — the mesh detects peer + master failures and recovers."
else
  echo "$fails HEALING CHECK(S) FAILED"; exit 1
fi
