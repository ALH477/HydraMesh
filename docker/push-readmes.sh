#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# Push the per-image overviews in docker/hub/*.md to the Docker Hub repo pages.
# Needs YOUR Docker Hub credential (a Personal Access Token with read/write), which
# is why this isn't run by the agent. Two ways:
#
#   A) docker-pushrm plugin (one-time install), then:  ./docker/push-readmes.sh pushrm
#   B) the Docker Hub API with a PAT (no extra binary): ./docker/push-readmes.sh api
#
set -euo pipefail
cd "$(dirname "$0")/.."
ORG="${ORG:-alh477}"
IMAGES=(dcf-go dcf-rs dcf-c dcf-cpp dcf-python dcf-nodejs)

short() { case "$1" in
  dcf-go)     echo "DCF mesh node (Go) — certified DeModFrame over ProtoMessage/UDP" ;;
  dcf-rs)     echo "DCF mesh node (Rust) — certified DeModFrame over ProtoMessage/UDP + gRPC mgmt" ;;
  dcf-c)      echo "DCF mesh node (C) — DeModFrame/UDP + a Faust-DSP modem (FSK/OOK/PSK/QAM)" ;;
  dcf-cpp)    echo "DCF gRPC node (C++) — DeModFrame/SuperPack over a bidirectional MeshStream" ;;
  dcf-python) echo "DCF mesh node (Python) — DeModFrame + SuperPack text endpoint" ;;
  dcf-nodejs) echo "DCF mesh node (Node.js) — DeModFrame + SuperPack over UDP (dgram)" ;;
esac; }

method="${1:-api}"

case "$method" in
  pushrm)
    # Requires: ~/.docker/cli-plugins/docker-pushrm  (https://github.com/christian-korneck/docker-pushrm)
    # and a logged-in Hub session (a PAT in DOCKER_API_TOKEN works best).
    for r in "${IMAGES[@]}"; do
      echo "== $ORG/$r =="
      docker pushrm "$ORG/$r" -f "docker/hub/$r.md" -s "$(short "$r")"
    done ;;

  api)
    # Asks for a Docker Hub PAT (read/write), exchanges it for a JWT, PATCHes each repo.
    read -rsp "Docker Hub username [$ORG]: " U; U="${U:-$ORG}"; echo
    read -rsp "Docker Hub PAT (read/write): " PAT; echo
    JWT="$(curl -fsS -H 'Content-Type: application/json' \
      -d "{\"username\":\"$U\",\"password\":\"$PAT\"}" \
      https://hub.docker.com/v2/users/login/ | python3 -c 'import sys,json;print(json.load(sys.stdin)["token"])')"
    for r in "${IMAGES[@]}"; do
      body="$(python3 -c "import json,sys;print(json.dumps({'full_description':open('docker/hub/$r.md').read(),'description':sys.argv[1]}))" "$(short "$r")")"
      code="$(curl -s -o /dev/null -w '%{http_code}' -X PATCH \
        -H "Authorization: JWT $JWT" -H 'Content-Type: application/json' \
        -d "$body" "https://hub.docker.com/v2/repositories/$ORG/$r/")"
      echo "$ORG/$r -> HTTP $code"
    done ;;

  *) echo "usage: $0 [api|pushrm]"; exit 2 ;;
esac
echo "DONE"
