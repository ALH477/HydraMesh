#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# Build and push every DCF Docker image to Docker Hub (alh477/*), tagged :latest
# and :<VERSION>. Run from the repo root, logged in to Docker Hub (`docker login`).
#
#   ./docker/build-and-push.sh            # build + push all four
#   PUSH=0 ./docker/build-and-push.sh     # build only (no push)
#   ./docker/build-and-push.sh dcf-go     # build + push a single image
#
# Note: dcf-c / dcf-rs / hydramesh install OS packages during the build, so they
# need outbound network to the Debian/Alpine/Ubuntu mirrors. dcf-go installs none.
set -euo pipefail

cd "$(dirname "$0")/.."
VERSION="${VERSION:-0.3.0}"
PUSH="${PUSH:-1}"
ORG="${ORG:-alh477}"

build() {
  local name="$1"; shift
  echo "== build ${ORG}/${name}:{latest,${VERSION}} =="
  docker build "$@" -t "${ORG}/${name}:latest" -t "${ORG}/${name}:${VERSION}" .
  if [ "$PUSH" = "1" ]; then
    docker push "${ORG}/${name}:latest"
    docker push "${ORG}/${name}:${VERSION}"
  fi
}

# Each image and its build invocation (context = repo root; -f selects the file).
do_go()    { build dcf-go    -f go/Dockerfile; }
do_c()     { docker build -t "${ORG}/dcf-c:latest" -t "${ORG}/dcf-c:${VERSION}" C_SDK
             [ "$PUSH" = 1 ] && { docker push "${ORG}/dcf-c:latest"; docker push "${ORG}/dcf-c:${VERSION}"; }; }
do_rs()    { build dcf-rs    -f rust/Dockerfile; }
do_hydra() { build hydramesh -f Dockerfile; }

target="${1:-all}"
case "$target" in
  dcf-go)    do_go ;;
  dcf-c)     do_c ;;
  dcf-rs)    do_rs ;;
  hydramesh) do_hydra ;;
  all)       do_go; do_c; do_rs; do_hydra ;;
  *) echo "unknown target: $target (dcf-go|dcf-c|dcf-rs|hydramesh|all)"; exit 2 ;;
esac
echo "DONE"
