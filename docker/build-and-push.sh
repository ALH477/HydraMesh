#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
#
# Build and push the DCF node Docker images to Docker Hub (alh477/*), tagged
# :latest and :<VERSION>. Built HERMETICALLY with Nix (dockerTools) — no apt/apk,
# no network to OS package mirrors needed at build time.
#
#   docker login
#   ./docker/build-and-push.sh            # build + load + push dcf-go and dcf-rs
#   PUSH=0 ./docker/build-and-push.sh     # build + load only
#   ./docker/build-and-push.sh dcf-go     # one image
#
# The images run like real nodes: their entrypoint is the node binary with
# `start` as the default command (override with any subcommand, e.g.
#   docker run --rm alh477/dcf-go send-text --peer host:7777 --text hi).
#
# Node-capable images: dcf-go (go/cmd/dcfnode), dcf-rs (rust `dcf`). The C SDK
# ships no networking (only 4 modules compile), and the C++ binding is a header-
# only codec, so there is no dcf-c / dcf-cpp *node* image to build here.
set -euo pipefail

cd "$(dirname "$0")/.."
VERSION="${VERSION:-0.3.0}"
PUSH="${PUSH:-1}"
ORG="${ORG:-alh477}"

build_one() {
  local img="$1" flake="$2"
  echo "== nix build .#${flake} =="
  nix build ".#${flake}"
  docker load < result
  docker tag "${ORG}/${img}:latest" "${ORG}/${img}:${VERSION}"
  if [ "$PUSH" = "1" ]; then
    docker push "${ORG}/${img}:latest"
    docker push "${ORG}/${img}:${VERSION}"
  fi
  rm -f result
}

target="${1:-all}"
case "$target" in
  dcf-go) build_one dcf-go docker-dcf-go ;;
  dcf-rs) build_one dcf-rs docker-dcf-rust ;;
  all)    build_one dcf-go docker-dcf-go; build_one dcf-rs docker-dcf-rust ;;
  *) echo "unknown target: $target (dcf-go|dcf-rs|all)"; exit 2 ;;
esac
echo "DONE"
