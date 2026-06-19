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
# Node-capable images and their on-wire dialect:
#   dcf-go, dcf-rs        — ProtoMessage transport (mesh with each other)
#   dcf-python, dcf-nodejs — bare DeModFrame + SuperPack (mesh with each other)
# Node-capable images and their on-wire transport:
#   dcf-go, dcf-rs, dcf-c   — ProtoMessage/UDP (mesh with each other)
#   dcf-python, dcf-nodejs  — bare DeModFrame + SuperPack/UDP (mesh with each other)
#   dcf-c                   — also a Faust-DSP modem (send-modem/recv-modem over a medium)
#   dcf-cpp                 — a gRPC node (bidi MeshStream)
#
# Builds run at low priority with capped cores (nice + --cores) so they don't
# saturate the machine. Set NICE=0 to disable.
set -euo pipefail

cd "$(dirname "$0")/.."
VERSION="${VERSION:-0.3.0}"
PUSH="${PUSH:-1}"
ORG="${ORG:-alh477}"
NICE="${NICE:-19}"
CORES="${CORES:-2}"
NIXNICE=(); [ "$NICE" != "0" ] && NIXNICE=(nice -n "$NICE")

build_one() {
  local img="$1" flake="$2"
  echo "== nix build .#${flake} (nice ${NICE}, ${CORES} cores) =="
  "${NIXNICE[@]}" nix build ".#${flake}" --cores "$CORES" --max-jobs 1
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
  dcf-go)     build_one dcf-go     docker-dcf-go ;;
  dcf-rs)     build_one dcf-rs     docker-dcf-rust ;;
  dcf-python) build_one dcf-python docker-dcf-python ;;
  dcf-nodejs) build_one dcf-nodejs docker-dcf-nodejs ;;
  dcf-c)      build_one dcf-c      docker-dcf-c ;;
  dcf-cpp)    build_one dcf-cpp    docker-dcf-cpp ;;
  all)
    build_one dcf-go     docker-dcf-go
    build_one dcf-rs     docker-dcf-rust
    build_one dcf-python docker-dcf-python
    build_one dcf-nodejs docker-dcf-nodejs
    build_one dcf-c      docker-dcf-c
    build_one dcf-cpp    docker-dcf-cpp
    ;;
  *) echo "unknown target: $target (dcf-go|dcf-rs|dcf-python|dcf-nodejs|dcf-c|dcf-cpp|all)"; exit 2 ;;
esac
echo "DONE"
