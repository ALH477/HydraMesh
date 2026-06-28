#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
# dcf-tools/build.sh -- build the vendored hydramodem static lib + the repo DCF
# interop / PER tools. Repo glue (DeMoD LLC, LGPL-3.0).
set -eu
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)

# Build the vendored static library via its own (pristine) Makefile.
make -C "$root" build/libhydramodem.a >/dev/null

CC=${CC:-cc}
CFLAGS=${CFLAGS:--std=gnu11 -O2 -Wall -Wextra}
out="$here/build"
mkdir -p "$out"
for t in dcf_loopback tx_campaign rx_campaign frame_tx frame_rx sense_node; do
    # shellcheck disable=SC2086
    $CC $CFLAGS "$here/$t.c" "$root/build/libhydramodem.a" -lm -o "$out/$t"
done
echo "built: $out/{dcf_loopback,tx_campaign,rx_campaign,frame_tx,frame_rx,sense_node}"
