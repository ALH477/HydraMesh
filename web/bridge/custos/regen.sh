#!/usr/bin/env bash
# web/bridge/custos/regen.sh -- re-emit custos.gen.c from an Exsecutor
# checkout and compare it with the committed copy (or overwrite it, --write).
#
#   EXSECUTOR=/path/to/exsecutor web/bridge/custos/regen.sh [--write]
#   EXSC=/path/to/exsc EXSECUTOR=... web/bridge/custos/regen.sh   (e.g. nix build .#exsc)
#
# $EXSECUTOR supplies the sources; the compiler is $EXSC, else $EXSECUTOR/build/exsc. The pinned commit is in
# PROVENANCE.md; a checkout at another commit may emit different bytes, and
# that difference is the point of running this.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
: "${EXSECUTOR:?set EXSECUTOR to an exsecutor checkout with build/exsc}"
exsc="${EXSC:-$EXSECUTOR/build/exsc}"
[ -x "$exsc" ] || { echo "regen: $exsc missing -- run 'make all' in $EXSECUTOR, or set EXSC" >&2; exit 2; }
[ -f "$EXSECUTOR/examples/custos/custos.exsc" ] \
  || { echo "regen: $EXSECUTOR has no examples/custos/custos.exsc (not upstream at that commit)" >&2; exit 3; }
out="$(mktemp)"; trap 'rm -f "$out"' EXIT
"$exsc" aedifica --hospes x86_64-linux --emitte c \
  "$EXSECUTOR/tests/conformance/entry23_demodframe_golden_vectors.exsc" \
  "$EXSECUTOR/tests/conformance/entry23/codex.exsc" \
  "$EXSECUTOR/examples/custos/custos.exsc" -o "$out" >/dev/null 2>&1
echo "exsecutor: $(git -C "$EXSECUTOR" rev-parse HEAD 2>/dev/null || echo '?')"
echo "sha256:    $(sha256sum "$out" | cut -d' ' -f1)  ($(wc -c <"$out") bytes)"
if [ "${1:-}" = "--write" ]; then
  cp "$out" "$here/custos.gen.c"; echo "wrote custos.gen.c -- update PROVENANCE.md"
elif cmp -s "$out" "$here/custos.gen.c"; then
  echo "custos.gen.c: byte-identical"
else
  echo "custos.gen.c: DIFFERS from what this exsc emits" >&2; exit 1
fi
