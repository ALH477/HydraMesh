#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
# dcf-tools/duplex-test.sh -- full-duplex HydraModem stress test over two interfaces.
#
# Runs BOTH directions at once: each interface simultaneously PLAYS one campaign and
# CAPTURES the other, so both cables carry signal concurrently and each device is in
# full duplex (independent playback + capture clocks). Each direction is tagged with a
# distinct src_id, and each receiver rejects the other direction's frames as crosstalk
# -- so the test also proves the two links don't bleed into each other.
#
#   ./duplex-test.sh --dev-a plughw:3,0 --dev-b plughw:4,0 -n 300
#
# A = first interface, B = second. Cabling: A.out -> B.in AND B.out -> A.in.
# Pass: both directions PER < 1% with FEC, and 0 crosstalk frames each way.
# Repo glue (DeMoD LLC, LGPL-3.0).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)

DEVA=""; DEVB=""; N=300; FEC="--conv"; RATE=48000; KEEP=""
SRC_AB="0x00A1"; SRC_BA="0x00B2"
while [ $# -gt 0 ]; do
    case "$1" in
        --dev-a) DEVA=$2; shift 2;;
        --dev-b) DEVB=$2; shift 2;;
        -n)      N=$2; shift 2;;
        --fec)   FEC=$2; shift 2;;
        --rate)  RATE=$2; shift 2;;
        --keep)  KEEP=$2; shift 2;;
        *) echo "unknown arg: $1" >&2; exit 2;;
    esac
done
if [ -z "$DEVA" ] || [ -z "$DEVB" ]; then
    echo "usage: $0 --dev-a plughw:A,0 --dev-b plughw:B,0 [-n 300] [--fec --conv|--rep3|--none] [--keep DIR]" >&2
    echo >&2; echo "playback (aplay -l):" >&2; aplay -l 2>/dev/null | grep '^card' >&2 || true
    echo "capture (arecord -l):" >&2; arecord -l 2>/dev/null | grep '^card' >&2 || true
    exit 2
fi

"$here/build.sh"

if [ -n "$KEEP" ]; then mkdir -p "$KEEP"; work="$KEEP"; else work=$(mktemp -d); trap 'rm -rf "$work"' EXIT; fi
txAB="$work/tx_AtoB.wav"; txBA="$work/tx_BtoA.wav"
rawAB="$work/raw_AtoB.wav"; rawBA="$work/raw_BtoA.wav"   # captured at B / at A
rxAB="$work/rx_AtoB.wav";  rxBA="$work/rx_BtoA.wav"

echo "[1/4] generating two $N-frame campaigns ($FEC), distinct src tags"
"$here/build/tx_campaign" "$N" "$txAB" "$FEC" --src "$SRC_AB"
"$here/build/tx_campaign" "$N" "$txBA" "$FEC" --src "$SRC_BA"

dur=$(python3 - "$txAB" <<'PY'
import sys, wave
w = wave.open(sys.argv[1]); print(int(w.getnframes() / w.getframerate()) + 2)
PY
)

echo "[2/4] FULL DUPLEX: A=$DEVA and B=$DEVB each play+capture simultaneously (${dur}s)"
# capture A->B at B, and B->A at A
arecord -D "$DEVB" -f S32_LE -r "$RATE" -c 2 --buffer-time=1000000 -d "$((dur + 1))" "$rawAB" &
recB=$!
arecord -D "$DEVA" -f S32_LE -r "$RATE" -c 2 --buffer-time=1000000 -d "$((dur + 1))" "$rawBA" &
recA=$!
sleep 0.3
# play A->B from A, and B->A from B -- both at once
aplay -D "$DEVA" "$txAB" >/dev/null 2>&1 &
plA=$!
aplay -D "$DEVB" "$txBA" >/dev/null 2>&1 &
plB=$!
wait "$plA" "$plB"
wait "$recB" "$recA" || true

echo "[3/4] extracting live channel from each capture"
for pair in "$rawAB:$rxAB" "$rawBA:$rxBA"; do
    python3 - "${pair%%:*}" "${pair##*:}" <<'PY'
import sys, wave, numpy as np
src, dst = sys.argv[1], sys.argv[2]
w = wave.open(src); n = w.getnframes(); ch = w.getnchannels(); fr = w.getframerate()
a = np.frombuffer(w.readframes(n), dtype="<i4").reshape(-1, ch).astype(np.float64) / 2147483648.0
c = int(np.argmax([np.sqrt(np.mean(a[:, k] ** 2)) for k in range(ch)]))
x = a[:, c]
pcm = (np.clip(x, -1, 1) * 32767.0).astype("<i2")
o = wave.open(dst, "wb"); o.setnchannels(1); o.setsampwidth(2); o.setframerate(fr)
o.writeframes(pcm.tobytes()); o.close()
pk = np.max(np.abs(x)) or 1e-9
clipped = int(np.sum(np.abs(x) >= 0.999))
warn = f"  *** CLIPPING ({clipped} samples) -- lower input gain ***" if clipped > 50 else ""
print(f"  {dst.split('/')[-1]}: input{c+1} peak {20*np.log10(pk):.1f} dBFS{warn}")
PY
done

echo "[4/4] decoding both directions"
echo "--- A -> B (expect src $SRC_AB) ---"
ab=0; "$here/build/rx_campaign" "$rxAB" "$N" "$FEC" --src "$SRC_AB" || ab=$?
echo "--- B -> A (expect src $SRC_BA) ---"
ba=0; "$here/build/rx_campaign" "$rxBA" "$N" "$FEC" --src "$SRC_BA" || ba=$?

echo
if [ "$ab" -eq 0 ] && [ "$ba" -eq 0 ]; then
    echo "DUPLEX PASS: both directions PER < 1% under simultaneous bidirectional load"
else
    echo "DUPLEX CHECK: at least one direction PER >= 1% (see above)"
fi
[ -n "$KEEP" ] && echo "artifacts kept in $KEEP"
exit $(( ab | ba ))
