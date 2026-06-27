#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
# dcf-tools/field-test.sh -- HydraModem cabled-link PER test over two ALSA devices.
#
# Generates an N-frame DeModFrame campaign WAV, plays it out --tx-dev while
# capturing on --rx-dev, then decodes the capture and reports PER. Run once per
# direction (this is the DCF_FIELD_USE.md "T2 wired coupling" tier; pass = PER<1%
# with FEC). Repo glue (DeMoD LLC, LGPL-3.0).
#
#   ./field-test.sh --tx-dev hw:1,0 --rx-dev hw:2,0 -n 200
#   ./field-test.sh --tx-dev hw:2,0 --rx-dev hw:1,0 -n 200   # reverse direction
#
# If a device rejects 48 kHz / S16_LE, use the ALSA plug layer: plughw:X,0.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)

TXDEV=""; RXDEV=""; N=200; FEC="--conv"; RATE=48000; KEEP=""
while [ $# -gt 0 ]; do
    case "$1" in
        --tx-dev) TXDEV=$2; shift 2;;
        --rx-dev) RXDEV=$2; shift 2;;
        -n)       N=$2; shift 2;;
        --fec)    FEC=$2; shift 2;;
        --rate)   RATE=$2; shift 2;;
        --keep)   KEEP=$2; shift 2;;   # dir to save tx.wav/rx.wav artifacts
        *) echo "unknown arg: $1" >&2; exit 2;;
    esac
done
if [ -z "$TXDEV" ] || [ -z "$RXDEV" ]; then
    echo "usage: $0 --tx-dev hw:A,0 --rx-dev hw:B,0 [-n 200] [--fec --conv|--rep3|--none] [--rate 48000] [--keep DIR]" >&2
    echo >&2
    echo "playback devices (aplay -l):" >&2; aplay -l 2>/dev/null | grep '^card' >&2 || true
    echo "capture devices (arecord -l):" >&2; arecord -l 2>/dev/null | grep '^card' >&2 || true
    exit 2
fi

"$here/build.sh"

if [ -n "$KEEP" ]; then mkdir -p "$KEEP"; work="$KEEP"; else work=$(mktemp -d); trap 'rm -rf "$work"' EXIT; fi
tx="$work/tx.wav"; rx="$work/rx.wav"

echo "[1/3] generating $N-frame campaign ($FEC) -> $tx"
"$here/build/tx_campaign" "$N" "$tx" "$FEC"

dur=$(python3 - "$tx" <<'PY'
import sys, wave
w = wave.open(sys.argv[1]); print(int(w.getnframes() / w.getframerate()) + 2)
PY
)

echo "[2/3] capturing on $RXDEV ($((dur+1))s) while playing on $TXDEV"
raw="$work/rx_raw.wav"
# Capture full-resolution S32 stereo; we extract the live channel below at full
# scale (avoids the 6 dB loss + averaging of a mono -c 1 plug downmix).
arecord -D "$RXDEV" -f S32_LE -r "$RATE" -c 2 --buffer-time=1000000 -d "$((dur + 1))" "$raw" &
recpid=$!
sleep 0.3
aplay -D "$TXDEV" "$tx"
wait "$recpid" || true

# Extract the louder input channel -> 16-bit mono (what rx_campaign reads).
python3 - "$raw" "$rx" <<'PY'
import sys, wave, numpy as np
src, dst = sys.argv[1], sys.argv[2]
w = wave.open(src); n = w.getnframes(); ch = w.getnchannels(); fr = w.getframerate()
a = np.frombuffer(w.readframes(n), dtype="<i4").reshape(-1, ch).astype(np.float64) / 2147483648.0
c = int(np.argmax([np.sqrt(np.mean(a[:, k] ** 2)) for k in range(ch)]))  # live channel
x = a[:, c]
pcm = (np.clip(x, -1, 1) * 32767.0).astype("<i2")
o = wave.open(dst, "wb"); o.setnchannels(1); o.setsampwidth(2); o.setframerate(fr)
o.writeframes(pcm.tobytes()); o.close()
pk = np.max(np.abs(x)) or 1e-9
clipped = int(np.sum(np.abs(x) >= 0.999))
warn = f"  *** CLIPPING ({clipped} samples) -- lower input gain ***" if clipped > 50 else ""
print(f"  extracted input{c+1}: peak {20*np.log10(pk):.1f} dBFS{warn}")
PY

echo "[3/3] decoding capture"
"$here/build/rx_campaign" "$rx" "$N" "$FEC"
[ -n "$KEEP" ] && echo "artifacts kept in $KEEP (tx.wav, rx.wav)"
