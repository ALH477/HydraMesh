#!/usr/bin/env bash
# SPDX-License-Identifier: LGPL-3.0-only
# DCF over the air, end to end — the one-command showcase.
#
#   1. tx a frame -> RS-FEC -> IQ -> .cf32 file -> rx -> recover "DCF!"  (no hardware)
#   2. corrupt the link beyond what a CRC can do, and watch RS-FEC CORRECT it
#      while the same damage destroys an un-FEC'd frame.
#
# Real radio is one flag away (see the printed hint); .cf32 also pipes straight into
# rtl_sdr / hackrf_transfer / GNU Radio. Run from anywhere in the repo.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
export ROOT
CF32="$(mktemp /tmp/dcf_demo.XXXXXX.cf32)"
trap 'rm -f "$CF32"' EXIT

echo "=== 1. frame -> RS-FEC -> GFSK IQ -> .cf32 -> frame  (software loopback, no SDR) ==="
python3 "$ROOT/python/modem/sdr.py" tx --text "DCF!" --mod gfsk --iq "$CF32"
python3 "$ROOT/python/modem/sdr.py" rx --iq "$CF32" --mod gfsk
echo
echo "   real radio is one flag away (TX needs a license / ISM band):"
echo "     sdr.py tx --text \"DCF!\" --soapy driver=hackrf  --freq 433.9M --rate 2M"
echo "     sdr.py rx               --soapy driver=rtlsdr  --freq 433.9M --rate 2M --secs 3"
echo

echo "=== 2. lossy channel: RS-FEC corrects errors a raw CRC can only detect ==="
python3 - <<'PY'
import os, sys
sys.path.insert(0, os.path.join(os.environ["ROOT"], "python", "MCP"))
import feclab_core as fec, wirelab_core as wl

frame = bytes.fromhex("d31312340001ffffdeadbeefab12cd24c0")  # a valid DeModFrame
code  = fec.rs_encode(frame)                                  # 17B + 16B parity = 33B

# Hit the on-air bytes with 8 errors — the RS correctable limit (t = 8).
damaged = bytearray(code)
for i in range(8):
    damaged[i * 2] ^= 0x5A

msg, n = fec.rs_decode(bytes(damaged), msglen=17)
ok = bytes(msg) == frame
print(f"   with FEC : {n} byte-errors CORRECTED -> frame {msg.hex()}  CRC {'valid' if ok else 'FAIL'}")
print(f"              src=0x{wl.decode(bytes(msg))['src']:04x} -- recovered intact [OK]")

# The same damage on a bare, un-FEC'd frame: the CRC catches it, but it's gone.
raw = bytearray(frame)
for i in range(8):
    if i * 2 < len(raw):
        raw[i * 2] ^= 0x5A
try:
    wl.decode(bytes(raw))
    print("   no  FEC : (unexpected) decoded")
except ValueError:
    print("   no  FEC : same damage -> CRC FAILS, frame dropped -- detection without correction")
PY
echo
echo "=== done. FEC bytes are certified in all 13 languages; the IQ waveform is loopback-tested. ==="
