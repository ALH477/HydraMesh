#!/usr/bin/env python3
# SPDX-License-Identifier: LGPL-3.0-only
# dcf-tools/link-meter.py -- live analog-link tuner for the HydraModem cable test.
#
# Plays a steady tone (or the real modem signal) out --tx-dev while showing a live
# input-level meter for BOTH channels of --rx-dev, so you can adjust an analog path
# -- mixer routing, output level, input gain, cabling -- and watch signal arrive in
# real time. Tune to a peak around -12..-3 dBFS (healthy, not clipping), Ctrl-C,
# then run field-test.sh for that direction. Repo glue (DeMoD LLC, LGPL-3.0).
#
#   ./link-meter.py --tx-dev plughw:4,0 --rx-dev plughw:3,0           # tone
#   ./link-meter.py --tx-dev plughw:4,0 --rx-dev plughw:3,0 --modem WAV  # real signal
import argparse, math, os, subprocess, tempfile, wave
import numpy as np


def gen_tone(path, freq, secs, fs, amp=0.8):
    t = np.arange(int(fs * secs))
    x = amp * np.sin(2 * np.pi * freq * t / fs)
    pcm = (np.clip(x, -1, 1) * 32767).astype("<i2")
    w = wave.open(path, "wb")
    w.setnchannels(1); w.setsampwidth(2); w.setframerate(fs)
    w.writeframes(pcm.tobytes()); w.close()


def dbfs(x):
    pk = float(np.max(np.abs(x))) if len(x) else 0.0
    return 20 * math.log10(pk) if pk > 1e-12 else -120.0


def bar(db, lo=-60.0):
    n = int(max(0.0, (db - lo) / -lo) * 40)
    return "#" * n


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tx-dev", required=True, help="ALSA play device, e.g. plughw:4,0")
    ap.add_argument("--rx-dev", required=True, help="ALSA capture device, e.g. plughw:3,0")
    ap.add_argument("--freq", type=float, default=2500.0, help="tone Hz (in the 2-3 kHz modem band)")
    ap.add_argument("--rate", type=int, default=48000)
    ap.add_argument("--modem", help="play this WAV (real modem signal) instead of a tone")
    a = ap.parse_args()

    tmp = tempfile.mkdtemp()
    src = a.modem
    if not src:
        src = os.path.join(tmp, "tone.wav")
        gen_tone(src, a.freq, 30, a.rate)

    play = subprocess.Popen(
        ["bash", "-c", f'while true; do aplay -q -D "{a.tx_dev}" "{src}" || break; done'])
    rec = subprocess.Popen(
        ["arecord", "-D", a.rx_dev, "-f", "S32_LE", "-c", "2", "-r", str(a.rate),
         "-t", "raw", "-q"], stdout=subprocess.PIPE)

    what = f"modem WAV {os.path.basename(src)}" if a.modem else f"{a.freq:.0f} Hz tone"
    print(f"playing {what} on {a.tx_dev}; metering {a.rx_dev} (ch0=input1, ch1=input2).")
    print("tune to peak ~ -12..-3 dBFS, then Ctrl-C and run field-test.sh.\n")

    win = int(a.rate * 0.15)
    fbytes = 4 * 2
    buf = b""
    try:
        while True:
            need = win * fbytes - len(buf)
            chunk = rec.stdout.read(need)
            if not chunk:
                break
            buf += chunk
            if len(buf) < win * fbytes:
                continue
            x = np.frombuffer(buf[:win * fbytes], dtype="<i4").reshape(-1, 2).astype(np.float64)
            x /= 2147483648.0
            buf = b""
            d0, d1 = dbfs(x[:, 0]), dbfs(x[:, 1])
            best = max(d0, d1)
            tag = ("CLIP!" if best > -1 else
                   "  OK " if best > -24 else
                   " low " if best > -50 else
                   " none")
            print(f"\rin1 {d0:6.1f} [{bar(d0):<40}] in2 {d1:6.1f}  {tag}",
                  end="", flush=True)
    except KeyboardInterrupt:
        pass
    finally:
        rec.terminate()
        play.terminate()
        subprocess.run(["pkill", "-f", f'aplay -q -D {a.tx_dev}'],
                       stderr=subprocess.DEVNULL, stdout=subprocess.DEVNULL)
        print("\nstopped.")


if __name__ == "__main__":
    main()
