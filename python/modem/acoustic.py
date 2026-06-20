# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF acoustic modem (pure-numpy) — carry a DeModFrame as real voice-band audio for a
handheld walkie-talkie, with a model of the radio channel so the handheld profile is
testable without hardware.

It is **interoperable with the real-time Faust modem** (`main.py`): both build the on-air
bit stream from the shared `acoustic_frame` module and use the same `PROFILES` tones/baud,
so a frame transmitted by either is decodable by the other. The audio layer here just
renders those bits as continuous-phase AFSK and detects them; `main.py` renders the same
bits through its JIT-compiled Faust FSK DSP.

Pipeline:  frame -> acoustic_frame.encode_bits (preamble | 0x7E | frame+crc8 *or* RS
codeword) -> continuous-phase AFSK in the 300-3000 Hz band -> [walkie channel: band-pass +
AGC + noise] -> non-coherent mark/space correlation -> acoustic_frame.decode_bits -> frame.

Profiles (Hz): "handheld" 1200/1800 (both mid-band, the radio-friendly choice), "standard"
1200/2200 (classic Bell-202 AFSK), both at 300 baud. `fec=True` swaps the frame+crc8
payload for the certified RS codeword so the radio's byte-errors are corrected, not just
detected. See Documentation/DCF_FIELD_USE.md.
"""
import os
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import acoustic_frame as af

PROFILES = af.PROFILES
FS = 48000              # audio sample rate


def frame_to_audio(frame, profile="handheld", fs=FS, fec=False,
                   mark=None, space=None, baud=None, preamble_bits=None):
    """DeModFrame bytes -> mono float32 audio, continuous phase. Returns (audio, sps,
    nbits). Bits come from the shared framing, so this is byte-identical to what the Faust
    modem puts on air for the same profile/fec setting."""
    p = PROFILES[profile]
    mark = p["mark"] if mark is None else mark
    space = p["space"] if space is None else space
    baud = p["baud"] if baud is None else baud
    pre = p["preamble_bits"] if preamble_bits is None else preamble_bits
    bits = af.encode_bits(bytes(frame), fec_mode=fec, preamble_bits=pre)
    sps = int(round(fs / baud))
    audio = np.zeros(len(bits) * sps, dtype=float)
    phase = 0.0
    t = np.arange(sps)
    for i, b in enumerate(bits):
        f = mark if b else space
        ph = phase + 2 * np.pi * f * t / fs
        audio[i * sps:(i + 1) * sps] = np.sin(ph)
        phase = (ph[-1] + 2 * np.pi * f / fs) % (2 * np.pi)   # carry -> continuous phase
    return audio.astype(np.float32), sps, len(bits)


def audio_to_frame(audio, sps, nbits, profile="handheld", fs=FS, fec=False,
                   mark=None, space=None):
    """Mono audio -> (frame bytes, n_corrected) or None. Non-coherent FSK detection
    (per-bit correlation energy at mark vs space) -> shared decode."""
    p = PROFILES[profile]
    mark = p["mark"] if mark is None else mark
    space = p["space"] if space is None else space
    audio = np.asarray(audio, dtype=float)
    t = np.arange(sps)
    ref_mark = np.exp(-2j * np.pi * mark * t / fs)
    ref_space = np.exp(-2j * np.pi * space * t / fs)
    bits = []
    for i in range(nbits):
        seg = audio[i * sps:(i + 1) * sps]
        if len(seg) < sps:
            break
        bits.append(1 if abs(np.dot(seg, ref_mark)) > abs(np.dot(seg, ref_space)) else 0)
    return af.decode_bits(bits, fec_mode=fec)


def walkie_channel(audio, fs=FS, snr_db=20.0, band=(300.0, 3000.0), agc=True, seed=None):
    """Model a handheld FM radio's audio path: a hard 300-3000 Hz band-pass (the radio's
    voice filters), AGC (RMS-normalise + a short attack transient the keyup preamble rides
    out), and additive noise. Why the field profile keeps both tones mid-band and leans on
    a long preamble + FEC."""
    rng = np.random.default_rng(seed)
    x = np.asarray(audio, dtype=float)
    X = np.fft.rfft(x)
    f = np.fft.rfftfreq(len(x), 1.0 / fs)
    X[(f < band[0]) | (f > band[1])] = 0.0            # the radio's filters are this hard
    y = np.fft.irfft(X, n=len(x))
    if agc:
        rms = np.sqrt(np.mean(y ** 2)) or 1.0
        y = y / rms
        att = max(1, int(0.005 * fs))                 # ~5 ms attack ramp at keyup
        y[:att] *= np.linspace(0.0, 1.0, att)
    p = np.mean(y ** 2) or 1.0
    y = y + np.sqrt(p / (10 ** (snr_db / 10.0))) * rng.standard_normal(len(y))
    return y.astype(np.float32)


def loopback(frame, profile="handheld", fec=False, snr_db=20.0, channel=True, seed=0):
    """frame -> audio -> (optional walkie channel) -> frame. Returns (frame|None, n|None)."""
    audio, sps, nbits = frame_to_audio(frame, profile=profile, fec=fec)
    if channel:
        audio = walkie_channel(audio, snr_db=snr_db, seed=seed)
    r = audio_to_frame(audio, sps, nbits, profile=profile, fec=fec)
    return r if r else (None, None)


if __name__ == "__main__":
    sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP"))
    from wirelab_core import encode
    fr = encode(3, 1, 0x00A1, 0xFFFF, b"DCF!", 0)
    print("DCF acoustic modem (numpy) — interoperable with the Faust modem via acoustic_frame\n")
    for prof in ("handheld", "standard"):
        for fec in (False, True):
            out, n = loopback(fr, profile=prof, fec=fec, snr_db=15.0)
            mode = "fec " if fec else "crc8"
            ok = out == fr
            extra = f" ({n} byte-errors corrected)" if (ok and fec) else ""
            print(f"  {prof:9} {mode} @15dB through band-pass+AGC: "
                  f"{'recovered' if ok else 'LOST'}{extra}")
