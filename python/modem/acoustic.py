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


# ── WAV file I/O (the audio-server-agnostic path: PipeWire / JACK / ALSA) ──────
# Generate a WAV, then play it into the radio with ANY server — pw-play / pw-cat,
# jack via a JACK client, or aplay — and capture the radio with pw-record / arecord.
# No live-audio dependency, so the modem works over PipeWire/JACK without sounddevice.
def write_wav(path, audio, fs=FS):
    """Write mono 16-bit PCM (what pw-play / aplay / a JACK player expect)."""
    import wave
    pcm = (np.clip(np.asarray(audio, dtype=np.float32), -1.0, 1.0) * 32767.0).astype("<i2")
    with wave.open(path, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(int(fs))
        w.writeframes(pcm.tobytes())


def read_wav(path):
    """Read a WAV (mono or multi-channel; takes channel 0) -> (float audio, fs)."""
    import wave
    with wave.open(path, "rb") as w:
        fs, ch, sw, n = w.getframerate(), w.getnchannels(), w.getsampwidth(), w.getnframes()
        raw = w.readframes(n)
    dt = {1: np.int8, 2: "<i2", 4: "<i4"}.get(sw, "<i2")
    a = np.frombuffer(raw, dtype=dt).astype(np.float32)
    a = a / float(np.iinfo(np.dtype(dt)).max)
    if ch > 1:
        a = a[::ch]                                     # channel 0
    return a, fs


def _energy_onset(audio, thr_frac=0.3):
    """First sample where the burst rises out of leading silence (aligns a pw-record
    capture to ~bit 0; the AFSK slicer has no fractional-bit recovery yet)."""
    m = np.abs(np.asarray(audio, dtype=np.float32))
    if len(m) == 0:
        return 0
    thr = max(0.02, m.max() * thr_frac)
    above = np.argmax(m > thr)
    return int(above)


def decode_audio(audio, profile="handheld", fs=FS, fec=False, trim=True):
    """Recover a frame from a full audio buffer (e.g. read from a WAV). Trims leading
    silence, then slices the whole buffer into bits and lets the framing find the sync."""
    p = PROFILES[profile]
    sps = int(round(fs / p["baud"]))
    a = np.asarray(audio, dtype=np.float32)
    if trim:
        a = a[_energy_onset(a):]
    nbits = len(a) // sps
    if nbits <= 0:
        return None
    return audio_to_frame(a, sps, nbits, profile=profile, fec=fec)


def _cli():
    import argparse
    sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP"))
    from wirelab_core import encode, decode

    p = argparse.ArgumentParser(
        prog="acoustic", description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="Route over PipeWire/JACK:\n"
               "  acoustic.py tx --text 'SOS' --wav out.wav && pw-play out.wav   # into the radio\n"
               "  pw-record -d <radio-source> in.wav ; acoustic.py rx --wav in.wav  # from the radio")
    sub = p.add_subparsers(dest="cmd", required=True)
    for name in ("tx", "rx", "loopback"):
        s = sub.add_parser(name)
        s.add_argument("--profile", choices=sorted(PROFILES), default="handheld")
        s.add_argument("--fec", action="store_true", help="RS-FEC payload (else frame+crc8)")
        if name != "rx":
            s.add_argument("--text", default="DCF!", help="up to 4 bytes of payload")
            s.add_argument("--hex", help="a 17-byte DeModFrame as hex")
            s.add_argument("--channel", type=lambda x: int(x, 0), default=0xFFFF)
        if name != "tx":
            s.add_argument("--snr", type=float, default=15.0, help="loopback channel SNR dB")
        s.add_argument("--wav", help="WAV file to write (tx) / read (rx)")
    args = p.parse_args()

    def build_frame():
        if getattr(args, "hex", None):
            b = bytes.fromhex(args.hex)
            if len(b) != 17:
                raise SystemExit("--hex must be a 17-byte DeModFrame")
            return b
        return encode(3, 1, 0x00A1, args.channel, args.text.encode()[:4].ljust(4, b"\x00"), 0)

    def report(fr, n):
        try:
            d = decode(fr)
            desc = f"src=0x{d['src']:04x} dst=0x{d['dst']:04x} type={d['frame_type']}"
        except ValueError:
            desc = "(frame CRC fail)"
        print(f"  frame {fr.hex()}  [{n} byte(s) corrected]  {desc}")

    if args.cmd == "tx":
        audio, sps, nbits = frame_to_audio(build_frame(), profile=args.profile, fec=args.fec)
        if not args.wav:
            raise SystemExit("tx needs --wav OUT.wav")
        write_wav(args.wav, audio)
        print(f"acoustic tx: {args.profile} {'fec' if args.fec else 'crc8'}, "
              f"{len(audio)} samples -> {args.wav} (play with: pw-play {args.wav})")
    elif args.cmd == "rx":
        if not args.wav:
            raise SystemExit("rx needs --wav IN.wav")
        audio, fs = read_wav(args.wav)
        r = decode_audio(audio, profile=args.profile, fs=fs, fec=args.fec)
        if r:
            report(*r)
        else:
            print("acoustic rx: no frame recovered")
    else:  # loopback
        out, n = loopback(build_frame(), profile=args.profile, fec=args.fec, snr_db=args.snr)
        print(f"acoustic loopback ({args.profile}, {'fec' if args.fec else 'crc8'}, "
              f"{args.snr:.0f} dB through band-pass+AGC):")
        report(out, n) if out else print("  LOST")


if __name__ == "__main__":
    _cli()
