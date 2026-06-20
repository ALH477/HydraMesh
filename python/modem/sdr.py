#!/usr/bin/env python3
# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""dcf-sdr — carry DeModFrames over real radio (SDR) with FEC, or to/from .cf32 IQ.

Pipeline: frame -> RS-FEC (certified) -> certified byte<->symbol map -> complex IQ
(GFSK/QPSK/QAM/OOK/AFSK-over-FM) -> a SoapySDR device (HackRF/Pluto/LimeSDR/rtl-sdr/
USRP) **or** a hardware-agnostic .cf32 file (GNU Radio, rtl_sdr, hackrf_transfer).

  dcf-sdr tx --text "hi" --mod gfsk --rate 1M --iq out.cf32
  dcf-sdr tx --dcf cap.dcf --mod qpsk --soapy driver=hackrf --freq 433.9M --rate 2M
  dcf-sdr rx --iq in.cf32 --mod gfsk
  dcf-sdr rx --soapy driver=rtlsdr --freq 433.9M --rate 2M --mod gfsk --secs 3

SoapySDR is a SOFT dependency: .cf32 files and the software loopback always work
without hardware or the SoapySDR python bindings. RX of a captured signal needs real
carrier/timing recovery for hardware; the .cf32/loopback path is the certified-clean
demo. Note: receiving is license-free; TRANSMITTING needs a license / ISM band, and
the DCF wire is plaintext — RF has no WireGuard, so treat it as a broadcast.
"""
import argparse
import os
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP"))
import iq as iqmod
from wirelab_core import encode, decode

FRAME_SIZE = 17
GAP = 64                                   # samples of silence between frames on TX


def _parse_freq(s):
    s = str(s).strip().lower()
    mult = 1
    if s.endswith("g"): mult, s = 1e9, s[:-1]
    elif s.endswith("m"): mult, s = 1e6, s[:-1]
    elif s.endswith("k"): mult, s = 1e3, s[:-1]
    return float(s) * mult


def _frames_from_args(args):
    if args.dcf:
        data = open(args.dcf, "rb").read()
        return [data[i:i + FRAME_SIZE] for i in range(0, len(data), FRAME_SIZE)
                if len(data[i:i + FRAME_SIZE]) == FRAME_SIZE]
    if args.hex:
        b = bytes.fromhex(args.hex)
        if len(b) != FRAME_SIZE:
            raise SystemExit("--hex must be a 17-byte DeModFrame")
        return [b]
    if args.text is not None:
        payload = (args.text.encode()[:4]).ljust(4, b"\x00")
        return [encode(3, 1, 0x00A1, args.channel, payload, 0)]
    raise SystemExit("tx needs --text, --hex, or --dcf")


# ── TX ────────────────────────────────────────────────────────────────────────
def cmd_tx(args):
    frames = _frames_from_args(args)
    bursts = []
    for f in frames:
        if args.mod == "afsk-fm":
            s, _sps, _nb = iqmod.afsk_fm_modulate(f)
        else:
            s = iqmod.frame_to_iq(f, mod=args.mod, sps=args.sps)
        bursts.append(s)
        bursts.append(np.zeros(GAP, dtype=np.complex64))
    sig = np.concatenate(bursts).astype(np.complex64)
    print(f"dcf-sdr tx: {len(frames)} frame(s), mod={args.mod}, {len(sig)} IQ samples",
          file=sys.stderr)
    if args.iq:
        iqmod.write_cf32(args.iq, sig)
        print(f"  wrote {args.iq} ({len(sig)} cf32 samples)", file=sys.stderr)
    if args.soapy:
        _soapy_tx(sig, args)


def _soapy_tx(sig, args):
    try:
        import SoapySDR
        from SoapySDR import SOAPY_SDR_TX, SOAPY_SDR_CF32
    except ImportError:
        raise SystemExit("SoapySDR python bindings not found (use --iq for files)")
    dev = SoapySDR.Device(dict(p.split("=") for p in args.soapy.split(",")))
    dev.setSampleRate(SOAPY_SDR_TX, 0, args.rate)
    dev.setFrequency(SOAPY_SDR_TX, 0, _parse_freq(args.freq))
    if args.gain is not None:
        dev.setGain(SOAPY_SDR_TX, 0, args.gain)
    st = dev.setupStream(SOAPY_SDR_TX, SOAPY_SDR_CF32)
    dev.activateStream(st)
    for i in range(0, len(sig), 4096):
        dev.writeStream(st, [sig[i:i + 4096]], len(sig[i:i + 4096]))
    dev.deactivateStream(st); dev.closeStream(st)
    print(f"  transmitted on {args.freq} @ {args.rate} S/s", file=sys.stderr)


# ── RX ────────────────────────────────────────────────────────────────────────
def cmd_rx(args):
    if args.iq:
        sig = iqmod.read_cf32(args.iq)
    elif args.soapy:
        sig = _soapy_rx(args)
    else:
        raise SystemExit("rx needs --iq or --soapy")
    frames = _decode_bursts(sig, args)
    out = open(args.out, "wb") if args.out else None
    for f, n in frames:
        try:
            d = decode(f)
            desc = f"src=0x{d['src']:04x} dst=0x{d['dst']:04x} type={d['frame_type']}"
        except ValueError:
            desc = "(frame CRC fail)"
        print(f"  frame {f.hex()}  [{n} byte(s) corrected]  {desc}")
        if out:
            out.write(f)
    if out:
        out.close()
        print(f"dcf-sdr rx: wrote {len(frames)} frame(s) -> {args.out}", file=sys.stderr)
    if not frames:
        print("dcf-sdr rx: no frames recovered", file=sys.stderr)


def _soapy_rx(args):
    try:
        import SoapySDR
        from SoapySDR import SOAPY_SDR_RX, SOAPY_SDR_CF32
    except ImportError:
        raise SystemExit("SoapySDR python bindings not found (use --iq for files)")
    dev = SoapySDR.Device(dict(p.split("=") for p in args.soapy.split(",")))
    dev.setSampleRate(SOAPY_SDR_RX, 0, args.rate)
    dev.setFrequency(SOAPY_SDR_RX, 0, _parse_freq(args.freq))
    if args.gain is not None:
        dev.setGain(SOAPY_SDR_RX, 0, args.gain)
    st = dev.setupStream(SOAPY_SDR_RX, SOAPY_SDR_CF32)
    dev.activateStream(st)
    n = int(args.rate * args.secs)
    buf = np.zeros(n, dtype=np.complex64)
    got = 0
    chunk = np.zeros(4096, dtype=np.complex64)
    while got < n:
        sr = dev.readStream(st, [chunk], len(chunk))
        if sr.ret > 0:
            buf[got:got + sr.ret] = chunk[:sr.ret]
            got += sr.ret
    dev.deactivateStream(st); dev.closeStream(st)
    return buf[:got]


def _decode_bursts(sig, args):
    """Energy-gate the IQ into bursts and decode each. For .cf32 produced by `tx`
    (back-to-back frames + gaps) this recovers every frame; for a real off-air
    capture, carrier/timing recovery is a future addition."""
    mag = np.abs(sig)
    thr = max(0.05, mag.max() * 0.25) if len(mag) else 0.0
    on = mag > thr
    frames = []
    i, N = 0, len(sig)
    while i < N:
        if not on[i]:
            i += 1
            continue
        j = i
        while j < N and (on[j] or (j + GAP < N and on[j:j + GAP].any())):
            j += 1
        burst = sig[i:j]
        r = (iqmod.afsk_fm_demodulate(burst, int(round(48000 / 1200)), len(burst) // 40)
             if args.mod == "afsk-fm"
             # sync=True: recover carrier-offset/timing/phase for real off-air captures.
             else iqmod.iq_to_frame(burst, mod=args.mod, sps=args.sps, sync=True))
        if r:
            frames.append(r)
        i = j + 1
    return frames


def build_parser():
    p = argparse.ArgumentParser(prog="dcf-sdr", description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--mod", default="gfsk",
                        choices=["fsk", "gfsk", "psk", "qpsk", "qam", "ook", "ask", "afsk-fm"])
    common.add_argument("--sps", type=int, default=8, help="samples/symbol (digital mods)")
    common.add_argument("--soapy", metavar="driver=NAME,...", help="SoapySDR device args")
    common.add_argument("--freq", default="433.9M", help="center frequency (e.g. 433.9M)")
    common.add_argument("--rate", type=lambda s: _parse_freq(s), default=1e6, help="sample rate")
    common.add_argument("--gain", type=float, help="device gain (dB)")

    t = sub.add_parser("tx", parents=[common], help="modulate frame(s) -> SDR / .cf32")
    t.add_argument("--text", help="send a short text in one frame's payload")
    t.add_argument("--hex", help="a 17-byte DeModFrame as hex")
    t.add_argument("--dcf", help="a .dcf file of frames")
    t.add_argument("--channel", type=lambda s: int(s, 0), default=0xFFFF, help="frame dst")
    t.add_argument("--iq", metavar="OUT.cf32", help="also write interleaved float32 IQ")
    t.set_defaults(func=cmd_tx)

    r = sub.add_parser("rx", parents=[common], help="demodulate SDR / .cf32 -> frames")
    r.add_argument("--iq", metavar="IN.cf32", help="read interleaved float32 IQ")
    r.add_argument("--secs", type=float, default=2.0, help="SoapySDR capture seconds")
    r.add_argument("--out", metavar="OUT.dcf", help="write recovered frames to a .dcf")
    r.set_defaults(func=cmd_rx)
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    args.func(args)


if __name__ == "__main__":
    main()
