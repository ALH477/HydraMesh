# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF IQ modem — carry a DeModFrame over complex baseband for SDR (and AM/FM).

Pipeline:  frame(17B) → RS-FEC (codec-certified) → bytes → certified byte↔symbol map
(modulationlab_core) → **complex IQ** ready for a SoapySDR sink / .cf32 file, and the
exact reverse. Five medium families: 2-FSK/GFSK, BPSK/QPSK, 16-QAM, OOK/ASK/AM, and
AFSK-over-FM (the Bell-202 audio FSK NBFM'd onto a carrier — interoperates with real
FM gear). The byte↔symbol map and the RS-FEC are the **certified** layers; the IQ
waveform here is analog and loopback-tested (like the existing modem waveform).

The FEC is what makes it legitimately useful over a lossy RF channel: the AWGN/burst
errors a real radio injects are *corrected*, not just detected.
"""
import os
import sys

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))
import modulationlab_core as ml
import feclab_core as fec

MOD = {"fsk": ml.MOD_FSK, "gfsk": ml.MOD_FSK, "psk": ml.MOD_PSK, "qpsk": ml.MOD_PSK,
       "qam": ml.MOD_QAM, "ook": ml.MOD_OOK, "ask": ml.MOD_OOK, "am": ml.MOD_OOK}

PREAMBLE = bytes([0xAA] * 6)     # 1010… for timing/energy
SYNC = bytes([0x7E])             # frame delimiter (01111110)


# ── bit/byte framing ──────────────────────────────────────────────────────────
def _bytes_to_symbols(mod, data):
    return ml.modulate(mod, data)


def _symbols_to_bytes(mod, syms, nbytes):
    return ml.demodulate(mod, syms, nbytes)


# ── per-family IQ rendering (symbols → complex baseband) ──────────────────────
def _render_psk(mod, syms, sps):
    con = ml.constellation(mod)
    pts = np.array([np.exp(1j * np.deg2rad(con[s]["phase_deg"])) for s in syms])
    return np.repeat(pts, sps)


def _render_qam(mod, syms, sps):
    con = ml.constellation(mod)
    pts = np.array([(con[s]["i"] + 1j * con[s]["q"]) / 3.0 for s in syms])
    return np.repeat(pts, sps)


def _render_fsk(syms, sps, gaussian, dev=0.25):
    # binary CPFSK: symbol 0 -> -dev, 1 -> +dev (cycles/sample), continuous phase.
    f = np.repeat(np.where(np.array(syms) > 0, dev, -dev).astype(float), sps)
    if gaussian:
        k = _gaussian_kernel(sps, bt=0.5)
        f = np.convolve(f, k, mode="same")
    phase = 2 * np.pi * np.cumsum(f)
    return np.exp(1j * phase)


def _render_ook(syms, sps):
    amp = np.repeat(np.array(syms, dtype=float), sps)   # 0/1
    return amp.astype(complex)


def _gaussian_kernel(sps, bt=0.5):
    n = sps * 4
    t = np.arange(-n, n + 1) / sps
    a = np.sqrt(np.log(2)) / (np.sqrt(2) * bt)
    g = np.exp(-(a * t) ** 2)
    return g / g.sum()


# ── per-family demod (complex baseband → symbols) ─────────────────────────────
def _demod_psk(mod, iq, sps, nsyms):
    con = ml.constellation(mod)
    pts = {s: np.exp(1j * np.deg2rad(con[s]["phase_deg"])) for s in con}
    return _nearest(iq, sps, nsyms, pts)


def _demod_qam(mod, iq, sps, nsyms):
    con = ml.constellation(mod)
    pts = {s: (con[s]["i"] + 1j * con[s]["q"]) / 3.0 for s in con}
    return _nearest(iq, sps, nsyms, pts)


def _nearest(iq, sps, nsyms, pts):
    syms = []
    keys = list(pts)
    cvals = np.array([pts[k] for k in keys])
    for n in range(nsyms):
        seg = iq[n * sps:(n + 1) * sps]
        z = seg.mean()                                   # matched filter (rect)
        syms.append(keys[int(np.argmin(np.abs(cvals - z)))])
    return syms


def _demod_fsk(iq, sps, nsyms):
    # FM discriminator: instantaneous frequency = angle(x[n] * conj(x[n-1])).
    d = np.angle(iq[1:] * np.conj(iq[:-1]))
    d = np.concatenate([[d[0]], d])
    means = np.array([d[n * sps:(n + 1) * sps].mean() for n in range(nsyms)])
    if len(means) == 0:
        return []
    # Slice against the MIDPOINT of the mark/space clusters, not the data mean. A
    # global-mean threshold drifts with bit balance, so unbalanced payloads (e.g. a
    # run of identical bytes) misdecode; the cluster midpoint is both balance- and
    # carrier-offset-robust — the 0xAA preamble guarantees both tones are present.
    thr = (means.max() + means.min()) / 2.0
    return [1 if m > thr else 0 for m in means]


def _demod_ook(iq, sps, nsyms):
    env = np.abs(iq)
    syms = []
    for n in range(nsyms):
        syms.append(1 if env[n * sps:(n + 1) * sps].mean() > 0.5 else 0)
    return syms


# ── high-level frame TX/RX (FEC + framing + IQ) ───────────────────────────────
def _modulate_bytes(data, mod, sps):
    """Render an arbitrary byte stream to complex baseband (no FEC/framing)."""
    m = MOD[mod]
    syms = _bytes_to_symbols(m, data)
    if mod in ("psk", "qpsk"):
        return _render_psk(m, syms, sps)
    if mod == "qam":
        return _render_qam(m, syms, sps)
    if mod in ("fsk", "gfsk"):
        return _render_fsk(syms, sps, gaussian=(mod == "gfsk"))
    if mod in ("ook", "ask", "am"):
        return _render_ook(syms, sps)
    raise ValueError(mod)


def frame_to_iq(frame, mod="gfsk", nparity=fec.RS_DEFAULT_NPARITY, sps=8):
    """DeModFrame bytes -> complex64 IQ (preamble + sync + RS codeword)."""
    code = fec.rs_encode(bytes(frame), nparity)
    return _modulate_bytes(PREAMBLE + SYNC + code, mod, sps).astype(np.complex64)


# ── receiver synchronization (carrier-freq offset, timing, phase) ─────────────
def _estimate_cfo(iq, sps):
    """Coarse carrier-frequency offset (cycles/sample) from the preamble's self-
    similarity — the 0xAA/constant-symbol preamble autocorrelates at lag=sps."""
    w = iq[: max(2 * sps, min(len(iq), 64 * sps))]
    if len(w) <= sps:
        return 0.0
    acc = np.sum(w[sps:] * np.conj(w[:-sps]))
    return float(np.angle(acc) / (2 * np.pi * sps))


def _derotate(iq, foff):
    return iq * np.exp(-2j * np.pi * foff * np.arange(len(iq)))


def _find_start(iq, ref):
    """Symbol-timing/frame start via preamble cross-correlation (argmax |corr|)."""
    if len(iq) < len(ref):
        return 0
    c = np.correlate(iq, ref, mode="valid")
    return int(np.argmax(np.abs(c)))


def _energy_onset(iq):
    """First sample where the burst rises out of the noise/leading delay."""
    m = np.abs(iq)
    if len(m) == 0:
        return 0
    thr = max(0.05, m.max() * 0.3)
    return int(np.argmax(m > thr))


def _synchronize(iq, mod, sps):
    """Return the burst aligned to the preamble, CFO- and phase-corrected."""
    ref = _modulate_bytes(PREAMBLE + SYNC, mod, sps)
    if mod in ("psk", "qpsk", "qam"):
        # CFO from JUST the (constant-symbol) preamble window after energy onset.
        onset = _energy_onset(iq)
        pre_len = len(PREAMBLE) * 8 // ml.BITS_PER_SYMBOL[MOD[mod]] * sps
        win = iq[onset:onset + pre_len]
        iq = _derotate(iq, _estimate_cfo(win, sps))
        start = _find_start(iq, ref)
        seg = iq[start:start + len(ref)]
        if len(seg) == len(ref):                            # static phase from known preamble
            phi = np.angle(np.vdot(ref, seg))
            iq = iq * np.exp(-1j * phi)
        return iq[start:]
    if mod in ("fsk", "gfsk"):
        return iq[_find_start(iq, ref):]                    # FSK CFO handled in demod (DC)
    return iq[_find_start(np.abs(iq).astype(complex), np.abs(ref).astype(complex)):]


def iq_to_frame(iq, mod="gfsk", msglen=17, nparity=fec.RS_DEFAULT_NPARITY, sps=8, sync=False):
    """Complex IQ -> (frame bytes, n_corrected) or None if undecodable. With
    sync=True, recover carrier-frequency offset, symbol timing, and phase first
    (for real off-air captures, not just clean loopback)."""
    m = MOD[mod]
    iq = np.asarray(iq)
    if sync:
        iq = _synchronize(iq, mod, sps)
    nbytes = len(PREAMBLE) + len(SYNC) + msglen + nparity
    nsyms = (nbytes * 8 + ml.BITS_PER_SYMBOL[m] - 1) // ml.BITS_PER_SYMBOL[m]
    nsyms = min(nsyms, len(iq) // sps)
    if mod in ("psk", "qpsk"):
        syms = _demod_psk(m, iq, sps, nsyms)
    elif mod == "qam":
        syms = _demod_qam(m, iq, sps, nsyms)
    elif mod in ("fsk", "gfsk"):
        syms = _demod_fsk(iq, sps, nsyms)
    elif mod in ("ook", "ask", "am"):
        syms = _demod_ook(iq, sps, nsyms)
    else:
        raise ValueError(mod)
    data = _symbols_to_bytes(m, syms, nbytes)
    idx = data.find(SYNC, len(PREAMBLE) - 2)             # locate frame start
    if idx < 0:
        idx = len(PREAMBLE)
    code = data[idx + 1:idx + 1 + msglen + nparity]
    if len(code) < msglen + nparity:
        return None
    try:
        msg, n = fec.rs_decode(code, nparity, msglen)
        return bytes(msg), n
    except fec.FecError:
        return None


# ── AFSK-over-FM (Bell-202 audio FSK, NBFM'd onto a carrier) ───────────────────
def afsk_fm_modulate(frame, fs=48000, baud=1200, mark=1200, space=2200,
                     fm_dev=3000.0, nparity=fec.RS_DEFAULT_NPARITY):
    """Frame -> NBFM IQ carrying Bell-202-style AFSK audio (interops with FM radios)."""
    code = fec.rs_encode(bytes(frame), nparity)
    bits = _to_bits(PREAMBLE + SYNC + code)
    sps = int(round(fs / baud))
    audio = np.zeros(len(bits) * sps)
    phase = 0.0
    for i, b in enumerate(bits):
        f = mark if b else space
        t = np.arange(sps)
        ph = phase + 2 * np.pi * f * t / fs
        audio[i * sps:(i + 1) * sps] = np.sin(ph)
        phase = (ph[-1] + 2 * np.pi * f / fs) % (2 * np.pi)
    iq = np.exp(1j * 2 * np.pi * fm_dev * np.cumsum(audio) / fs)
    return iq.astype(np.complex64), sps, len(bits)


def afsk_fm_demodulate(iq, sps, nbits, fs=48000, mark=1200, space=2200,
                       msglen=17, nparity=fec.RS_DEFAULT_NPARITY):
    """NBFM IQ -> (frame, n_corrected) or None."""
    audio = np.angle(iq[1:] * np.conj(iq[:-1]))          # FM discriminator -> the AFSK audio
    audio = np.concatenate([[audio[0]], audio])
    t = np.arange(sps)
    ref_mark = np.exp(-2j * np.pi * mark * t / fs)
    ref_space = np.exp(-2j * np.pi * space * t / fs)
    bits = []
    for i in range(nbits):
        seg = audio[i * sps:(i + 1) * sps]
        if len(seg) < sps:
            break
        me = abs(np.dot(seg, ref_mark))                  # non-coherent FSK detection
        se = abs(np.dot(seg, ref_space))
        bits.append(1 if me > se else 0)
    data = _from_bits(bits)
    idx = data.find(SYNC, len(PREAMBLE) - 2)
    if idx < 0:
        idx = len(PREAMBLE)
    code = data[idx + 1:idx + 1 + msglen + nparity]
    if len(code) < msglen + nparity:
        return None
    try:
        msg, n = fec.rs_decode(code, nparity, msglen)
        return bytes(msg), n
    except fec.FecError:
        return None


def _to_bits(data):
    return [(b >> (7 - k)) & 1 for b in data for k in range(8)]


def _from_bits(bits):
    out = bytearray()
    for i in range(0, len(bits) - 7, 8):
        v = 0
        for k in range(8):
            v = (v << 1) | bits[i + k]
        out.append(v)
    return bytes(out)


# ── channel + .cf32 I/O ───────────────────────────────────────────────────────
def awgn(iq, snr_db):
    iq = np.asarray(iq, dtype=np.complex64)
    p = np.mean(np.abs(iq) ** 2) or 1.0
    n = np.sqrt(p / (2 * 10 ** (snr_db / 10)))
    noise = n * (np.random.randn(len(iq)) + 1j * np.random.randn(len(iq)))
    return (iq + noise).astype(np.complex64)


def write_cf32(path, iq):
    np.asarray(iq, dtype=np.complex64).tofile(path)      # interleaved float32 I,Q


def read_cf32(path):
    return np.fromfile(path, dtype=np.complex64)
