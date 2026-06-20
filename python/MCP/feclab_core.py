# SPDX-License-Identifier: LGPL-3.0-only
"""DCF FEC-Lab core — the certified forward-error-correction adapter.

DCF frames are detection-only (CRC-16). For lossy media (RF/SDR, acoustic) this adds
*correction*: a systematic **Reed-Solomon** code over GF(2^8) plus a **block
interleaver** that spreads RF burst/fading errors across codewords. It is an adapter
*around* the 17-byte DeModFrame — the frame and its 246-vector certificate are
untouched; FEC wraps the frame, the radio corrupts it, RS recovers it, then the frame
CRC validates.

This is the **certified** half (byte-deterministic, golden-vectored across
Python/C/Rust — exactly like SuperPack). The IQ waveform that carries these bytes is
analog and loopback-tested, not certified (see DCF_SDR_SPEC.md).

Field: GF(2^8), primitive polynomial **0x11D**, generator **α=2**, first consecutive
root **fcr=0** (the QR/DVB field). Systematic: a codeword is `message ++ parity`, so
an undamaged frame passes straight through and parity is pure overhead. Default parity
`2t = 16` bytes → corrects up to **t = 8 byte-errors** per codeword.

References mirror this byte-for-byte: ``codec/demod_fec.h`` (C), ``codec/src/fec.rs``
(Rust). Vectors: ``Documentation/fec_vectors.json`` (+ ``python/MCP/`` copy) and
``codec/fec_vectors.gen.h``. See ``python/MCP/gen_fec_vectors.py``.
"""

GF_PRIM = 0x11D          # primitive polynomial of GF(2^8)
GF_GEN = 2               # α (generator element)
FCR = 0                  # first consecutive root: roots are α^0 .. α^(2t-1)
RS_DEFAULT_NPARITY = 16  # 2t -> corrects t=8 byte errors


class FecError(Exception):
    """Raised when a codeword has more errors than the code can correct."""


# ── GF(256) arithmetic ────────────────────────────────────────────────────────
_EXP = [0] * 512
_LOG = [0] * 256
_x = 1
for _i in range(255):
    _EXP[_i] = _x
    _LOG[_x] = _i
    _x <<= 1
    if _x & 0x100:
        _x ^= GF_PRIM
for _i in range(255, 512):
    _EXP[_i] = _EXP[_i - 255]


def gf_mul(a, b):
    return 0 if (a == 0 or b == 0) else _EXP[_LOG[a] + _LOG[b]]


def gf_div(a, b):
    if b == 0:
        raise ZeroDivisionError
    return 0 if a == 0 else _EXP[(_LOG[a] + 255 - _LOG[b]) % 255]


def gf_pow(a, p):
    return _EXP[(_LOG[a] * p) % 255]


def gf_inv(a):
    return _EXP[255 - _LOG[a]]


# ── GF polynomials (coefficients high-order first) ────────────────────────────
def _pscale(p, s):
    return [gf_mul(c, s) for c in p]


def _padd(p, q):
    r = [0] * max(len(p), len(q))
    for i in range(len(p)):
        r[i + len(r) - len(p)] = p[i]
    for i in range(len(q)):
        r[i + len(r) - len(q)] ^= q[i]
    return r


def _pmul(p, q):
    r = [0] * (len(p) + len(q) - 1)
    for j in range(len(q)):
        for i in range(len(p)):
            r[i + j] ^= gf_mul(p[i], q[j])
    return r


def _peval(p, x):
    y = p[0]
    for i in range(1, len(p)):
        y = gf_mul(y, x) ^ p[i]
    return y


def _pdiv(dividend, divisor):
    out = list(dividend)
    for i in range(len(dividend) - (len(divisor) - 1)):
        coef = out[i]
        if coef != 0:
            for j in range(1, len(divisor)):
                if divisor[j] != 0:
                    out[i + j] ^= gf_mul(divisor[j], coef)
    sep = -(len(divisor) - 1)
    return out[:sep], out[sep:]


def rs_generator_poly(nparity):
    """g(x) = ∏ (x - α^(fcr+i)), i in 0..nparity-1 (high-order first)."""
    g = [1]
    for i in range(nparity):
        g = _pmul(g, [1, gf_pow(GF_GEN, FCR + i)])
    return g


# ── encode (systematic) ───────────────────────────────────────────────────────
def rs_encode(msg, nparity=RS_DEFAULT_NPARITY):
    """message bytes -> codeword bytes (message ++ nparity parity bytes)."""
    msg = bytes(msg)
    if len(msg) + nparity > 255:
        raise ValueError("RS(255): message+parity must be <= 255 bytes")
    gen = rs_generator_poly(nparity)
    out = list(msg) + [0] * nparity
    for i in range(len(msg)):
        coef = out[i]
        if coef != 0:
            for j in range(1, len(gen)):
                out[i + j] ^= gf_mul(gen[j], coef)
    return msg + bytes(out[len(msg):])


# ── decode (Berlekamp–Massey → Chien → Forney) ────────────────────────────────
def _syndromes(cw, nparity):
    s = [_peval(cw, gf_pow(GF_GEN, FCR + i)) for i in range(nparity)]
    return [0] + s                       # leading 0 (Forney convention)


def _error_locator(synd, nparity):
    err_loc, old_loc = [1], [1]
    synd_shift = len(synd) - nparity
    for i in range(nparity):
        k = i + synd_shift
        delta = synd[k]
        for j in range(1, len(err_loc)):
            delta ^= gf_mul(err_loc[-(j + 1)], synd[k - j])
        old_loc = old_loc + [0]
        if delta != 0:
            if len(old_loc) > len(err_loc):
                new_loc = _pscale(old_loc, delta)
                old_loc = _pscale(err_loc, gf_inv(delta))
                err_loc = new_loc
            err_loc = _padd(err_loc, _pscale(old_loc, delta))
    while err_loc and err_loc[0] == 0:
        del err_loc[0]
    return err_loc


def _error_positions(err_loc, nmess):
    errs = len(err_loc) - 1
    pos = [nmess - 1 - i for i in range(nmess)
           if _peval(err_loc, gf_pow(GF_GEN, i)) == 0]
    if len(pos) != errs:
        raise FecError("error location failed (too many errors)")
    return pos


def _correct_errata(cw, synd, err_pos):
    coef_pos = [len(cw) - 1 - p for p in err_pos]
    # errata locator
    e_loc = [1]
    for p in coef_pos:
        e_loc = _pmul(e_loc, _padd([1], [gf_pow(GF_GEN, p), 0]))
    # error evaluator Ω(x) = (S(x)·Λ(x)) mod x^(e+1)
    synd_rev = synd[::-1]
    prod = _pmul(synd_rev, e_loc)
    _, e_eval = _pdiv(prod, [1] + [0] * (len(e_loc)))
    e_eval = e_eval[::-1]
    # error positions in the field
    X = [gf_pow(GF_GEN, -(255 - p)) for p in coef_pos]
    out = list(cw)
    for i, Xi in enumerate(X):
        Xi_inv = gf_inv(Xi)
        denom = 1
        for j in range(len(X)):
            if j != i:
                denom = gf_mul(denom, 1 ^ gf_mul(Xi_inv, X[j]))
        numer = _peval(e_eval[::-1], Xi_inv)
        numer = gf_mul(numer, gf_pow(Xi, 1 - FCR))
        if denom == 0:
            raise FecError("Forney denominator zero")
        out[err_pos[i]] ^= gf_div(numer, denom)
    return bytes(out)


def rs_decode(codeword, nparity=RS_DEFAULT_NPARITY, msglen=None):
    """codeword -> (message bytes, n_corrected). Raises FecError if uncorrectable."""
    cw = bytes(codeword)
    if msglen is None:
        msglen = len(cw) - nparity
    synd = _syndromes(cw, nparity)
    if max(synd) == 0:
        return cw[:msglen], 0
    err_loc = _error_locator(synd, nparity)
    if len(err_loc) - 1 > nparity // 2:
        raise FecError("too many errors to correct")
    pos = _error_positions(err_loc[::-1], len(cw))
    fixed = _correct_errata(cw, synd, pos)
    if max(_syndromes(fixed, nparity)) != 0:
        raise FecError("decode failed (residual syndrome)")
    return fixed[:msglen], len(pos)


# ── block interleaver (spreads RF burst errors across codewords) ───────────────
def interleave(codewords):
    """List of equal-length codewords -> column-major byte stream. A burst of <= D
    (= len(codewords)) consecutive bytes then hits <= 1 byte per codeword."""
    if not codewords:
        return b""
    d, n = len(codewords), len(codewords[0])
    if any(len(c) != n for c in codewords):
        raise ValueError("interleave: codewords must be equal length")
    out = bytearray(d * n)
    for r in range(d):
        for c in range(n):
            out[c * d + r] = codewords[r][c]
    return bytes(out)


def deinterleave(stream, depth, n):
    """Inverse of interleave -> list of `depth` codewords of length `n`."""
    if len(stream) != depth * n:
        raise ValueError("deinterleave: length mismatch")
    cws = [bytearray(n) for _ in range(depth)]
    for c in range(n):
        for r in range(depth):
            cws[r][c] = stream[c * depth + r]
    return [bytes(c) for c in cws]
