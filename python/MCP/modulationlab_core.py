# SPDX-License-Identifier: LGPL-3.0-only
"""DCF modulation mapping — the byte<->symbol layer of the Faust-DSP modem.

This is the *certified* half of the modem (mirroring how DCF-Audio certifies the L2
framing but not the synthesised audio): a pure-integer, lossless mapping from frame
bytes to a stream of Gray-coded symbol indices, and back. The Faust waveform that
renders those symbols onto a carrier (FSK tones, OOK on/off, PSK phase, QAM I/Q) and
recovers them is analog and is NOT byte-certified — only this mapping is.

Rule (identical in C `codec/demod_modulation.h` and Rust `codec/src/modulation.rs`):
  modulate(mod, data):   MSB-first bits, zero-padded to a multiple of bits_per_symbol,
                         each group -> Gray(group_value) = a symbol index.
  demodulate(mod, syms, nbytes): inverse-Gray each symbol -> bits -> bytes[:nbytes].
So demodulate(modulate(data), len(data)) == data for every modulation.
"""

# modulation_id registry (parallels the audio codec_id registry).
MOD_FSK = 0   # Bell-202 binary FSK   (1 bit/symbol)
MOD_OOK = 1   # on-off keying         (1 bit/symbol)
MOD_PSK = 2   # QPSK, Gray-coded      (2 bits/symbol)
MOD_QAM = 3   # 16-QAM, Gray-coded    (4 bits/symbol)

BITS_PER_SYMBOL = {MOD_FSK: 1, MOD_OOK: 1, MOD_PSK: 2, MOD_QAM: 4}
MOD_NAMES = {MOD_FSK: "fsk", MOD_OOK: "ook", MOD_PSK: "psk", MOD_QAM: "qam"}


def gray(n):
    """Binary -> reflected Gray code."""
    return n ^ (n >> 1)


def ungray(g):
    """Reflected Gray code -> binary."""
    n = 0
    while g:
        n ^= g
        g >>= 1
    return n


def modulate(mod, data):
    """bytes -> list of Gray-coded symbol indices (0 .. 2**bits_per_symbol - 1)."""
    bps = BITS_PER_SYMBOL[mod]
    bits = []
    for byte in data:
        for i in range(7, -1, -1):
            bits.append((byte >> i) & 1)
    while len(bits) % bps != 0:
        bits.append(0)
    syms = []
    for off in range(0, len(bits), bps):
        val = 0
        for i in range(bps):
            val = (val << 1) | bits[off + i]
        syms.append(gray(val))
    return syms


def demodulate(mod, syms, nbytes):
    """list of symbol indices -> bytes (the first nbytes recovered)."""
    bps = BITS_PER_SYMBOL[mod]
    bits = []
    for s in syms:
        val = ungray(s)
        for i in range(bps - 1, -1, -1):
            bits.append((val >> i) & 1)
    out = bytearray()
    for off in range(0, nbytes * 8, 8):
        byte = 0
        for i in range(8):
            byte = (byte << 1) | (bits[off + i] if off + i < len(bits) else 0)
        out.append(byte)
    return bytes(out)


# Constellation tables — the bridge to the (uncertified) Faust waveform. Pinned as
# anchors so the geometry is documented and stable, but the certified law is the
# byte<->symbol bijection above, not these points.
def constellation(mod):
    """symbol index -> a carrier descriptor for the waveform layer."""
    if mod == MOD_FSK:
        return {0: {"freq_hz": 2200}, 1: {"freq_hz": 1200}}        # space / mark
    if mod == MOD_OOK:
        return {0: {"amp": 0}, 1: {"amp": 1}}                       # off / on
    if mod == MOD_PSK:
        return {s: {"phase_deg": (ungray(s) * 90) % 360} for s in range(4)}
    if mod == MOD_QAM:
        level = [-3, -1, 3, 1]  # 2-bit Gray order
        out = {}
        for s in range(16):
            v = ungray(s)
            out[s] = {"i": level[(v >> 2) & 3], "q": level[v & 3]}
        return out
    raise ValueError(f"unknown modulation {mod}")


if __name__ == "__main__":
    # gray bijection
    assert [gray(n) for n in range(8)] == [0, 1, 3, 2, 6, 7, 5, 4]
    assert all(ungray(gray(n)) == n for n in range(256))
    # round-trip over every modulation on a representative frame
    sample = bytes.fromhex("d31312340001ffffdeadbeefab12cd24c0")  # the golden exampleFrame
    for mod in (MOD_FSK, MOD_OOK, MOD_PSK, MOD_QAM):
        syms = modulate(mod, sample)
        assert demodulate(mod, syms, len(sample)) == sample, MOD_NAMES[mod]
        assert all(0 <= s < (1 << BITS_PER_SYMBOL[mod]) for s in syms)
    print("modulationlab selftest: CERTIFIED (gray bijection + byte<->symbol round-trip)")
