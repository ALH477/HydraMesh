// SPDX-License-Identifier: LGPL-3.0-only
//! DCF modulation mapping — the byte↔symbol layer of the Faust-DSP modem,
//! byte-identical to `python/MCP/modulationlab_core.py` and
//! `codec/demod_modulation.h`. This is the **certified** half of the modem: a
//! pure-integer, lossless map from frame bytes to Gray-coded symbol indices and
//! back. The Faust waveform that renders/recovers those symbols is analog and is
//! **not** byte-certified (mirroring DCF-Audio's L2-vs-synthesis split).
//!
//! Rule: MSB-first bits, zero-padded to a multiple of `bits_per_symbol`, each
//! group Gray-coded to a symbol index. `demodulate(modulate(x), x.len()) == x`.

/// Modulation registry (parallels the audio `codec_id` registry).
pub const MOD_FSK: u8 = 0; // binary FSK   (1 bit/symbol)
pub const MOD_OOK: u8 = 1; // on-off keying (1 bit/symbol)
pub const MOD_PSK: u8 = 2; // QPSK, Gray   (2 bits/symbol)
pub const MOD_QAM: u8 = 3; // 16-QAM, Gray (4 bits/symbol)

/// Bits carried per symbol for each `modulation_id`.
pub fn bits_per_symbol(modulation: u8) -> u32 {
    match modulation {
        MOD_FSK | MOD_OOK => 1,
        MOD_PSK => 2,
        MOD_QAM => 4,
        _ => panic!("unknown modulation {modulation}"),
    }
}

/// Binary → reflected Gray code.
pub fn gray(n: u32) -> u32 {
    n ^ (n >> 1)
}

/// Reflected Gray code → binary.
pub fn ungray(mut g: u32) -> u32 {
    let mut n = 0;
    while g != 0 {
        n ^= g;
        g >>= 1;
    }
    n
}

/// Bytes → Gray-coded symbol indices.
pub fn modulate(modulation: u8, data: &[u8]) -> Vec<u8> {
    let bps = bits_per_symbol(modulation);
    let total_bits = data.len() * 8;
    let nsyms = (total_bits + bps as usize - 1) / bps as usize;
    let mut out = Vec::with_capacity(nsyms);
    for s in 0..nsyms {
        let mut val = 0u32;
        for i in 0..bps as usize {
            let bi = s * bps as usize + i;
            let bit = if bi < total_bits {
                ((data[bi / 8] >> (7 - (bi % 8))) & 1) as u32
            } else {
                0
            };
            val = (val << 1) | bit;
        }
        out.push(gray(val) as u8);
    }
    out
}

/// Symbol indices → bytes (first `nbytes` recovered).
pub fn demodulate(modulation: u8, syms: &[u8], nbytes: usize) -> Vec<u8> {
    let bps = bits_per_symbol(modulation) as usize;
    let mut out = Vec::with_capacity(nbytes);
    for b in 0..nbytes {
        let mut byte = 0u32;
        for k in 0..8 {
            let bi = b * 8 + k;
            let s = bi / bps;
            let within = bi % bps;
            let bit = if s < syms.len() {
                (ungray(syms[s] as u32) >> (bps - 1 - within)) & 1
            } else {
                0
            };
            byte = (byte << 1) | bit;
        }
        out.push(byte as u8);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gray_bijection_and_roundtrip() {
        assert_eq!((0..8).map(gray).collect::<Vec<_>>(), vec![0, 1, 3, 2, 6, 7, 5, 4]);
        assert!((0..256).all(|n| ungray(gray(n)) == n));
        let frame: Vec<u8> = (0u16..17).map(|i| i as u8).collect();
        for m in [MOD_FSK, MOD_OOK, MOD_PSK, MOD_QAM] {
            let syms = modulate(m, &frame);
            assert_eq!(demodulate(m, &syms, frame.len()), frame);
        }
    }
}
