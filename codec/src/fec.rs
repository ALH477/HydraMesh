// SPDX-License-Identifier: LGPL-3.0-only
//! DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
//! byte-identical to `python/MCP/feclab_core.py` and `codec/demod_fec.h` and pinned
//! by `Documentation/fec_vectors.json`. FEC wraps a 17-byte DeModFrame so a lossy
//! medium (RF/SDR, acoustic) can *correct* it, not just detect damage — the frame
//! and its 246-vector certificate are untouched. The certified half (this); the IQ
//! waveform that carries the bytes is analog/loopback-tested.
//!
//! Field GF(2^8), prim 0x11D, generator α=2, fcr=0. Systematic: codeword =
//! message ++ parity. Default 2t=16 parity bytes → corrects t=8 byte-errors.

use std::sync::OnceLock;

pub const GF_PRIM: u16 = 0x11D;
pub const GF_GEN: u8 = 2;
pub const FCR: u8 = 0;
pub const RS_DEFAULT_NPARITY: usize = 16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FecError;

struct Gf {
    exp: [u8; 512],
    log: [u8; 256],
}

fn gf() -> &'static Gf {
    static G: OnceLock<Gf> = OnceLock::new();
    G.get_or_init(|| {
        let mut exp = [0u8; 512];
        let mut log = [0u8; 256];
        let mut x: u16 = 1;
        for i in 0..255 {
            exp[i] = x as u8;
            log[x as usize] = i as u8;
            x <<= 1;
            if x & 0x100 != 0 {
                x ^= GF_PRIM;
            }
        }
        for i in 255..512 {
            exp[i] = exp[i - 255];
        }
        Gf { exp, log }
    })
}

fn mul(a: u8, b: u8) -> u8 {
    if a == 0 || b == 0 {
        0
    } else {
        let g = gf();
        g.exp[g.log[a as usize] as usize + g.log[b as usize] as usize]
    }
}
fn div(a: u8, b: u8) -> u8 {
    if a == 0 {
        0
    } else {
        let g = gf();
        g.exp[(g.log[a as usize] as usize + 255 - g.log[b as usize] as usize) % 255]
    }
}
fn pow(a: u8, p: i32) -> u8 {
    let g = gf();
    let e = (((g.log[a as usize] as i32 * p) % 255) + 255) % 255;
    g.exp[e as usize]
}
fn inv(a: u8) -> u8 {
    let g = gf();
    g.exp[255 - g.log[a as usize] as usize]
}

// ── GF polynomials (high-order coefficient first) ────────────────────────────
fn pmul(p: &[u8], q: &[u8]) -> Vec<u8> {
    let mut r = vec![0u8; p.len() + q.len() - 1];
    for (j, &qj) in q.iter().enumerate() {
        for (i, &pi) in p.iter().enumerate() {
            r[i + j] ^= mul(pi, qj);
        }
    }
    r
}
fn padd(p: &[u8], q: &[u8]) -> Vec<u8> {
    let n = p.len().max(q.len());
    let mut r = vec![0u8; n];
    for (i, &c) in p.iter().enumerate() {
        r[i + n - p.len()] = c;
    }
    for (i, &c) in q.iter().enumerate() {
        r[i + n - q.len()] ^= c;
    }
    r
}
fn peval(p: &[u8], x: u8) -> u8 {
    let mut y = p[0];
    for &c in &p[1..] {
        y = mul(y, x) ^ c;
    }
    y
}

pub fn rs_generator_poly(nparity: usize) -> Vec<u8> {
    let mut g = vec![1u8];
    for i in 0..nparity {
        g = pmul(&g, &[1, pow(GF_GEN, FCR as i32 + i as i32)]);
    }
    g
}

/// message → codeword (message ++ `nparity` parity bytes), systematic.
pub fn rs_encode(msg: &[u8], nparity: usize) -> Vec<u8> {
    assert!(msg.len() + nparity <= 255, "RS(255) overflow");
    let gen = rs_generator_poly(nparity);
    let mut out = vec![0u8; msg.len() + nparity];
    out[..msg.len()].copy_from_slice(msg);
    for i in 0..msg.len() {
        let coef = out[i];
        if coef != 0 {
            for j in 1..gen.len() {
                out[i + j] ^= mul(gen[j], coef);
            }
        }
    }
    let mut code = msg.to_vec();
    code.extend_from_slice(&out[msg.len()..]);
    code
}

fn syndromes(cw: &[u8], nparity: usize) -> Vec<u8> {
    let mut s = vec![0u8]; // leading 0 (Forney convention)
    for i in 0..nparity {
        s.push(peval(cw, pow(GF_GEN, FCR as i32 + i as i32)));
    }
    s
}

fn err_locator(synd: &[u8], nparity: usize) -> Vec<u8> {
    let mut err_loc = vec![1u8];
    let mut old_loc = vec![1u8];
    for i in 0..nparity {
        let mut delta = synd[i + 1];
        for j in 1..err_loc.len() {
            delta ^= mul(err_loc[err_loc.len() - 1 - j], synd[i + 1 - j]);
        }
        old_loc.push(0);
        if delta != 0 {
            if old_loc.len() > err_loc.len() {
                let new_loc: Vec<u8> = old_loc.iter().map(|&c| mul(c, delta)).collect();
                let dinv = inv(delta);
                old_loc = err_loc.iter().map(|&c| mul(c, dinv)).collect();
                err_loc = new_loc;
            }
            let scaled: Vec<u8> = old_loc.iter().map(|&c| mul(c, delta)).collect();
            err_loc = padd(&err_loc, &scaled);
        }
    }
    while !err_loc.is_empty() && err_loc[0] == 0 {
        err_loc.remove(0);
    }
    err_loc
}

fn err_positions(err_loc_rev: &[u8], nmess: usize) -> Option<Vec<usize>> {
    let errs = err_loc_rev.len() - 1;
    let mut pos = Vec::new();
    for i in 0..nmess {
        if peval(err_loc_rev, pow(GF_GEN, i as i32)) == 0 {
            pos.push(nmess - 1 - i);
        }
    }
    if pos.len() == errs {
        Some(pos)
    } else {
        None
    }
}

fn correct(cw: &mut [u8], synd: &[u8], err_pos: &[usize]) -> bool {
    let n = cw.len();
    let coef_pos: Vec<usize> = err_pos.iter().map(|&p| n - 1 - p).collect();
    let mut e_loc = vec![1u8];
    let mut x = Vec::new();
    for &p in &coef_pos {
        e_loc = pmul(&e_loc, &[pow(GF_GEN, p as i32), 1]);
        x.push(pow(GF_GEN, p as i32));
    }
    let synd_rev: Vec<u8> = synd.iter().rev().copied().collect();
    let prod = pmul(&synd_rev, &e_loc);
    let remlen = e_loc.len();
    let rem = &prod[prod.len() - remlen..];
    for i in 0..x.len() {
        let xi = x[i];
        let xi_inv = inv(xi);
        let mut denom = 1u8;
        for j in 0..x.len() {
            if j != i {
                denom = mul(denom, 1 ^ mul(xi_inv, x[j]));
            }
        }
        let mut numer = peval(rem, xi_inv);
        numer = mul(numer, pow(xi, 1 - FCR as i32));
        if denom == 0 {
            return false;
        }
        cw[err_pos[i]] ^= div(numer, denom);
    }
    true
}

/// codeword → (message, n_corrected) or `FecError` if uncorrectable.
pub fn rs_decode(codeword: &[u8], nparity: usize, msglen: Option<usize>) -> Result<(Vec<u8>, usize), FecError> {
    let mut cw = codeword.to_vec();
    let mlen = msglen.unwrap_or(cw.len() - nparity);
    let synd = syndromes(&cw, nparity);
    if synd.iter().all(|&s| s == 0) {
        return Ok((cw[..mlen].to_vec(), 0));
    }
    let err_loc = err_locator(&synd, nparity);
    if err_loc.len() - 1 > nparity / 2 {
        return Err(FecError);
    }
    let err_loc_rev: Vec<u8> = err_loc.iter().rev().copied().collect();
    let pos = err_positions(&err_loc_rev, cw.len()).ok_or(FecError)?;
    if !correct(&mut cw, &synd, &pos) {
        return Err(FecError);
    }
    if !syndromes(&cw, nparity).iter().all(|&s| s == 0) {
        return Err(FecError);
    }
    Ok((cw[..mlen].to_vec(), pos.len()))
}

// ── multi-codeword messages (any length: chunk + interleave + protected header) ─
pub const HDR_PARITY: usize = 16;
pub const HDR_LEN: usize = 5 + HDR_PARITY; // 21-byte self-protecting header

fn chunking(l: usize, np: usize) -> (usize, usize) {
    let maxk = 255 - np;
    let nchunks = if l == 0 { 1 } else { l.div_ceil(maxk) };
    let k = if l == 0 { 1 } else { l.div_ceil(nchunks) };
    (nchunks, k)
}

/// Any-length message → a self-describing RS+interleave blob (corrects bursts).
pub fn encode_message(msg: &[u8], nparity: usize) -> Vec<u8> {
    let l = msg.len();
    let (nchunks, k) = chunking(l, nparity);
    let hdr = [(l >> 24) as u8, (l >> 16) as u8, (l >> 8) as u8, l as u8, nparity as u8];
    let mut out = rs_encode(&hdr, HDR_PARITY);
    let cws: Vec<Vec<u8>> = (0..nchunks)
        .map(|c| {
            let mut block = vec![0u8; k];
            for (i, b) in block.iter_mut().enumerate() {
                let o = c * k + i;
                if o < l {
                    *b = msg[o];
                }
            }
            rs_encode(&block, nparity)
        })
        .collect();
    out.extend_from_slice(&interleave(&cws));
    out
}

/// Inverse of `encode_message` → (message, total_corrected) or `FecError`.
pub fn decode_message(blob: &[u8]) -> Result<(Vec<u8>, usize), FecError> {
    if blob.len() < HDR_LEN {
        return Err(FecError);
    }
    let (hdr, _) = rs_decode(&blob[..HDR_LEN], HDR_PARITY, Some(5))?;
    let l = ((hdr[0] as usize) << 24) | ((hdr[1] as usize) << 16)
        | ((hdr[2] as usize) << 8) | (hdr[3] as usize);
    let np = hdr[4] as usize;
    let (nchunks, k) = chunking(l, np);
    let cwlen = k + np;
    let body = &blob[HDR_LEN..];
    if body.len() != nchunks * cwlen {
        return Err(FecError);
    }
    let mut out = Vec::new();
    let mut total = 0;
    for cw in deinterleave(body, nchunks, cwlen) {
        let (block, n) = rs_decode(&cw, np, Some(k))?;
        total += n;
        out.extend_from_slice(&block);
    }
    out.truncate(l);
    Ok((out, total))
}

// ── block interleaver ────────────────────────────────────────────────────────
pub fn interleave(codewords: &[Vec<u8>]) -> Vec<u8> {
    if codewords.is_empty() {
        return Vec::new();
    }
    let d = codewords.len();
    let n = codewords[0].len();
    let mut out = vec![0u8; d * n];
    for (r, cw) in codewords.iter().enumerate() {
        for c in 0..n {
            out[c * d + r] = cw[c];
        }
    }
    out
}
pub fn deinterleave(stream: &[u8], depth: usize, n: usize) -> Vec<Vec<u8>> {
    let mut cws = vec![vec![0u8; n]; depth];
    for c in 0..n {
        for r in 0..depth {
            cws[r][c] = stream[c * depth + r];
        }
    }
    cws
}
