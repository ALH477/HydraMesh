// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
//! Certifies the bridge's datagram gate. The gate is Exsecutor's `custos`,
//! compiled to C (`custos/`). It is held to two independent things:
//!
//! 1. **Punctim's committed vectors.** Every 17- or 32-byte hex string in
//!    `golden_vectors.json`, `superpack_vectors.json` and
//!    `medium_vectors.json`, judged as one bare datagram.
//! 2. **The Rust reference codec** (`dcf-wire-codec`). The bare-dialect rule
//!    is `medium::bare_decode` followed by `medium::gate` on every frame it
//!    yields: a datagram is admitted iff that yields at least one frame and
//!    every frame passes. The gate must agree with that rule on every input
//!    below.
//!
//! The inputs are the vectors; eight single-bit flips of each; random bytes at
//! every length from 0 to 40; random valid frames of all sixteen types and
//! their SuperPacks; SuperPacks whose joint CRC holds but one core's version
//! is not 1; and SuperPacks whose sflags type is not 5. The verdict anchors
//! check the reason codes as well.
//!
//! Each class is asserted non-empty and to contain both outcomes where it
//! should. A sweep that saw nothing, or only refusals, is a failure.

use dcf_wire_codec::medium;
use dcf_ws_bridge::gate::{admit, Verdict};
use serde_json::Value;

/// The reference rule, from the Rust codec rather than from the gate.
fn reference(d: &[u8]) -> bool {
    let frames = medium::bare_decode(d);
    !frames.is_empty() && frames.iter().all(|f| medium::gate(f))
}

/// splitmix64: deterministic, dependency-free.
struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }
    fn byte(&mut self) -> u8 {
        self.next() as u8
    }
    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }
}

fn hex(s: &str) -> Option<Vec<u8>> {
    if !s.len().is_multiple_of(2) || !s.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).ok())
        .collect()
}

fn walk(v: &Value, out: &mut Vec<Vec<u8>>) {
    match v {
        Value::String(s) => {
            if let Some(b) = hex(s) {
                if b.len() == 17 || b.len() == 32 {
                    out.push(b);
                }
            }
        }
        Value::Array(a) => a.iter().for_each(|x| walk(x, out)),
        Value::Object(o) => o.values().for_each(|x| walk(x, out)),
        _ => {}
    }
}

fn vectors() -> Vec<Vec<u8>> {
    let doc = concat!(env!("CARGO_MANIFEST_DIR"), "/../../Documentation/");
    let mut out = Vec::new();
    for f in ["golden_vectors.json", "superpack_vectors.json", "medium_vectors.json"] {
        let text = std::fs::read_to_string(format!("{doc}{f}"))
            .unwrap_or_else(|e| panic!("{f}: {e}"));
        let json: Value = serde_json::from_str(&text).unwrap_or_else(|e| panic!("{f}: {e}"));
        let before = out.len();
        walk(&json, &mut out);
        assert!(out.len() > before, "{f}: no 17- or 32-byte vectors found");
    }
    out
}

/// A valid frame of type `ty` with random fields.
fn frame(rng: &mut Rng, ty: u8) -> [u8; 17] {
    let mut f = [0u8; 17];
    f[0] = 0xd3;
    f[1] = 0x10 | (ty & 0x0f);
    for b in &mut f[2..15] {
        *b = rng.byte();
    }
    let c = dcf_wire_codec::crc16_ccitt(&f[..15]);
    f[15..].copy_from_slice(&c.to_be_bytes());
    f
}

fn reseal(sp: &mut [u8; 32]) {
    let c = dcf_wire_codec::crc16_ccitt(&sp[..30]);
    sp[30..].copy_from_slice(&c.to_be_bytes());
}

/// Run one class through gate and reference; return (cases, admitted).
fn agree(class: &str, cases: &[Vec<u8>]) -> (usize, usize) {
    assert!(!cases.is_empty(), "{class}: no cases");
    let mut admitted = 0;
    for d in cases {
        let g = admit(d).admitted();
        assert_eq!(
            g,
            reference(d),
            "{class}: gate and reference disagree on {:02x?} (gate verdict {:?})",
            d,
            admit(d)
        );
        admitted += g as usize;
    }
    (cases.len(), admitted)
}

#[test]
fn gate_agrees_with_the_reference_codec_everywhere() {
    let mut rng = Rng(0x0dcf_c057_0d05_0001);
    let vecs = vectors();

    let (n, a) = agree("vectors", &vecs);
    assert!(a > 0 && a < n, "vectors: {a}/{n} admitted -- expected both outcomes");

    let mut flips = Vec::new();
    for v in &vecs {
        for _ in 0..8 {
            let mut x = v.clone();
            let i = rng.below(x.len() as u64) as usize;
            x[i] ^= 1 << rng.below(8);
            flips.push(x);
        }
    }
    let (n, a) = agree("single-bit flips", &flips);
    assert!(a < n, "flips: every one admitted");

    let mut noise = Vec::new();
    for len in 0..=40 {
        for _ in 0..64 {
            noise.push((0..len).map(|_| rng.byte()).collect::<Vec<u8>>());
        }
    }
    agree("random bytes, lengths 0..=40", &noise);

    let mut valid = Vec::new();
    for i in 0..4096 {
        let a = frame(&mut rng, (i % 16) as u8);
        let tb = rng.below(16) as u8;
        let b = frame(&mut rng, tb);
        valid.push(a.to_vec());
        valid.push(dcf_wire_codec::superpack::pack(&a, &b).expect("pack").to_vec());
    }
    let (n, a) = agree("valid frames and SuperPacks, all 16 types", &valid);
    assert_eq!(a, n, "a valid frame or SuperPack was refused");

    let mut bad_core = Vec::new();
    for i in 0..1024 {
        let a = frame(&mut rng, 0);
        let b = frame(&mut rng, 3);
        let mut sp = dcf_wire_codec::superpack::pack(&a, &b).expect("pack");
        let k = if i % 2 == 0 { 2 } else { 16 };
        let v = [0u8, 2, 3, 7, 15][i % 5];
        sp[k] = (v << 4) | (sp[k] & 0x0f);
        reseal(&mut sp);
        bad_core.push(sp.to_vec());
    }
    let (_, a) = agree("SuperPacks with a bad core version", &bad_core);
    assert_eq!(a, 0, "a SuperPack with a core of version != 1 was admitted");

    let mut bad_type = Vec::new();
    for t in (0u8..16).filter(|&t| t != 5) {
        let a = frame(&mut rng, 0);
        let b = frame(&mut rng, 0);
        let mut sp = dcf_wire_codec::superpack::pack(&a, &b).expect("pack");
        sp[1] = 0x10 | t;
        reseal(&mut sp);
        bad_type.push(sp.to_vec());
    }
    let (_, a) = agree("32 bytes, sflags type not 5", &bad_type);
    assert_eq!(a, 0, "32 bytes that are not a SuperPack were admitted");
}

#[test]
fn verdict_anchors() {
    let h = |s: &str| hex(s).unwrap();
    // SUPERPACK_SPEC.md's anchor: two zero-payload DATA frames, joint CRC 0x5B75.
    let filler = "d310000000000000000000000000005b80";
    let pair = "d315100000000000000000000000000010000000000000000000000000005b75";
    assert_eq!(admit(&h(filler)), Verdict::Admitted);
    assert_eq!(admit(&h(pair)), Verdict::Admitted);
    assert_eq!(admit(&h("d210000000000000000000000000005b80")), Verdict::BadSync);
    assert_eq!(admit(&h("d320000000000000000000000000005b80")), Verdict::BadVersion);
    assert_eq!(admit(&h("d310000000000000000000000000005b81")), Verdict::BadCrc);
    assert_eq!(
        admit(&h("d314100000000000000000000000000010000000000000000000000000005b75")),
        Verdict::NotSuperPack
    );
    assert_eq!(
        admit(&h("d31510000000000000000000000000002000000000000000000000000000004d")),
        Verdict::BadCoreVersion
    );
    assert_eq!(admit(&[]), Verdict::BadLength);
    assert_eq!(admit(&h(&format!("{filler}00"))), Verdict::BadLength);
    // Longer than the gate's 32-byte window: refused on length, nothing past
    // the window is read.
    assert_eq!(admit(&[0xd3; 2048]), Verdict::BadLength);
    assert_eq!(admit(&h(&format!("{pair}{pair}"))), Verdict::BadLength);
}

#[test]
fn the_committed_unit_is_the_one_provenance_names() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/custos/");
    let prov = std::fs::read_to_string(format!("{dir}PROVENANCE.md")).unwrap();
    let unit = std::fs::read(format!("{dir}custos.gen.c")).unwrap();
    let digits = unit.len().to_string();
    let mut grouped = String::new();
    for (i, c) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i) % 3 == 0 {
            grouped.push(',');
        }
        grouped.push(c);
    }
    let size = format!("{grouped} bytes");
    assert!(
        prov.contains(&size),
        "custos.gen.c is {} bytes; PROVENANCE.md does not say so -- regenerate with regen.sh and update it",
        unit.len()
    );
    assert!(
        std::str::from_utf8(&unit).unwrap().starts_with("#define EXS_NEED_FLOAT 0\n"),
        "custos.gen.c is not an integer-only exsc unit"
    );
}
