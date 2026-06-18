// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF SuperPack container — diffs the Rust
//! implementation against the cross-language golden vectors
//! (Documentation/superpack_vectors.json). Passing this == byte-agreement with
//! the C, Python, and Go references.

use dcf_wire_codec::superpack::{is_superpack, pack, unpack};
use dcf_wire_codec::{Frame, FrameType};
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct SuperPackVectors {
    super_len: usize,
    anchors: Anchors,
    cases: Vec<Case>,
}
#[derive(Deserialize)]
struct Anchors {
    zero_core_joint_crc: u16,
}
#[derive(Deserialize)]
struct Case {
    a: String,
    b: String,
    #[serde(rename = "super")]
    sp: String,
}

fn load() -> SuperPackVectors {
    let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    for p in [
        format!("{}/../Documentation/superpack_vectors.json", dir),
        format!("{}/../python/MCP/superpack_vectors.json", dir),
    ] {
        if Path::new(&p).exists() {
            let data = std::fs::read_to_string(&p).unwrap();
            return serde_json::from_str(&data).unwrap_or_else(|e| panic!("parse {}: {}", p, e));
        }
    }
    panic!("superpack_vectors.json not found (run python3 python/MCP/gen_superpack_vectors.py)");
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap())
        .collect()
}
fn to_hex(b: &[u8]) -> String {
    b.iter().map(|x| format!("{:02x}", x)).collect()
}

#[test]
fn pack_matches_golden() {
    let v = load();
    for (i, c) in v.cases.iter().enumerate() {
        let a: [u8; 17] = hex(&c.a).try_into().unwrap();
        let b: [u8; 17] = hex(&c.b).try_into().unwrap();
        let sp = pack(&a, &b).expect("pack");
        assert_eq!(sp.len(), v.super_len);
        assert!(is_superpack(&sp), "case[{}] not recognised as SuperPack", i);
        assert_eq!(to_hex(&sp), c.sp.to_lowercase(), "case[{}] pack bytes", i);
    }
    println!("PASS  {} SuperPack pairs pack byte-identically", v.cases.len());
}

#[test]
fn unpack_is_lossless() {
    let v = load();
    for (i, c) in v.cases.iter().enumerate() {
        let sp = hex(&c.sp);
        let (a, b) = unpack(&sp).expect("unpack");
        assert_eq!(to_hex(&a), c.a.to_lowercase(), "case[{}] frame A", i);
        assert_eq!(to_hex(&b), c.b.to_lowercase(), "case[{}] frame B", i);
    }
    println!("PASS  {} SuperPack pairs unpack losslessly", v.cases.len());
}

#[test]
fn joint_crc_is_tamper_evident() {
    let v = load();
    let sp = hex(&v.cases[0].sp);
    for i in 0..sp.len() {
        let mut bad = sp.clone();
        bad[i] ^= 0x01;
        assert!(unpack(&bad).is_err(), "tamper at byte {} not detected", i);
    }
    // zero-core anchor: SuperPack of two all-zero-core frames has joint CRC 0x5B75.
    let zero = Frame::new(1, FrameType::Data, 0, 0, 0, [0, 0, 0, 0], 0).encode();
    let spz = pack(&zero, &zero).unwrap();
    let joint = u16::from(spz[30]) << 8 | u16::from(spz[31]);
    assert_eq!(joint, v.anchors.zero_core_joint_crc);
    assert_eq!(joint, 0x5B75);
    println!("PASS  joint CRC is tamper-evident; zero-core anchor = 0x5B75");
}
