// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF FEC adapter — diffs the Rust Reed-Solomon against
//! the cross-language golden vectors (Documentation/fec_vectors.json). Passing this
//! == byte-agreement with the Python + C references (encode) and the correction law.

use dcf_wire_codec::fec::{deinterleave, interleave, rs_decode, rs_encode, RS_DEFAULT_NPARITY};
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct Vectors {
    nparity: usize,
    cases: Vec<Case>,
    correct: Vec<Fix>,
}
#[derive(Deserialize)]
struct Case {
    msg: String,
    code: String,
}
#[derive(Deserialize)]
struct Fix {
    corrupt: String,
    msg: String,
    nerr: usize,
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len()).step_by(2).map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap()).collect()
}

fn load() -> Vectors {
    let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    for p in [
        format!("{}/../Documentation/fec_vectors.json", dir),
        format!("{}/../python/MCP/fec_vectors.json", dir),
    ] {
        if Path::new(&p).exists() {
            let data = std::fs::read_to_string(&p).unwrap();
            return serde_json::from_str(&data).unwrap_or_else(|e| panic!("parse {}: {}", p, e));
        }
    }
    panic!("fec_vectors.json not found");
}

#[test]
fn encode_matches_golden() {
    let v = load();
    for (i, c) in v.cases.iter().enumerate() {
        let msg = hex(&c.msg);
        let code = rs_encode(&msg, v.nparity);
        assert_eq!(code, hex(&c.code), "encode mismatch case {i}");
        assert_eq!(&code[..msg.len()], &msg[..], "not systematic case {i}");
    }
}

#[test]
fn decode_corrects_golden() {
    let v = load();
    for (i, f) in v.correct.iter().enumerate() {
        let (msg, n) = rs_decode(&hex(&f.corrupt), v.nparity, Some(17)).expect("decode failed");
        assert_eq!(msg, hex(&f.msg), "decode did not recover case {i}");
        assert_eq!(n, f.nerr, "wrong correction count case {i}");
    }
}

#[test]
fn correction_property() {
    let np = RS_DEFAULT_NPARITY;
    let frame: Vec<u8> = (0..17u8).collect();
    let code = rs_encode(&frame, np);
    // clean
    assert_eq!(rs_decode(&code, np, Some(17)).unwrap(), (frame.clone(), 0));
    // corrupt t = 8 bytes -> recover
    let mut bad = code.clone();
    for k in 0..8 {
        bad[k * 3] ^= 0x9C;
    }
    assert_eq!(rs_decode(&bad, np, Some(17)).unwrap().0, frame);
    // t+1 = 9 -> must error (or be caught), never silently equal-but-wrong
    let mut over = code.clone();
    for k in 0..9 {
        over[k * 2] ^= 0x33;
    }
    assert!(rs_decode(&over, np, Some(17)).is_err());
}

#[test]
fn interleave_roundtrip() {
    let np = RS_DEFAULT_NPARITY;
    let cws: Vec<Vec<u8>> = (0..5u8).map(|i| rs_encode(&[i; 17], np)).collect();
    let stream = interleave(&cws);
    assert_eq!(stream.len(), 5 * (17 + np));
    let back = deinterleave(&stream, 5, 17 + np);
    assert_eq!(back, cws);
}
