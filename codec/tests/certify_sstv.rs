// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF-SSTV L2 framing — diffs the Rust implementation
//! against the cross-language golden vectors (Documentation/sstv_vectors.json).
//! Passing this == byte-agreement with the C, Python, Go and Node references.

use dcf_wire_codec::sstv::{channel_id, packetize, SstvReassembler};
use serde::Deserialize;
use std::path::Path;

// ── JSON shapes (only the fields we assert on) ──────────────────────────────
#[derive(Deserialize)]
struct SstvVectors {
    framing: Vec<FramingCase>,
    reassembly: Vec<ReasmCase>,
}
#[derive(Deserialize)]
struct FramingCase {
    src: u16,
    dst: u16,
    image_id: u16,
    ts_us: u32,
    format_id: u8,
    flags: u8,
    payload: String,
    frames: Vec<String>,
}
#[derive(Deserialize)]
struct ReasmCase {
    name: String,
    input_frames: Vec<String>,
    images: Vec<ImgCase>,
    lost: Vec<u16>,
}
#[derive(Deserialize)]
struct ImgCase {
    image_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    format_id: u8,
    flags: u8,
    payload: String,
}

fn load<T: for<'de> Deserialize<'de>>(name: &str) -> T {
    let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    for p in [
        format!("{}/../Documentation/{}", dir, name),
        format!("{}/../python/MCP/{}", dir, name),
    ] {
        if Path::new(&p).exists() {
            let data = std::fs::read_to_string(&p).unwrap();
            return serde_json::from_str(&data).unwrap_or_else(|e| panic!("parse {}: {}", p, e));
        }
    }
    panic!("{} not found (run python3 python/MCP/gen_sstv_vectors.py)", name);
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
fn framing_matches_golden() {
    let v: SstvVectors = load("sstv_vectors.json");
    for (i, c) in v.framing.iter().enumerate() {
        let payload = hex(&c.payload);
        let frames =
            packetize(&payload, c.image_id, c.ts_us, c.src, c.dst, c.format_id, c.flags).unwrap();
        assert_eq!(frames.len(), c.frames.len(), "framing[{}] frame count", i);
        for (f, exp) in frames.iter().zip(&c.frames) {
            assert_eq!(to_hex(f), exp.to_lowercase(), "framing[{}] frame bytes", i);
        }
    }
    println!("PASS  {} framing cases packetize byte-identically", v.framing.len());
}

#[test]
fn reassembly_matches_golden() {
    let v: SstvVectors = load("sstv_vectors.json");
    for c in &v.reassembly {
        let mut r = SstvReassembler::new();
        let mut got = Vec::new();
        for h in &c.input_frames {
            let bytes = hex(h);
            let arr: [u8; 17] = bytes.as_slice().try_into().unwrap();
            if let Some(img) = r.push(&arr) {
                got.push(img);
            }
        }
        assert_eq!(got.len(), c.images.len(), "{}: image count", c.name);
        for (p, e) in got.iter().zip(&c.images) {
            assert_eq!(p.image_id, e.image_id, "{}: image_id", c.name);
            assert_eq!(p.ts_us, e.ts_us, "{}: ts_us", c.name);
            assert_eq!(p.src, e.src, "{}: src", c.name);
            assert_eq!(p.dst, e.dst, "{}: dst", c.name);
            assert_eq!(p.format_id, e.format_id, "{}: format_id", c.name);
            assert_eq!(p.flags, e.flags, "{}: flags", c.name);
            assert_eq!(to_hex(&p.data), e.payload.to_lowercase(), "{}: image bytes", c.name);
        }
        assert_eq!(r.finalize(), c.lost, "{}: lost set", c.name);
    }
    println!("PASS  {} reassembly cases", v.reassembly.len());
}

#[test]
fn channel_rendezvous_anchor() {
    // crc16_ccitt("123456789") == 0x29B1 is the repo-wide CRC anchor.
    assert_eq!(channel_id("123456789"), 0x29B1);
    println!("PASS  channel_id rendezvous anchor holds");
}
