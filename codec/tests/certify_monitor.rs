// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF-Cue (cue-plane) L2 framing — diffs the Rust implementation
//! against the cross-language golden vectors (Documentation/monitor_vectors.json).

use dcf_wire_codec::monitor::{channel_id, packetize, CueReassembler};
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct MonVectors {
    framing: Vec<FramingCase>,
    reassembly: Vec<ReasmCase>,
}
#[derive(Deserialize)]
struct FramingCase {
    src: u16,
    dst: u16,
    block_seq: u16,
    ts_us: u32,
    block_samples: u8,
    format: u8,
    channels: u8,
    flags: u8,
    payload: String,
    frames: Vec<String>,
}
#[derive(Deserialize)]
struct ReasmCase {
    name: String,
    input_frames: Vec<String>,
    blocks: Vec<BlockCase>,
    lost: Vec<u16>,
}
#[derive(Deserialize)]
struct BlockCase {
    block_seq: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    block_samples: u8,
    format: u8,
    channels: u8,
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
    panic!("{} not found (run python3 python/MCP/gen_monitor_vectors.py)", name);
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
    let v: MonVectors = load("monitor_vectors.json");
    for (i, c) in v.framing.iter().enumerate() {
        let pcm = hex(&c.payload);
        let frames = packetize(&pcm, c.block_seq, c.ts_us, c.src, c.dst, c.block_samples,
                               c.format, c.channels, c.flags)
            .unwrap();
        assert_eq!(frames.len(), c.frames.len(), "framing[{}] frame count", i);
        for (f, exp) in frames.iter().zip(&c.frames) {
            assert_eq!(to_hex(f), exp.to_lowercase(), "framing[{}] frame bytes", i);
        }
    }
    println!("PASS  {} framing cases packetize byte-identically", v.framing.len());
}

#[test]
fn reassembly_matches_golden() {
    let v: MonVectors = load("monitor_vectors.json");
    for c in &v.reassembly {
        let mut r = CueReassembler::new();
        let mut got = Vec::new();
        for h in &c.input_frames {
            let arr: [u8; 17] = hex(h).as_slice().try_into().unwrap();
            if let Some(b) = r.push(&arr) {
                got.push(b);
            }
        }
        assert_eq!(got.len(), c.blocks.len(), "{}: block count", c.name);
        for (p, e) in got.iter().zip(&c.blocks) {
            assert_eq!(p.block_seq, e.block_seq, "{}: block_seq", c.name);
            assert_eq!(p.ts_us, e.ts_us, "{}: ts_us", c.name);
            assert_eq!(p.src, e.src, "{}: src", c.name);
            assert_eq!(p.dst, e.dst, "{}: dst", c.name);
            assert_eq!(p.block_samples, e.block_samples, "{}: block_samples", c.name);
            assert_eq!(p.format, e.format, "{}: format", c.name);
            assert_eq!(p.channels, e.channels, "{}: channels", c.name);
            assert_eq!(p.flags, e.flags, "{}: flags", c.name);
            assert_eq!(to_hex(&p.data), e.payload.to_lowercase(), "{}: pcm bytes", c.name);
        }
        assert_eq!(r.finalize(), c.lost, "{}: lost set", c.name);
    }
    println!("PASS  {} reassembly cases", v.reassembly.len());
}

#[test]
fn channel_rendezvous_anchor() {
    assert_eq!(channel_id("123456789"), 0x29B1);
    println!("PASS  channel_id rendezvous anchor holds");
}
