// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF-Snake record-plane L2 framing, the BEACON grandmaster
//! media clock, and the unwrap_pid timeline primitive — diffs the Rust implementation
//! against the cross-language golden vectors (Documentation/snake_vectors.json).
//! Passing this == byte-agreement with the C and Python references.

use dcf_wire_codec::snake::{
    beacon_packetize, channel_id, pack_clock, packetize, unpack_clock, unwrap_pid, Clock,
    SnakeReassembler, CLOCK_LEN,
};
use dcf_wire_codec::FrameType;
use serde::Deserialize;
use std::path::Path;

// ── JSON shapes (only the fields we assert on) ──────────────────────────────
#[derive(Deserialize)]
struct SnakeVectors {
    framing: Vec<FramingCase>,
    reassembly: Vec<ReasmCase>,
    clock: Vec<ClockCase>,
    unwrap: Vec<UnwrapCase>,
}
#[derive(Deserialize)]
struct FramingCase {
    src: u16,
    dst: u16,
    stream_id: u16,
    ts_us: u32,
    mode_id: u8,
    flags: u8,
    payload: String,
    frames: Vec<String>,
}
#[derive(Deserialize)]
struct ReasmCase {
    name: String,
    input_frames: Vec<String>,
    messages: Vec<MsgCase>,
    lost: Vec<u16>,
}
#[derive(Deserialize)]
struct MsgCase {
    stream_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    mode_id: u8,
    flags: u8,
    payload: String,
}
#[derive(Deserialize)]
struct ClockCase {
    gm_sample_count: u64,
    nominal_rate_mhz: u32,
    tx_seq: u16,
    epoch: u16,
    payload: String,
    beacon_slot: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    frames: Vec<String>,
}
#[derive(Deserialize)]
struct UnwrapCase {
    prev_abs: u64,
    raw: u16,
    #[serde(rename = "mod")]
    modulus: u64,
    expect: u64,
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
    panic!("{} not found (run python3 python/MCP/gen_snake_vectors.py)", name);
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
    let v: SnakeVectors = load("snake_vectors.json");
    for (i, c) in v.framing.iter().enumerate() {
        let payload = hex(&c.payload);
        let frames =
            packetize(&payload, c.stream_id, c.ts_us, c.src, c.dst, c.mode_id, c.flags).unwrap();
        assert_eq!(frames.len(), c.frames.len(), "framing[{}] frame count", i);
        for (f, exp) in frames.iter().zip(&c.frames) {
            assert_eq!(to_hex(f), exp.to_lowercase(), "framing[{}] frame bytes", i);
        }
    }
    println!("PASS  {} framing cases packetize byte-identically", v.framing.len());
}

#[test]
fn reassembly_matches_golden() {
    let v: SnakeVectors = load("snake_vectors.json");
    for c in &v.reassembly {
        let mut r = SnakeReassembler::new();
        let mut got = Vec::new();
        for h in &c.input_frames {
            let arr: [u8; 17] = hex(h).as_slice().try_into().unwrap();
            if let Some(m) = r.push(&arr) {
                got.push(m);
            }
        }
        assert_eq!(got.len(), c.messages.len(), "{}: message count", c.name);
        for (p, e) in got.iter().zip(&c.messages) {
            assert_eq!(p.stream_id, e.stream_id, "{}: stream_id", c.name);
            assert_eq!(p.ts_us, e.ts_us, "{}: ts_us", c.name);
            assert_eq!(p.src, e.src, "{}: src", c.name);
            assert_eq!(p.dst, e.dst, "{}: dst", c.name);
            assert_eq!(p.mode_id, e.mode_id, "{}: mode_id", c.name);
            assert_eq!(p.flags, e.flags, "{}: flags", c.name);
            assert_eq!(to_hex(&p.data), e.payload.to_lowercase(), "{}: message bytes", c.name);
        }
        assert_eq!(r.finalize(), c.lost, "{}: lost set", c.name);
    }
    println!("PASS  {} reassembly cases", v.reassembly.len());
}

#[test]
fn clock_matches_golden() {
    let v: SnakeVectors = load("snake_vectors.json");
    for (i, c) in v.clock.iter().enumerate() {
        let clock = Clock {
            gm_sample_count: c.gm_sample_count,
            nominal_rate_mhz: c.nominal_rate_mhz,
            tx_seq: c.tx_seq,
            epoch: c.epoch,
        };
        let packed = pack_clock(&clock);
        assert_eq!(to_hex(&packed), c.payload.to_lowercase(), "clock[{}] pack bytes", i);
        assert_eq!(unpack_clock(&packed), clock, "clock[{}] unpack roundtrip", i);

        let frames =
            beacon_packetize(&packed, c.beacon_slot, c.ts_us, c.src, c.dst, 0).unwrap();
        assert_eq!(frames.len(), c.frames.len(), "clock[{}] beacon frame count", i);
        for (f, exp) in frames.iter().zip(&c.frames) {
            assert_eq!(to_hex(f), exp.to_lowercase(), "clock[{}] beacon frame bytes", i);
        }

        let mut r = SnakeReassembler::with_type(FrameType::Beacon);
        let mut got = None;
        for f in &frames {
            if let Some(m) = r.push(f) {
                got = Some(m);
            }
        }
        let m = got.expect("beacon reassembles");
        assert_eq!(m.data, packed.to_vec(), "clock[{}] reassembled payload", i);
        let back: [u8; CLOCK_LEN] = m.data.as_slice().try_into().unwrap();
        assert_eq!(unpack_clock(&back), clock, "clock[{}] end-to-end", i);
    }
    println!("PASS  {} clock cases (pack/unpack + BEACON framing)", v.clock.len());
}

#[test]
fn unwrap_pid_matches_golden() {
    let v: SnakeVectors = load("snake_vectors.json");
    for (i, u) in v.unwrap.iter().enumerate() {
        assert_eq!(unwrap_pid(u.prev_abs, u.raw, u.modulus), u.expect, "unwrap[{}]", i);
    }
    println!("PASS  {} unwrap_pid cases", v.unwrap.len());
}

#[test]
fn channel_rendezvous_anchor() {
    assert_eq!(channel_id("123456789"), 0x29B1);
    println!("PASS  channel_id rendezvous anchor holds");
}
