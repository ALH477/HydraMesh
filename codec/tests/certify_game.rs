// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF-Game L2 framing — diffs the Rust implementation
//! against the cross-language golden vectors (Documentation/game_vectors.json).
//! Passing this == byte-agreement with the C and Python references.

use dcf_wire_codec::game::{
    input_pack, input_unpack, join_pack, join_unpack, packetize, snapshot_pack, snapshot_unpack,
    GameReassembler, Input, Snapshot, INPUT_LEN, SNAPSHOT_LEN,
};
use serde::Deserialize;
use std::path::Path;

// ── JSON shapes (only the fields we assert on) ──────────────────────────────
#[derive(Deserialize)]
struct GameVectors {
    framing: Vec<FramingCase>,
    reassembly: Vec<ReasmCase>,
    snapshot_roundtrip: Vec<ByteCase>,
    input_roundtrip: Vec<ByteCase>,
    join_roundtrip: Vec<JoinCase>,
}
#[derive(Deserialize)]
struct FramingCase {
    msg_type_id: u8,
    src: u16,
    dst: u16,
    packet_id: u16,
    ts_us: u32,
    flags: u8,
    payload: String,
    frames: Vec<String>,
}
#[derive(Deserialize)]
struct ReasmCase {
    name: String,
    input_frames: Vec<String>,
    packets: Vec<PktCase>,
    lost: Vec<u16>,
}
#[derive(Deserialize)]
struct PktCase {
    packet_id: u16,
    ts_us: u32,
    msg_type_id: u8,
    payload: String,
    flags: u8,
}
#[derive(Deserialize)]
struct ByteCase {
    bytes: String,
}
#[derive(Deserialize)]
struct JoinCase {
    fields: JoinFields,
    bytes: String,
}
#[derive(Deserialize)]
struct JoinFields {
    player_id: u16,
    name: String,
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
    panic!("{} not found (run python3 python/MCP/gen_game_vectors.py)", name);
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
    let v: GameVectors = load("game_vectors.json");
    for (i, c) in v.framing.iter().enumerate() {
        let payload = hex(&c.payload);
        let frames =
            packetize(c.msg_type_id, &payload, c.packet_id, c.ts_us, c.src, c.dst, c.flags).unwrap();
        assert_eq!(frames.len(), c.frames.len(), "framing[{}] frame count", i);
        for (f, exp) in frames.iter().zip(&c.frames) {
            assert_eq!(to_hex(f), exp.to_lowercase(), "framing[{}] frame bytes", i);
        }
    }
    println!("PASS  {} framing cases packetize byte-identically", v.framing.len());
}

#[test]
fn reassembly_matches_golden() {
    let v: GameVectors = load("game_vectors.json");
    for c in &v.reassembly {
        let mut r = GameReassembler::new();
        let mut got = Vec::new();
        for h in &c.input_frames {
            let bytes = hex(h);
            let arr: [u8; 17] = bytes.as_slice().try_into().unwrap();
            if let Some(pkt) = r.push(&arr) {
                got.push(pkt);
            }
        }
        assert_eq!(got.len(), c.packets.len(), "{}: packet count", c.name);
        for (p, e) in got.iter().zip(&c.packets) {
            assert_eq!(p.packet_id, e.packet_id, "{}: packet_id", c.name);
            assert_eq!(p.ts_us, e.ts_us, "{}: ts_us", c.name);
            assert_eq!(p.msg_type_id, e.msg_type_id, "{}: msg_type_id", c.name);
            assert_eq!(p.flags, e.flags, "{}: flags", c.name);
            assert_eq!(to_hex(&p.payload), e.payload.to_lowercase(), "{}: payload", c.name);
        }
        assert_eq!(r.finalize(), c.lost, "{}: lost set", c.name);
    }
    println!("PASS  {} reassembly cases", v.reassembly.len());
}

#[test]
fn snapshot_input_roundtrip() {
    let v: GameVectors = load("game_vectors.json");
    for (i, c) in v.snapshot_roundtrip.iter().enumerate() {
        let bytes = hex(&c.bytes);
        let arr: [u8; SNAPSHOT_LEN] = bytes.as_slice().try_into().unwrap();
        let back = snapshot_pack(&snapshot_unpack(&arr));
        assert_eq!(to_hex(&back), c.bytes.to_lowercase(), "snapshot[{}]", i);
    }
    for (i, c) in v.input_roundtrip.iter().enumerate() {
        let bytes = hex(&c.bytes);
        let arr: [u8; INPUT_LEN] = bytes.as_slice().try_into().unwrap();
        let back = input_pack(&input_unpack(&arr));
        assert_eq!(to_hex(&back), c.bytes.to_lowercase(), "input[{}]", i);
    }
    // sanity: types are constructible/usable
    let _ = Snapshot::default();
    let _ = Input::default();
    println!(
        "PASS  {} snapshot + {} input bodies round-trip losslessly",
        v.snapshot_roundtrip.len(),
        v.input_roundtrip.len()
    );
}

#[test]
fn join_roundtrip() {
    let v: GameVectors = load("game_vectors.json");
    for (i, c) in v.join_roundtrip.iter().enumerate() {
        let packed = join_pack(c.fields.player_id, &c.fields.name);
        assert_eq!(to_hex(&packed), c.bytes.to_lowercase(), "join[{}] pack", i);
        let (pid, name) = join_unpack(&hex(&c.bytes)).unwrap();
        assert_eq!(pid, c.fields.player_id, "join[{}] player_id", i);
        assert_eq!(name, c.fields.name, "join[{}] name", i);
    }
    println!("PASS  {} JOIN bodies pack/unpack identically", v.join_roundtrip.len());
}
