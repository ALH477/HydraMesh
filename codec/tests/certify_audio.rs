//! Rust certification for the DCF-Audio L2 framing — diffs the Rust implementation
//! against the cross-language golden vectors (Documentation/audio_vectors.json and
//! pm_param_vectors.json). Passing this == byte-agreement with the C and Python refs.

use dcf_wire_codec::audio::{
    packetize, pcm_diag_decode, pcm_diag_encode, pm_pack, pm_unpack, AudioReassembler, PmParams,
};
use serde::Deserialize;
use std::path::Path;

// ── JSON shapes (only the fields we assert on) ──────────────────────────────
#[derive(Deserialize)]
struct AudioVectors {
    framing: Vec<FramingCase>,
    reassembly: Vec<ReasmCase>,
    pcm_roundtrip: Vec<PcmCase>,
}
#[derive(Deserialize)]
struct FramingCase {
    codec_id: u8,
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
    codec_id: u8,
    payload: String,
    flags: u8,
}
#[derive(Deserialize)]
struct PcmCase {
    bytes: String,
}
#[derive(Deserialize)]
struct PmVectors {
    cases: Vec<PmCase>,
}
#[derive(Deserialize)]
struct PmCase {
    params: PmFields,
    bytes: String,
}
#[derive(Deserialize)]
struct PmFields {
    f0: u16,
    amp: u8,
    mod_index: u8,
    mod_ratio: u8,
    bright: u8,
    env: u8,
    flags: u8,
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
    panic!("{} not found (run python3 python/MCP/gen_audio_vectors.py)", name);
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
    let v: AudioVectors = load("audio_vectors.json");
    for (i, c) in v.framing.iter().enumerate() {
        let payload = hex(&c.payload);
        let frames =
            packetize(c.codec_id, &payload, c.packet_id, c.ts_us, c.src, c.dst, c.flags).unwrap();
        assert_eq!(frames.len(), c.frames.len(), "framing[{}] frame count", i);
        for (f, exp) in frames.iter().zip(&c.frames) {
            assert_eq!(to_hex(f), exp.to_lowercase(), "framing[{}] frame bytes", i);
        }
    }
    println!("PASS  {} framing cases packetize byte-identically", v.framing.len());
}

#[test]
fn reassembly_matches_golden() {
    let v: AudioVectors = load("audio_vectors.json");
    for c in &v.reassembly {
        let mut r = AudioReassembler::new();
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
            assert_eq!(p.codec_id, e.codec_id, "{}: codec_id", c.name);
            assert_eq!(p.flags, e.flags, "{}: flags", c.name);
            assert_eq!(to_hex(&p.payload), e.payload.to_lowercase(), "{}: payload", c.name);
        }
        assert_eq!(r.finalize(), c.lost, "{}: lost set", c.name);
    }
    println!("PASS  {} reassembly cases", v.reassembly.len());
}

#[test]
fn pcm_diag_roundtrip() {
    let v: AudioVectors = load("audio_vectors.json");
    for (i, c) in v.pcm_roundtrip.iter().enumerate() {
        let bytes = hex(&c.bytes);
        let back = pcm_diag_encode(&pcm_diag_decode(&bytes));
        assert_eq!(back, bytes, "pcm_roundtrip[{}]", i);
    }
    println!("PASS  {} PCM-diag blocks round-trip losslessly", v.pcm_roundtrip.len());
}

#[test]
fn pm_param_roundtrip() {
    let v: PmVectors = load("pm_param_vectors.json");
    for (i, c) in v.cases.iter().enumerate() {
        let p = PmParams {
            f0: c.params.f0,
            amp: c.params.amp,
            mod_index: c.params.mod_index,
            mod_ratio: c.params.mod_ratio,
            bright: c.params.bright,
            env: c.params.env,
            flags: c.params.flags,
        };
        let packed = pm_pack(&p);
        assert_eq!(to_hex(&packed), c.bytes.to_lowercase(), "pm[{}] pack", i);
        assert_eq!(pm_unpack(&hex(&c.bytes).as_slice().try_into().unwrap()), p, "pm[{}] unpack", i);
    }
    println!("PASS  {} PM param blocks pack/unpack identically", v.cases.len());
}
