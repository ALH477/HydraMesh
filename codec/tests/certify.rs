use dcf_wire_codec::{
    crc16_ccitt, Frame, FrameError, FrameType,
    BROADCAST, FRAME_SIZE,
};
use serde::Deserialize;
use std::path::Path;

const CRC_COVER: usize = 15;

#[derive(Deserialize)]
struct GoldenVectors {
    anchors: Anchors,
    encode_basis: Vec<EncodeVec>,
    syndrome_basis: Vec<SyndromeVec>,
}

#[derive(Deserialize)]
struct Anchors {
    #[serde(rename = "crc_123456789")]
    crc_123456789: String,
    #[serde(rename = "crc_zero15")]
    crc_zero15: String,
    #[serde(rename = "exampleFrame_full")]
    example_frame_full: String,
}

#[derive(Deserialize)]
struct EncodeVec {
    #[serde(default)]
    input_bits: Option<String>,
    #[serde(default)]
    input_bit: Option<u32>,
    frame: String,
}

#[derive(Deserialize)]
struct SyndromeVec {
    #[serde(default)]
    word: Option<String>,
    #[serde(default)]
    bit: Option<u32>,
    syndrome: u64,
}

fn load_golden() -> GoldenVectors {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let paths = [
        format!("{}/../Documentation/golden_vectors.json", manifest_dir),
        format!("{}/../python/MCP/golden_vectors.json", manifest_dir),
    ];
    for p in &paths {
        let path = Path::new(p);
        if path.exists() {
            let data = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("Failed to read {}: {}", p, e));
            return serde_json::from_str(&data)
                .unwrap_or_else(|e| panic!("Failed to parse {}: {}", p, e));
        }
    }
    panic!("golden_vectors.json not found in expected locations");
}

fn hex_to_bytes(hex: &str) -> Vec<u8> {
    let hex = hex.trim();
    (0..hex.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
        .collect()
}

fn to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02X}", b)).collect::<Vec<_>>().join("")
}

fn syndrome(word: &[u8]) -> u16 {
    assert_eq!(word.len(), FRAME_SIZE);
    let crc_calc = crc16_ccitt(&word[..CRC_COVER]);
    let crc_stored = ((word[15] as u16) << 8) | word[16] as u16;
    crc_calc ^ crc_stored
}

#[test]
fn test_crc_anchor_123456789() {
    let c = crc16_ccitt(b"123456789");
    assert_eq!(c, 0x29B1, "CRC('123456789') must be 0x29B1, got 0x{:04X}", c);
}

#[test]
fn test_crc_anchor_zero15() {
    let c = crc16_ccitt(&[0u8; 15]);
    assert_eq!(c, 0x4EC3, "CRC(0^15) must be 0x4EC3, got 0x{:04X}", c);
}

fn raw_crc_valid(bytes: &[u8]) -> bool {
    if bytes.len() != FRAME_SIZE { return false; }
    if bytes[0] != dcf_wire_codec::SYNC_BYTE { return false; }
    let crc_calc = crc16_ccitt(&bytes[..CRC_COVER]);
    let crc_stored = ((bytes[15] as u16) << 8) | bytes[16] as u16;
    crc_calc == crc_stored
}

#[test]
fn test_example_frame_anchor() {
    let golden = load_golden();
    let expected = hex_to_bytes(&golden.anchors.example_frame_full.to_lowercase());
    let frame = Frame::new(
        1,
        FrameType::Ctrl,
        0x1234,
        0x0001,
        BROADCAST,
        [0xDE, 0xAD, 0xBE, 0xEF],
        0xAB12CD,
    );
    let wire = frame.encode();
    assert_eq!(wire.as_slice(), expected.as_slice(),
        "exampleFrame anchor mismatch:\n  got    {}\n  expect {}",
        to_hex(&wire), to_hex(&expected));
}

#[test]
fn test_encode_basis_all_decode() {
    let golden = load_golden();
    let mut failures = 0usize;
    for (i, vec) in golden.encode_basis.iter().enumerate() {
        let bytes = hex_to_bytes(&vec.frame.to_lowercase());
        if !raw_crc_valid(&bytes) {
            eprintln!("encode_basis[{}]: raw CRC invalid", i);
            failures += 1;
            continue;
        }
        match Frame::decode(&bytes) {
            Ok(f) => {
                let re_encoded = f.encode();
                if re_encoded.as_slice() != bytes.as_slice() {
                    eprintln!("encode_basis[{}]: roundtrip mismatch", i);
                    failures += 1;
                }
            }
            Err(FrameError::UnknownType) => {
                // Basis vectors with frame_type > 3 are valid wire frames
                // but have no FrameType enum variant — CRC is already verified.
            }
            Err(e) => {
                eprintln!("encode_basis[{}]: unexpected decode error: {:?}", i, e);
                failures += 1;
            }
        }
    }
    assert_eq!(failures, 0,
        "{} encode_basis vectors failed", failures);
}

#[test]
fn test_syndrome_basis() {
    let golden = load_golden();
    let mut failures = 0usize;
    for (i, vec) in golden.syndrome_basis.iter().enumerate() {
        let word: Vec<u8> = if let Some(bit) = vec.bit {
            let mut w = vec![0u8; FRAME_SIZE];
            w[bit as usize / 8] = 1 << (7 - bit as usize % 8);
            w
        } else {
            vec![0u8; FRAME_SIZE]
        };
        let computed = syndrome(&word);
        let expected = vec.syndrome as u16;
        if computed != expected {
            eprintln!("syndrome_basis[{}]: computed 0x{:04X} != expected 0x{:04X}",
                i, computed, expected);
            failures += 1;
        }
    }
    assert_eq!(failures, 0,
        "{} syndrome_basis vectors failed", failures);
}

#[test]
fn test_roundtrip_all_frame_types() {
    for ft in [FrameType::Data, FrameType::Ack, FrameType::Beacon, FrameType::Ctrl] {
        let f = Frame::new(1, ft, 0, 1, 2, [0xAA; 4], 0);
        let wire = f.encode();
        let back = Frame::decode(&wire).expect("all frame types must roundtrip");
        assert_eq!(back.frame_type, ft);
    }
}

#[test]
fn test_bad_sync_rejected() {
    let f = Frame::new(1, FrameType::Data, 1, 1, BROADCAST, [0xDE, 0xAD, 0xBE, 0xEF], 0);
    let mut wire = f.encode();
    wire[0] = 0x00;
    assert_eq!(Frame::decode(&wire), Err(FrameError::BadSync));
}

#[test]
fn test_payload_corruption_detected() {
    let f = Frame::new(1, FrameType::Data, 1, 1, BROADCAST, [0xDE, 0xAD, 0xBE, 0xEF], 0);
    let mut wire = f.encode();
    wire[9] ^= 0xFF;
    assert_eq!(Frame::decode(&wire), Err(FrameError::BadCrc));
}

#[test]
fn test_crc_cross_language_pin() {
    let body: [u8; 15] = [
        0xD3, 0x10, 0x00, 0x01,
        0x00, 0x01, 0xFF, 0xFF,
        0xDE, 0xAD, 0xBE, 0xEF,
        0x00, 0x00, 0x00,
    ];
    let crc = crc16_ccitt(&body);
    assert_eq!(crc, 0x42DD,
        "CRC pin failed: algorithm diverged from cross-language reference (0x42DD), got 0x{:04X}", crc);
}

#[test]
fn test_all_certification() {
    let golden = load_golden();
    println!("certify_sdk.rs — Rust wire certification harness");
    println!("{}", "=".repeat(60));

    let c0 = crc16_ccitt(&[0u8; 15]);
    println!("  PASS  CRC(0^15) = 0x{:04X}", c0);
    assert_eq!(c0, 0x4EC3);

    let c1 = crc16_ccitt(b"123456789");
    println!("  PASS  CRC('123456789') = 0x{:04X}", c1);
    assert_eq!(c1, 0x29B1);

    let n_enc = golden.encode_basis.len();
    let n_syn = golden.syndrome_basis.len();
    println!("  INFO  loaded golden_vectors.json ({} encode, {} syndrome vectors)", n_enc, n_syn);

    let expected = hex_to_bytes(&golden.anchors.example_frame_full.to_lowercase());
    let f = Frame::new(1, FrameType::Ctrl, 0x1234, 0x0001, BROADCAST,
                       [0xDE, 0xAD, 0xBE, 0xEF], 0xAB12CD);
    let wire = f.encode();
    assert_eq!(wire.as_slice(), expected.as_slice(), "exampleFrame mismatch");
    println!("  PASS  exampleFrame = {}", to_hex(&wire));

    let d = Frame::decode(&wire).expect("exampleFrame must decode");
    println!("  PASS  decode(exampleFrame): type={:?} seq=0x{:04X}", d.frame_type, d.seq);

    let mut n_decoded = 0usize;
    let mut n_crc_only = 0usize;
    for (i, vec) in golden.encode_basis.iter().enumerate() {
        let bytes = hex_to_bytes(&vec.frame.to_lowercase());
        assert!(raw_crc_valid(&bytes), "encode_basis[{}]: raw CRC invalid", i);
        match Frame::decode(&bytes) {
            Ok(decoded) => {
                let re_encoded = decoded.encode();
                assert_eq!(re_encoded.as_slice(), bytes.as_slice(),
                    "encode_basis[{}] roundtrip failed", i);
                n_decoded += 1;
            }
            Err(FrameError::UnknownType) => { n_crc_only += 1; }
            Err(e) => panic!("encode_basis[{}]: unexpected error: {:?}", i, e),
        }
    }
    println!("  PASS  {} decode+roundtrip, {} CRC-only (reserved frame_type bits)", n_decoded, n_crc_only);

    for (i, vec) in golden.syndrome_basis.iter().enumerate() {
        let word: Vec<u8> = if let Some(bit) = vec.bit {
            let mut w = vec![0u8; FRAME_SIZE];
            w[bit as usize / 8] = 1 << (7 - bit as usize % 8);
            w
        } else {
            vec![0u8; FRAME_SIZE]
        };
        let computed = syndrome(&word);
        let expected_syn = vec.syndrome as u16;
        assert_eq!(computed, expected_syn,
            "syndrome_basis[{}] mismatch: 0x{:04X} != 0x{:04X}", i, computed, expected_syn);
    }
    println!("  PASS  all {} syndrome_basis vectors match", n_syn);

    println!();
    println!("ALL CERTIFICATION CHECKS PASSED — Rust codec is cemented.");
}