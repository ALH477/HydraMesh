// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for DCF-Pipe Multi-Control — diffs against golden vectors.

use dcf_wire_codec::pipemulti::*;
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct Vectors {
    main: Vec<MainCase>,
    reject: Vec<RejectCase>,
    discriminator: Vec<DiscrimCase>,
}
#[derive(Deserialize)]
struct MainCase {
    name: String,
    count: u16,
    flags: u8,
    cmds: Vec<CmdJson>,
    bytes: String,
}
#[derive(Deserialize)]
struct CmdJson {
    local_idx: u8,
    opcode: u8,
    param_lsb: u8,
}
#[derive(Deserialize)]
struct RejectCase {
    name: String,
    byte_input: Option<String>,
    unpack_raised: Option<bool>,
}
#[derive(Deserialize)]
struct DiscrimCase {
    name: String,
    #[serde(rename = "byte0_hex")]
    byte0_hex: String,
    is_multicontrol: bool,
    is_classic_pipe: bool,
}

fn load<T: for<'de> Deserialize<'de>>(name: &str) -> T {
    let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    for p in [
        format!("{}/../Documentation/{}", dir, name),
        format!("{}/../python/MCP/{}", dir, name),
    ] {
        if Path::new(&p).exists() {
            return serde_json::from_str(&std::fs::read_to_string(&p).unwrap()).unwrap();
        }
    }
    panic!("{} not found (run gen_pipemulti_vectors.py)", name);
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len()).step_by(2).map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap()).collect()
}
fn to_hex(b: &[u8]) -> String { b.iter().map(|x| format!("{:02x}", x)).collect() }

#[test]
fn main_matches_golden() {
    let v: Vectors = load("pipemulti_vectors.json");
    for (i, c) in v.main.iter().enumerate() {
        let cmds: Vec<Cmd> = c.cmds.iter().map(|cj|
            Cmd::new(cj.local_idx, cj.opcode, cj.param_lsb).unwrap()
        ).collect();
        let packed = pack_multicontrol(&cmds, c.flags).unwrap();
        let expected = hex(&c.bytes);
        assert_eq!(to_hex(&packed), expected.iter().map(|x| format!("{:02x}", x)).collect::<String>(),
                   "main[{}]: {} byte mismatch", i, c.name);
        // round-trip
        let u = unpack_multicontrol(&packed).unwrap();
        assert_eq!(u.count as u16, c.count, "main[{}]: count", i);
        assert_eq!(u.cmds, cmds, "main[{}]: round-trip", i);
    }
    println!("PASS  {} main cases byte-identical + round-tripped", v.main.len());
}

#[test]
fn reject_cases_raise() {
    let v: Vectors = load("pipemulti_vectors.json");
    let mut n = 0;
    for c in &v.reject {
        if let Some(ref hex_in) = c.byte_input {
            let buf = hex(hex_in);
            let result = unpack_multicontrol(&buf);
            assert!(result.is_err(), "reject[{}]: {} should raise", n, c.name);
            n += 1;
        }
    }
    println!("PASS  {} reject cases all raise on unpack", n);
}

#[test]
fn discriminator_partions() {
    let v: Vectors = load("pipemulti_vectors.json");
    for (i, d) in v.discriminator.iter().enumerate() {
        let b0 = u8::from_str_radix(&d.byte0_hex, 16).unwrap();
        let buf = [b0];
        assert_eq!(is_multicontrol(&buf), d.is_multicontrol, "discrim[{}]: {} is_mc", i, d.name);
        assert_eq!(is_classic_pipe(&buf), d.is_classic_pipe, "discrim[{}]: {} is_classic", i, d.name);
    }
    println!("PASS  {} discriminator samples partition cleanly", v.discriminator.len());
}