// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF modulation mapping — diffs the Rust
//! implementation against the cross-language golden vectors
//! (Documentation/modulation_vectors.json). Passing == byte-agreement with the
//! Python and C references on the byte↔symbol bijection.

use dcf_wire_codec::modulation::{bits_per_symbol, demodulate, gray, modulate, ungray};
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct Vectors {
    gray_anchor: Vec<u8>,
    cases: Vec<Case>,
}
#[derive(Deserialize)]
struct Case {
    #[serde(rename = "mod")]
    modulation: u8,
    name: String,
    data: String,
    symbols: Vec<u8>,
}

fn load() -> Vectors {
    let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    for p in [
        format!("{}/../Documentation/modulation_vectors.json", dir),
        format!("{}/../python/MCP/modulation_vectors.json", dir),
    ] {
        if Path::new(&p).exists() {
            let data = std::fs::read_to_string(&p).unwrap();
            return serde_json::from_str(&data).unwrap_or_else(|e| panic!("parse {}: {}", p, e));
        }
    }
    panic!("modulation_vectors.json not found (run gen_modulation_vectors.py)");
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len()).step_by(2).map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap()).collect()
}

#[test]
fn gray_anchor_holds() {
    let v = load();
    assert_eq!((0..v.gray_anchor.len() as u32).map(|n| gray(n) as u8).collect::<Vec<_>>(), v.gray_anchor);
    assert!((0..256).all(|n| ungray(gray(n)) == n));
}

#[test]
fn cases_match_golden() {
    let v = load();
    for c in &v.cases {
        let data = hex(&c.data);
        let syms = modulate(c.modulation, &data);
        assert_eq!(syms, c.symbols, "{}: symbols", c.name);
        let bps = bits_per_symbol(c.modulation) as usize;
        assert_eq!(syms.len(), (8 * data.len() + bps - 1) / bps, "{}: count", c.name);
        assert_eq!(demodulate(c.modulation, &syms, data.len()), data, "{}: round-trip", c.name);
    }
    println!("PASS  {} modulation cases match golden", v.cases.len());
}
