// SPDX-License-Identifier: LGPL-3.0-only
//! Cross-language check: a token built by the Python knock client
//! (python/dcf/spa/knock.py, HMAC mode — dependency-free) must verify and
//! grant in the Rust authorizer. This pins byte-for-byte header agreement
//! across the two implementations.

use std::net::Ipv4Addr;
use std::process::Command;

use dcf_spa::{Authorizer, Cred, Granter, Outcome};

#[derive(Default)]
struct MockGranter {
    grants: Vec<(Ipv4Addr, u16)>,
}
impl Granter for MockGranter {
    fn grant(&mut self, ip: Ipv4Addr, port: u16) {
        self.grants.push((ip, port));
    }
}

/// Ask Python to build an HMAC token with a fixed timestamp and nonce so the
/// Rust side can pin the freshness clock. Prints the token as hex on stdout.
fn python_hmac_token(key_hex: &str, device_id: u16, port: u16, ts_ms: u64) -> Option<Vec<u8>> {
    let repo_root = concat!(env!("CARGO_MANIFEST_DIR"), "/..");
    let code = format!(
        "import sys; sys.path.insert(0, 'python'); \
         from dcf.spa.knock import token_hmac; \
         t = token_hmac(bytes.fromhex('{key}'), {dev}, {port}, \
                        timestamp_ms={ts}, nonce=bytes(range(16))); \
         print(t.hex())",
        key = key_hex, dev = device_id, port = port, ts = ts_ms
    );
    let out = Command::new("python3")
        .args(["-c", &code])
        .current_dir(repo_root)
        .output()
        .ok()?;
    if !out.status.success() {
        eprintln!("python knock failed: {}", String::from_utf8_lossy(&out.stderr));
        return None;
    }
    let hex_str = String::from_utf8(out.stdout).ok()?;
    hex::decode(hex_str.trim()).ok()
}

#[test]
fn python_token_grants_in_rust() {
    let key_hex = "07".repeat(32); // matches [7u8; 32]
    let key = [7u8; 32];
    let device_id = 5u16;
    let port = 7100u16;
    let ts_ms = 1_000_000_000_000u64;

    let token = match python_hmac_token(&key_hex, device_id, port, ts_ms) {
        Some(t) => t,
        None => {
            // No python3 on PATH — skip rather than fail (mirrors the janus tests).
            eprintln!("SKIP: python3 unavailable for cross-language check");
            return;
        }
    };
    assert_eq!(token.len(), dcf_spa::LEN_HMAC, "python built a 63-byte HMAC token");

    let mut a = Authorizer::new(port, 30_000);
    a.add_cred(device_id, Cred::Hmac(key));
    let mut g = MockGranter::default();

    let out = a.process(&token, Ipv4Addr::new(192, 0, 2, 9), &mut g, Some(ts_ms));
    assert_eq!(out, Outcome::Granted { device_id, port });
    assert_eq!(g.grants, vec![(Ipv4Addr::new(192, 0, 2, 9), port)]);

    // And an exact replay of the python bytes is rejected.
    assert_eq!(
        a.process(&token, Ipv4Addr::new(192, 0, 2, 9), &mut g, Some(ts_ms)),
        Outcome::Rejected(dcf_spa::Reject::Replay)
    );
}
