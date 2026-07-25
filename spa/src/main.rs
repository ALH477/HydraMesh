// SPDX-License-Identifier: LGPL-3.0-only
//!
//! `dcf-spa-authorizer` — the DCF-SPA daemon. Binds the knock port, verifies
//! tokens, and installs time-limited nftables allow-set elements for the
//! source addresses of valid knocks. Silent on every failure by design (§6).
//!
//! Authentication only — see `dcf_spa` crate docs and DCF_SPA_SPEC.md §3.

use std::collections::HashMap;
use std::fs;
use std::net::{Ipv4Addr, SocketAddr, UdpSocket};
use std::path::Path;
use std::process::Command;

use dcf_spa::{Authorizer, Cred, Granter, Outcome};
use ed25519_dalek::VerifyingKey;

struct Config {
    knock_addr: String,
    default_mesh_port: u16,
    window_ms: u64,
    grant_ttl_secs: u64,
    nft_table: String,
    nft_set: String,
    creds_dir: String,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            knock_addr: "0.0.0.0:62201".to_string(),
            default_mesh_port: 7100,
            window_ms: 30_000,
            grant_ttl_secs: 30,
            nft_table: "hydramesh_spa".to_string(),
            nft_set: "allowed_peers".to_string(),
            creds_dir: String::new(),
        }
    }
}

/// Parse a hex string into exactly N bytes.
fn hex_to_array<const N: usize>(s: &str) -> Option<[u8; N]> {
    let s = s.trim();
    if s.len() != N * 2 {
        return None;
    }
    let mut out = [0u8; N];
    for i in 0..N {
        out[i] = u8::from_str_radix(&s[2 * i..2 * i + 2], 16).ok()?;
    }
    Some(out)
}

/// Load credentials from `creds_dir`: `NNNN.pub` = Ed25519 public key (32-byte
/// hex), `NNNN.key` = HMAC PSK (32-byte hex). Device id is the file stem.
fn load_creds(dir: &str) -> HashMap<u16, Cred> {
    let mut creds = HashMap::new();
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("creds dir {dir}: {e}");
            return creds;
        }
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let stem = match path.file_stem().and_then(|s| s.to_str()) {
            Some(s) => s,
            None => continue,
        };
        let device_id: u16 = match stem.parse() {
            Ok(id) => id,
            Err(_) => continue,
        };
        let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");
        let contents = match fs::read_to_string(&path) {
            Ok(c) => c,
            Err(_) => continue,
        };
        match ext {
            "pub" => match hex_to_array::<32>(&contents)
                .and_then(|b| VerifyingKey::from_bytes(&b).ok())
            {
                Some(vk) => {
                    creds.insert(device_id, Cred::Ed25519(vk));
                }
                None => eprintln!("bad ed25519 pubkey in {}", path.display()),
            },
            "key" => match hex_to_array::<32>(&contents) {
                Some(k) => {
                    creds.insert(device_id, Cred::Hmac(k));
                }
                None => eprintln!("bad hmac key in {}", path.display()),
            },
            _ => {}
        }
    }
    creds
}

fn parse_args() -> Config {
    let mut cfg = Config::default();
    let args: Vec<String> = std::env::args().collect();
    let mut i = 1;
    while i < args.len() {
        let next = |i: usize| args.get(i + 1).cloned();
        match args[i].as_str() {
            "--knock-addr" => if let Some(v) = next(i) { cfg.knock_addr = v; i += 1; },
            "--knock-port" => if let Some(v) = next(i) {
                cfg.knock_addr = format!("0.0.0.0:{v}"); i += 1;
            },
            "--mesh-port" => if let Some(v) = next(i) {
                cfg.default_mesh_port = v.parse().unwrap_or(cfg.default_mesh_port); i += 1;
            },
            "--window-ms" => if let Some(v) = next(i) {
                cfg.window_ms = v.parse().unwrap_or(cfg.window_ms); i += 1;
            },
            "--grant-ttl" => if let Some(v) = next(i) {
                cfg.grant_ttl_secs = v.parse().unwrap_or(cfg.grant_ttl_secs); i += 1;
            },
            "--creds-dir" => if let Some(v) = next(i) { cfg.creds_dir = v; i += 1; },
            "--nft-table" => if let Some(v) = next(i) { cfg.nft_table = v; i += 1; },
            "--nft-set" => if let Some(v) = next(i) { cfg.nft_set = v; i += 1; },
            "-h" | "--help" => {
                eprintln!("dcf-spa-authorizer --creds-dir DIR [--knock-port P] [--mesh-port P] \
                           [--window-ms MS] [--grant-ttl S] [--nft-table T] [--nft-set S]");
                std::process::exit(0);
            }
            other => eprintln!("ignoring unknown arg: {other}"),
        }
        i += 1;
    }
    cfg
}

/// Production granter: shell out to `nft add element` (spec §7 reference form).
struct NftGranter {
    table: String,
    set: String,
    ttl_secs: u64,
}

impl Granter for NftGranter {
    fn grant(&mut self, ip: Ipv4Addr, _port: u16) {
        let elem = format!("{{ {} timeout {}s }}", ip, self.ttl_secs);
        let status = Command::new("nft")
            .args(["add", "element", "inet", &self.table, &self.set, &elem])
            .status();
        match status {
            Ok(s) if s.success() => {}
            Ok(s) => eprintln!("nft add element exited {s} for {ip}"),
            Err(e) => eprintln!("nft add element failed for {ip}: {e}"),
        }
    }
}

fn main() -> std::io::Result<()> {
    let cfg = parse_args();
    if cfg.creds_dir.is_empty() || !Path::new(&cfg.creds_dir).is_dir() {
        eprintln!("--creds-dir must point at a directory of NNNN.pub / NNNN.key files");
        std::process::exit(2);
    }

    let mut authorizer = Authorizer::new(cfg.default_mesh_port, cfg.window_ms);
    let creds = load_creds(&cfg.creds_dir);
    if creds.is_empty() {
        eprintln!("warning: no credentials loaded from {}", cfg.creds_dir);
    }
    for (id, cred) in creds {
        authorizer.add_cred(id, cred);
    }

    let mut granter = NftGranter {
        table: cfg.nft_table.clone(),
        set: cfg.nft_set.clone(),
        ttl_secs: cfg.grant_ttl_secs,
    };

    let sock = UdpSocket::bind(&cfg.knock_addr)?;
    eprintln!(
        "dcf-spa-authorizer listening on {} (mesh port {}, ttl {}s) — silent by design",
        cfg.knock_addr, cfg.default_mesh_port, cfg.grant_ttl_secs
    );

    let mut buf = [0u8; 128];
    loop {
        let (n, peer) = match sock.recv_from(&mut buf) {
            Ok(x) => x,
            Err(_) => continue,
        };
        let src = match peer {
            SocketAddr::V4(a) => *a.ip(),
            _ => continue, // IPv4 knock channel only
        };
        match authorizer.process(&buf[..n], src, &mut granter, None) {
            Outcome::Granted { device_id, port } => {
                println!("grant device={device_id} saddr={src} port={port} ttl={}s",
                         cfg.grant_ttl_secs);
            }
            // Silent on the wire; log only policy denials (they indicate a
            // provisioned device reaching for a port it may not have).
            Outcome::Rejected(dcf_spa::Reject::PolicyDenied) => {
                eprintln!("deny saddr={src}: policy");
            }
            Outcome::Rejected(_) => {}
        }
    }
}
