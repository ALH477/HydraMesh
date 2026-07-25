// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF-Pipe control codec — diffs the Rust
//! implementation against the cross-language golden vectors
//! (Documentation/pipe_vectors.json). Passing == byte-agreement with the Python
//! reference and the C port.

use dcf_wire_codec::pipe;
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct Vectors {
    open: Vec<Open>,
    credit: Vec<Credit>,
    sack: Vec<Sack>,
    nack: Vec<Nack>,
    done: Vec<Done>,
    abort: Vec<Abort>,
    chunk: Vec<Chunk>,
    fnv: Vec<Fnv>,
    counts: Vec<Count>,
}
#[derive(Deserialize)]
struct Open { session_id: u16, total_len: u32, chunk_size: u16, checksum: u32, bytes: String }
#[derive(Deserialize)]
struct Credit { session_id: u16, credit: u32, bytes: String }
#[derive(Deserialize)]
struct Sack { session_id: u16, base: u32, bitmap: String, bytes: String }
#[derive(Deserialize)]
struct Nack { session_id: u16, missing: Vec<u32>, bytes: String }
#[derive(Deserialize)]
struct Done { session_id: u16, bytes: String }
#[derive(Deserialize)]
struct Abort { session_id: u16, reason: u8, bytes: String }
#[derive(Deserialize)]
struct Chunk { session_id: u16, chunk_seq: u32, payload: String, bytes: String }
#[derive(Deserialize)]
struct Fnv { data: String, checksum: u32 }
#[derive(Deserialize)]
struct Count { total_len: u32, chunk_size: u16, n: u32 }

fn hexb(s: &str) -> Vec<u8> {
    (0..s.len()).step_by(2).map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap()).collect()
}

fn load() -> Vectors {
    let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    for p in [
        format!("{}/../Documentation/pipe_vectors.json", dir),
        format!("{}/../python/MCP/pipe_vectors.json", dir),
    ] {
        if Path::new(&p).exists() {
            let data = std::fs::read_to_string(&p).unwrap();
            return serde_json::from_str(&data).unwrap_or_else(|e| panic!("parse {}: {}", p, e));
        }
    }
    panic!("pipe_vectors.json not found (run gen_pipe_vectors.py)");
}

#[test]
fn certify_pipe() {
    let v = load();

    for o in &v.open {
        let b = pipe::pack_open(o.session_id, o.total_len, o.chunk_size, o.checksum);
        assert_eq!(b, hexb(&o.bytes), "OPEN pack");
        assert_eq!(pipe::unpack_open(&b), Some((o.session_id, o.total_len, o.chunk_size, o.checksum)));
    }
    for c in &v.credit {
        let b = pipe::pack_credit(c.session_id, c.credit);
        assert_eq!(b, hexb(&c.bytes), "CREDIT pack");
        assert_eq!(pipe::unpack_credit(&b), Some((c.session_id, c.credit)));
    }
    for s in &v.sack {
        let bm = hexb(&s.bitmap);
        let b = pipe::pack_sack(s.session_id, s.base, &bm);
        assert_eq!(b, hexb(&s.bytes), "SACK pack");
        assert_eq!(pipe::unpack_sack(&b), Some((s.session_id, s.base, bm)));
    }
    for n in &v.nack {
        let b = pipe::pack_nack(n.session_id, &n.missing);
        assert_eq!(b, hexb(&n.bytes), "NACK pack");
        assert_eq!(pipe::unpack_nack(&b), Some((n.session_id, n.missing.clone())));
    }
    for d in &v.done {
        let b = pipe::pack_done(d.session_id);
        assert_eq!(b, hexb(&d.bytes), "DONE pack");
        assert_eq!(pipe::unpack_done(&b), Some(d.session_id));
    }
    for a in &v.abort {
        let b = pipe::pack_abort(a.session_id, a.reason);
        assert_eq!(b, hexb(&a.bytes), "ABORT pack");
        assert_eq!(pipe::unpack_abort(&b), Some((a.session_id, a.reason)));
    }
    for c in &v.chunk {
        let pl = hexb(&c.payload);
        let b = pipe::pack_chunk(c.session_id, c.chunk_seq, &pl);
        assert_eq!(b, hexb(&c.bytes), "chunk pack");
        assert_eq!(pipe::unpack_chunk(&b), Some((c.session_id, c.chunk_seq, pl)));
    }
    for f in &v.fnv {
        assert_eq!(pipe::fnv1a32(&hexb(&f.data)), f.checksum, "FNV");
    }
    for c in &v.counts {
        assert_eq!(pipe::num_chunks(c.total_len, c.chunk_size), c.n, "num_chunks");
    }
}
