// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for HydraPack — diffs the Rust implementation against the
//! cross-language golden vectors (Documentation/hydrapack_vectors.json).
//! Passing this == byte-agreement with the C and Python references.

use dcf_wire_codec::hydrapack::*;
use dcf_wire_codec::pipe;
use serde::Deserialize;
use std::path::Path;

// ── Schemas (must match the Python test schemas in gen_hydrapack_vectors.py) ─
static SUB_POS: [Field; 2] = [Field::u("x", 12), Field::u("y", 12)];
static SUB_VEL: [Field; 2] = [Field::i("vx", 8), Field::i("vy", 8)];

static F0: [Field; 4] = [
    Field::u("x", 10), Field::u("y", 10), Field::i("v", 8), Field::bool_("hot"),
];
static F1: [Field; 3] = [
    Field::u("sensor_type", 8), Field::i("value", 16), Field::u("flags", 8),
];
static F2: [Field; 4] = [
    Field::enum_("cmd", 4), Field::enum_("mode", 4),
    Field::i("param", 16), Field::u("seq", 8),
];
static F3: [Field; 6] = [
    Field::u("x", 12), Field::u("y", 12), Field::i("vx", 8), Field::i("vy", 8),
    Field::u("heading", 8), Field::u("flags", 8),
];
static F4: [Field; 8] = [
    Field::u("p0", 12), Field::u("p1", 12), Field::u("p2", 12), Field::u("p3", 12),
    Field::u("p4", 12), Field::u("p5", 12), Field::u("p6", 12), Field::u("p7", 12),
];
static F5: [Field; 3] = [
    Field::struct_("pos", &SUB_POS),
    Field::struct_("vel", &SUB_VEL),
    Field::u("id", 8),
];

fn make_schemas() -> Vec<Schema> {
    vec![
        Schema::new(0, 1, &F0),
        Schema::new(1, 1, &F1),
        Schema::new(2, 1, &F2),
        Schema::new(3, 1, &F3),
        Schema::new(4, 1, &F4),
        Schema::new(5, 1, &F5),
    ]
}

// Test values are embedded in the JSON vectors; the Rust cert reconstructs
// them from the JSON `value` fields (for quantum) or round-trips the buffer
// (for pipe). No hardcoded value arrays are needed here.

// ── JSON shapes (only the fields we assert on) ──────────────────────────────
#[derive(Deserialize)]
struct Vectors {
    #[serde(rename = "quantum")]
    quantum: Quantum,
    pipe: Vec<PipeCase>,
    openpipe: Vec<OpenPipeCase>,
}
#[derive(Deserialize)]
struct Quantum {
    single: Vec<QuantumSingle>,
    multi: Vec<QuantumMulti>,
}
#[derive(Deserialize)]
#[allow(dead_code)]
struct QuantumSingle {
    schema_id: u16,
    version: u8,
    quanta: Vec<String>,
    value: serde_json::Value,
}
#[derive(Deserialize)]
struct QuantumMulti {
    schema_id: u16,
    version: u8,
    flags: u8,
    quanta: Vec<String>,
    value: serde_json::Value,
}
#[derive(Deserialize)]
#[allow(dead_code)]
struct PipeCase {
    schema_id: u16,
    version: u8,
    buffer: String,
    checksum: u32,
}
#[derive(Deserialize)]
struct OpenPipeCase {
    session_id: u16,
    total_len: u32,
    chunk_size: u16,
    checksum: u32,
    schema_id: u16,
    schema_version: u8,
    flags: u8,
    #[serde(rename = "bytes")]
    bytes_hex: String,
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
    panic!("{} not found (run python3 python/MCP/gen_hydrapack_vectors.py)", name);
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len()).step_by(2).map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap()).collect()
}
fn to_hex(b: &[u8]) -> String { b.iter().map(|x| format!("{:02x}", x)).collect() }

fn value_from_json(v: &serde_json::Value, schema: &Schema) -> Value {
    let obj = v.as_object().unwrap();
    let mut ints = [0i64; MAX_FIELDS];
    let mut bools = [false; MAX_FIELDS];
    let mut idx = 0;
    let mut bidx = 0;
    fn fill(obj: &serde_json::Map<String, serde_json::Value>, fields: &[Field],
            ints: &mut [i64], bools: &mut [bool], idx: &mut usize, bidx: &mut usize) {
        for f in fields {
            match f.kind {
                Kind::Struct => {
                    let sub = obj.get(f.name).unwrap().as_object().unwrap();
                    fill(sub, f.sub_fields, ints, bools, idx, bidx);
                }
                Kind::Bool => {
                    bools[*bidx] = obj.get(f.name).unwrap().as_bool().unwrap();
                    *bidx += 1; *idx += 1;
                }
                _ => {
                    ints[*idx] = obj.get(f.name).unwrap().as_i64().unwrap();
                    *idx += 1;
                }
            }
        }
    }
    fill(obj, schema.fields, &mut ints, &mut bools, &mut idx, &mut bidx);
    Value { ints, bools, n: idx }
}

#[test]
fn single_quantum_matches_golden() {
    let v: Vectors = load("hydrapack_vectors.json");
    let schemas = make_schemas();
    for (i, c) in v.quantum.single.iter().enumerate() {
        let schema = schemas.iter().find(|s| s.schema_id == c.schema_id).unwrap();
        let val = value_from_json(&c.value, schema);
        let q = pack_quantum(schema, &val, 0, false).unwrap();
        assert_eq!(q.len(), c.quanta.len(), "single[{}]: count", i);
        for (j, (got, exp)) in q.iter().zip(&c.quanta).enumerate() {
            assert_eq!(to_hex(got), exp.to_lowercase(), "single[{}]: quantum[{}]", i, j);
        }
        // round-trip: unpack → re-pack → same bytes
        let r = unpack_quantum_single(&q, schema);
        let q2 = pack_quantum(schema, &r.value, 0, false).unwrap();
        assert_eq!(q2, q, "single[{}]: round-trip", i);
    }
    println!("PASS  {} single-quantum cases byte-identical", v.quantum.single.len());
}

#[test]
fn multi_quantum_matches_golden() {
    let v: Vectors = load("hydrapack_vectors.json");
    let schemas = make_schemas();
    for (i, c) in v.quantum.multi.iter().enumerate() {
        let schema = schemas.iter().find(|s| s.schema_id == c.schema_id).unwrap();
        let val = value_from_json(&c.value, schema);
        let q = pack_quantum(schema, &val, c.flags, false).unwrap();
        assert_eq!(q.len(), c.quanta.len(), "multi[{}]: count", i);
        for (j, (got, exp)) in q.iter().zip(&c.quanta).enumerate() {
            assert_eq!(to_hex(got), exp.to_lowercase(), "multi[{}]: quantum[{}]", i, j);
        }
        // round-trip
        let r = unpack_quantum_multi(&q, schema);
        assert_eq!(r.schema_id, c.schema_id, "multi[{}]: schema_id", i);
        assert_eq!(r.version, c.version, "multi[{}]: version", i);
        assert_eq!(r.flags, c.flags, "multi[{}]: flags", i);
        let q2 = pack_quantum(schema, &r.value, c.flags, false).unwrap();
        assert_eq!(q2, q, "multi[{}]: round-trip", i);
    }
    println!("PASS  {} multi-quantum cases byte-identical", v.quantum.multi.len());
}

#[test]
fn pipe_matches_golden() {
    let v: Vectors = load("hydrapack_vectors.json");
    let schemas = make_schemas();
    for (i, c) in v.pipe.iter().enumerate() {
        let schema = schemas.iter().find(|s| s.schema_id == c.schema_id).unwrap();
        // The pipe buffer is just pack_value; compare directly
        let expected = hex(&c.buffer);
        // We can't reconstruct the value from the JSON for pipe cases without
        // storing it; instead verify the buffer + checksum by re-computing FNV
        assert_eq!(pipe::fnv1a32(&expected), c.checksum, "pipe[{}]: checksum", i);
        // Verify unpack → re-pack round-trip
        let val = unpack_pipe(&expected, schema);
        let rebuf = pack_value(schema, &val);
        assert_eq!(rebuf, expected, "pipe[{}]: round-trip", i);
    }
    println!("PASS  {} pipe cases checksum-verified + round-tripped", v.pipe.len());
}

#[test]
fn openpipe_matches_golden() {
    let v: Vectors = load("hydrapack_vectors.json");
    for (i, c) in v.openpipe.iter().enumerate() {
        let op = pack_openpipe(c.session_id, c.total_len, c.chunk_size, c.checksum,
                               c.schema_id, c.schema_version, c.flags);
        let expected = hex(&c.bytes_hex);
        assert_eq!(op, expected, "openpipe[{}]: bytes", i);
        let r = unpack_openpipe(&op).unwrap();
        assert_eq!(r.session_id, c.session_id, "openpipe[{}]: session", i);
        assert_eq!(r.total_len, c.total_len, "openpipe[{}]: total_len", i);
        assert_eq!(r.chunk_size, c.chunk_size, "openpipe[{}]: chunk_size", i);
        assert_eq!(r.checksum, c.checksum, "openpipe[{}]: checksum", i);
        assert_eq!(r.schema_id, c.schema_id, "openpipe[{}]: schema_id", i);
        assert_eq!(r.schema_version, c.schema_version, "openpipe[{}]: version", i);
        assert_eq!(r.flags, c.flags, "openpipe[{}]: flags", i);
    }
    println!("PASS  {} openpipe cases byte-identical", v.openpipe.len());
}

#[test]
fn fnv_anchors() {
    assert_eq!(pipe::fnv1a32(b""), 0x811C9DC5);
    assert_eq!(pipe::fnv1a32(b"hello"), 0x4F9F2CAB);
    println!("PASS  FNV-1a anchors hold");
}