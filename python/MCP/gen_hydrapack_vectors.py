# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for HydraPack v0.1.

Mirrors gen_text_vectors.py / gen_pipe_vectors.py: it first asserts the packing,
reassembly, pipe, and OpenPipe laws hold, then emits the finite vectors that the
C and Rust implementations certify against byte-for-byte.  HydraPack is a layer
above the 17-byte DeModFrame quantum and the DCF-Pipe data plane — none of this
touches the 246-vector wire certificate.

Two vector families (spec §7):
  1. Quantum vectors — (value → list of 4-byte quanta) for single + multi + reassembly.
  2. Pipe vectors    — (value → byte buffer + checksum) + OpenPipe round-trips.

Usage:  python3 gen_hydrapack_vectors.py [hydrapack_vectors.json]
  Writes  <path>                     (quantum + pipe + openpipe vectors)
  and     <dir>/hydrapack_vectors.gen.h  (dependency-free C test header)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from hydrapack_core import (
    BitReader, BitWriter, Field, Schema,
    pack_value, unpack_value, packed_size,
    pack_quantum, unpack_quantum,
    pack_pipe, unpack_pipe,
    pack_openpipe, unpack_openpipe,
    plane_select,
    fnv1a32,
    HYDRAPACK_VERSION, DEFAULT_THRESHOLD, QUANTUM_LEN, DESC_PAYLOAD_MAX,
    KIND_U, KIND_I, KIND_BOOL, KIND_ENUM, KIND_BITS, KIND_STRUCT,
)
_ALL_KINDS = (KIND_U, KIND_I, KIND_BOOL, KIND_ENUM, KIND_BITS, KIND_STRUCT)

ok = lambda name: print(f"  PASS  {name}")


# ── Test schemas (a fixed set the C header can embed) ────────────────────────
SCHEMAS = [
    # 0: single-quantum (29 bits = 4 bytes) — u10/u10/i8/bool
    Schema(0, 1, [
        Field("x",   KIND_U,    10),
        Field("y",   KIND_U,    10),
        Field("v",   KIND_I,     8),
        Field("hot", KIND_BOOL),
    ]),
    # 1: single-quantum (32 bits = 4 bytes) — sensor reading (mirrors DCF-Sense)
    Schema(1, 1, [
        Field("sensor_type", KIND_U,  8),
        Field("value",       KIND_I, 16),
        Field("flags",       KIND_U,  8),
    ]),
    # 2: single-quantum (32 bits = 4 bytes) — enum-heavy control
    Schema(2, 1, [
        Field("cmd",   KIND_ENUM, 4, enum_values={"IDLE": 0, "MOVE": 1, "FIRE": 2, "STOP": 3}),
        Field("mode",  KIND_ENUM, 4, enum_values={"AUTO": 0, "MANUAL": 1, "SAFE": 2}),
        Field("param", KIND_I,   16),
        Field("seq",   KIND_U,    8),
    ]),
    # 3: multi-quantum (56 bits = 7 bytes) — position+velocity
    Schema(3, 1, [
        Field("x",       KIND_U, 12),
        Field("y",       KIND_U, 12),
        Field("vx",      KIND_I,  8),
        Field("vy",      KIND_I,  8),
        Field("heading", KIND_U,  8),
        Field("flags",   KIND_U,  8),
    ]),
    # 4: multi-quantum (96 bits = 12 bytes) — 8 × u12 params
    Schema(4, 1, [
        Field(f"p{i}", KIND_U, 12) for i in range(8)
    ]),
    # 5: multi-quantum struct nesting (48 bits = 6 bytes)
    Schema(5, 1, [
        Field("pos", KIND_STRUCT, sub_fields=[
            Field("x", KIND_U, 12), Field("y", KIND_U, 12)]),
        Field("vel", KIND_STRUCT, sub_fields=[
            Field("vx", KIND_I, 8), Field("vy", KIND_I, 8)]),
        Field("id", KIND_U, 8),
    ]),
]

REGISTRY = {(s.schema_id, s.version): s for s in SCHEMAS}


def _hex(b):
    return b.hex()

def _quanta_hex(quanta):
    return [_hex(q) for q in quanta]


# ── Law A: pack_value → unpack_value = identity for every schema ────────────
test_values = {
    0: {"x": 1000, "y": 700, "v": -3, "hot": True},
    1: {"sensor_type": 2, "value": -32000, "flags": 0x0F},
    2: {"cmd": 1, "mode": 2, "param": -300, "seq": 42},
    3: {"x": 3500, "y": 2800, "vx": -12, "vy": 7, "heading": 180, "flags": 0x42},
    4: {f"p{i}": 100 * i + 17 for i in range(8)},
    5: {"pos": {"x": 3500, "y": 2800}, "vel": {"vx": -1, "vy": 2}, "id": 42},
}

for s in SCHEMAS:
    v = test_values[s.schema_id]
    packed = pack_value(s, v)
    assert unpack_value(s, packed) == v, f"schema {s.schema_id} round-trip"
    assert len(packed) == packed_size(s), f"schema {s.schema_id} size mismatch"
ok(f"pack_value→unpack_value = id on {len(SCHEMAS)} schemas "
   f"(sizes: {[packed_size(s) for s in SCHEMAS]})")


# ── Law B: bit-pack density — two's complement + zero-pad ────────────────────
# Signed: -1 in 8 bits = 0xFF; -128 in 8 bits = 0x80; 127 = 0x7F
bw = BitWriter()
bw.write(-1, 8)
bw.write(-128, 8)
bw.write(127, 8)
assert bw.to_bytes() == bytes([0xFF, 0x80, 0x7F])

br = BitReader(bytes([0xFF, 0x80, 0x7F]))
assert br.read(8, signed=True) == -1
assert br.read(8, signed=True) == -128
assert br.read(8, signed=True) == 127

# Sub-byte: 3 bits x→5, 3 bits y→3, 2 bits z→1 → byte 0b101_011_01 = 0xAD
bw2 = BitWriter()
bw2.write(5, 3)
bw2.write(3, 3)
bw2.write(1, 2)
assert bw2.to_bytes() == bytes([0xAD]), hex(int.from_bytes(bw2.to_bytes(), "big"))
br2 = BitReader(bytes([0xAD]))
assert br2.read(3) == 5 and br2.read(3) == 3 and br2.read(2) == 1

# 12-bit straddling byte boundary
bw3 = BitWriter()
bw3.write(0xFFF, 12)
bw3.write(0x000, 4)
assert bw3.to_bytes() == bytes([0xFF, 0xF0])
br3 = BitReader(bytes([0xFF, 0xF0]))
assert br3.read(12) == 0xFFF
ok("bit-pack: two's complement + sub-byte + cross-byte boundary")


# ── Quantum vectors ──────────────────────────────────────────────────────────
quantum_single = []
quantum_multi = []

for s in SCHEMAS:
    v = test_values[s.schema_id]
    ps = packed_size(s)
    q = pack_quantum(v, s)
    # Round-trip
    if ps <= QUANTUM_LEN:
        r = unpack_quantum(q, REGISTRY, single_schema=s)
        assert r == (s.schema_id, s.version, 0, v), f"single-quantum schema {s.schema_id}"
        quantum_single.append({
            "schema_id": s.schema_id, "version": s.version,
            "value": v, "quanta": _quanta_hex(q),
        })
    else:
        q = pack_quantum(v, s, flags=0x5)
        r = unpack_quantum(q, REGISTRY)
        assert r == (s.schema_id, s.version, 0x5, v), f"multi-quantum schema {s.schema_id}"
        quantum_multi.append({
            "schema_id": s.schema_id, "version": s.version, "flags": 0x5,
            "value": v, "quanta": _quanta_hex(q),
        })

ok(f"quantum: {len(quantum_single)} single + {len(quantum_multi)} multi round-trip")


# ── Law C: quantum descriptor self-describes (multi) ─────────────────────────
# HydraPack takes an ordered quanta list; the adapter layer (DeModFrame seq/frag_idx)
# handles reordering before quanta reach HydraPack. Here we verify the descriptor
# correctly names the schema + version + length, and that data quanta reproduce the
# exact packed bytes.
for case in quantum_multi:
    sid = case["schema_id"]
    s = REGISTRY[(sid, case["version"])]
    v = case["value"]
    quanta = [bytes.fromhex(h) for h in case["quanta"]]
    # descriptor byte-level checks
    desc = quanta[0]
    assert desc[0] == (sid >> 8) and desc[1] == (sid & 0xFF), "descriptor schema_id"
    assert (desc[2] >> 4) == case["version"], "descriptor version"
    assert (desc[2] & 0x0F) == 0x5, "descriptor flags"
    assert desc[3] == packed_size(s), "descriptor payload_byte_len"
    # data quanta → exact packed bytes
    raw = b"".join(quanta[1:])
    assert raw[:desc[3]] == pack_value(s, v), "data quanta = packed bytes"
ok("quantum multi: descriptor self-describes schema+version+len; data = packed bytes")


# ── Law D: truncated quanta → wrong values (completeness is the adapter's job) ─
# HydraPack has no per-quantum checksum; the adapter layer (DeModFrame frag_idx)
# detects missing quanta and drops the whole message. Here we verify that truncated
# data yields *different* values from the original (corruption is detectable).
for case in quantum_multi[:2]:
    sid = case["schema_id"]
    s = REGISTRY[(sid, case["version"])]
    v = case["value"]
    q = [bytes.fromhex(h) for h in case["quanta"]]
    # Drop the last data quantum, zero-fill past end → wrong values
    q_trunc = [q[0]] + q[1:-1]
    r = unpack_quantum(q_trunc, REGISTRY)
    assert r[3] != v, f"truncated schema {sid} should not match original"
ok("quantum multi: truncated data → wrong values (adapter detects, HydraPack corrupts)")


# ── Pipe vectors ─────────────────────────────────────────────────────────────
pipe_cases = []
for s in [SCHEMAS[3], SCHEMAS[4], SCHEMAS[5]]:
    v = test_values[s.schema_id]
    buf, sid, ver, ck = pack_pipe(v, s)
    assert unpack_pipe(buf, s) == v
    assert ck == fnv1a32(buf)
    pipe_cases.append({
        "schema_id": sid, "version": ver,
        "value": v, "buffer": _hex(buf), "checksum": ck,
    })
ok(f"pipe: {len(pipe_cases)} pack/unpack + FNV-1a round-trips")


# ── OpenPipe vectors ─────────────────────────────────────────────────────────
openpipe_cases = [
    # (session, total_len, chunk_size, checksum, schema_id, version, flags)
    (1, 1000, 256, 0xDEADBEEF, 3, 1, 0),
    (2, 65535, 1400, 0x01234567, 4, 1, 0x5),
    (0xFFFF, 0, 1, 0x811C9DC5, 5, 1, 0xF),
]
open_vectors = []
for sid_, tl, cs, ck, scid, scv, fl in openpipe_cases:
    op = pack_openpipe(sid_, tl, cs, ck, scid, scv, fl)
    assert len(op) == 17
    r = unpack_openpipe(op)
    assert r == (sid_, tl, cs, ck, scid, scv, fl)
    open_vectors.append({
        "session_id": sid_, "total_len": tl, "chunk_size": cs,
        "checksum": ck, "schema_id": scid, "schema_version": scv, "flags": fl,
        "bytes": _hex(op),
    })
ok(f"openpipe: {len(open_vectors)} pack/unpack round-trips")


# ── Plane selection ──────────────────────────────────────────────────────────
for s in SCHEMAS:
    assert plane_select(s) == "quantum", f"schema {s.schema_id} should be quantum"
big = Schema(99, 1, [Field(f"b{i}", KIND_U, 8) for i in range(121)])
assert plane_select(big) == "pipe"
big2 = Schema(98, 1, [Field(f"b{i}", KIND_U, 8) for i in range(201)])
assert plane_select(big2, threshold=200) == "pipe"
assert plane_select(SCHEMAS[0], threshold=2) == "pipe"   # 4B > 2
ok("plane_select: threshold boundary correct")


# ── FNV-1a anchors (same as pipelab_core) ───────────────────────────────────
assert fnv1a32(b"") == 0x811C9DC5
assert fnv1a32(b"hello") == 0x4F9F2CAB
ok("FNV-1a anchors hold (0x811C9DC5, 0x4F9F2CAB)")


# ── Assemble the vector file ─────────────────────────────────────────────────
def _value_to_jsonable(v):
    """Convert a value dict to JSON-serialisable form (structs stay nested dicts)."""
    return v

def _schema_to_jsonable(s):
    return s.to_dict()

hydrapack_vectors = {
    "format": "HydraPack v0.1 quantum + pipe serialization (byte-certified)",
    "spec": ("Plane-aware serialization above DeModFrame + DCF-Pipe. "
             "Quantum descriptor (4B, byte-aligned): [schema_id_hi, schema_id_lo, "
             "(version<<4)|flags, payload_byte_len]. "
             "Pipe: packed bytes + FNV-1a-32. OpenPipe: OPEN[14] + [schema_id_hi, "
             "schema_id_lo, (version<<4)|flags]."),
    "constants": {
        "hydrapack_version": HYDRAPACK_VERSION,
        "default_threshold": DEFAULT_THRESHOLD,
        "quantum_len": QUANTUM_LEN,
        "desc_payload_max": DESC_PAYLOAD_MAX,
        "field_kinds": list(_ALL_KINDS),
    },
    "schemas": [_schema_to_jsonable(s) for s in SCHEMAS],
    "theorem": (
        "Matching these quantum + pipe + openpipe vectors pins the C and Rust "
        "implementations to this reference.  The bit-packer is big-endian, MSB-first, "
        "zero-padded; all field types in v0.1 are fixed-width, so packed_size is a "
        "pure function of the schema.  The 246-vector wire certificate is untouched."),
    "quantum": {
        "single": quantum_single,
        "multi":  quantum_multi,
    },
    "pipe": pipe_cases,
    "openpipe": open_vectors,
}


# ── C header emitter (dependency-free expected bytes for the C cert test) ────
# The header carries ONLY expected output bytes (quanta, pipe buffers, openpipe
# bytes).  The C test file (test_hydrapack_certify.c) hardcodes the schemas and
# test values inline, calls the pack functions, and compares.  Unpack is verified
# by the round-trip: unpack the expected bytes → re-pack → compare to the same
# bytes (a faithful unpack must reproduce identical packed output).

def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"

C_MAX_QUANTA = 8
C_MAX_BUFFER = 128


def emit_c_header():
    L = [
        "/* GENERATED by python/MCP/gen_hydrapack_vectors.py — DO NOT EDIT. */",
        "#ifndef DCF_HYDRAPACK_VECTORS_GEN_H",
        "#define DCF_HYDRAPACK_VECTORS_GEN_H",
        "#include <stdint.h>",
        "",
        f"#define HPV_VERSION        {HYDRAPACK_VERSION}",
        f"#define HPV_THRESHOLD      {DEFAULT_THRESHOLD}",
        f"#define HPV_QUANTUM_LEN    {QUANTUM_LEN}",
        f"#define HPV_DESC_MAX       {DESC_PAYLOAD_MAX}",
        "",
        "/* Field kinds (must match demod_hydrapack.h). */",
        "#define HPV_KIND_U      0",
        "#define HPV_KIND_I      1",
        "#define HPV_KIND_BOOL   2",
        "#define HPV_KIND_ENUM   3",
        "#define HPV_KIND_BITS   4",
        "#define HPV_KIND_STRUCT 5",
        "",
    ]

    # ── Quantum single cases: expected quanta bytes ──
    L += [
        f"typedef struct {{ uint16_t schema_id; uint8_t n_quanta; "
        f"uint8_t quanta[{C_MAX_QUANTA}][4]; }} hpv_quantum_single_t;",
        f"static const hpv_quantum_single_t HPV_QUANTUM_SINGLE[] = {{",
    ]
    for case in quantum_single:
        quanta = [bytes.fromhex(h) for h in case["quanta"]]
        qi = ",".join(carr(q) for q in quanta)
        L.append(f'  {{0x{case["schema_id"]:04X},{len(quanta)},{{{qi}}}}},')
    L += [f"}};",
          f"#define HPV_N_QUANTUM_SINGLE (int)(sizeof(HPV_QUANTUM_SINGLE)/sizeof(HPV_QUANTUM_SINGLE[0]))",
          ""]

    # ── Quantum multi cases: expected quanta bytes ──
    L += [
        f"typedef struct {{ uint16_t schema_id; uint8_t version; uint8_t flags; "
        f"uint8_t n_quanta; uint8_t quanta[{C_MAX_QUANTA}][4]; }} hpv_quantum_multi_t;",
        f"static const hpv_quantum_multi_t HPV_QUANTUM_MULTI[] = {{",
    ]
    for case in quantum_multi:
        quanta = [bytes.fromhex(h) for h in case["quanta"]]
        qi = ",".join(carr(q) for q in quanta)
        L.append(f'  {{0x{case["schema_id"]:04X},0x{case["version"]:02X},0x{case["flags"]:02X},'
                  f'{len(quanta)},{{{qi}}}}},')
    L += [f"}};",
          f"#define HPV_N_QUANTUM_MULTI (int)(sizeof(HPV_QUANTUM_MULTI)/sizeof(HPV_QUANTUM_MULTI[0]))",
          ""]

    # ── Pipe cases: expected buffer + checksum ──
    L += [
        f"typedef struct {{ uint16_t schema_id; uint8_t version; "
        f"uint16_t buf_len; uint8_t buffer[{C_MAX_BUFFER}]; "
        f"uint32_t checksum; }} hpv_pipe_t;",
        f"static const hpv_pipe_t HPV_PIPE[] = {{",
    ]
    for case in pipe_cases:
        buf = bytes.fromhex(case["buffer"])
        buf_c = carr(buf) if buf else "{0}"
        L.append(f'  {{0x{case["schema_id"]:04X},0x{case["version"]:02X},'
                  f'{len(buf)},{buf_c},0x{case["checksum"]:08X}u}},')
    L += [f"}};",
          f"#define HPV_N_PIPE (int)(sizeof(HPV_PIPE)/sizeof(HPV_PIPE[0]))",
          ""]

    # ── OpenPipe cases: expected 17-byte output ──
    L += [
        "typedef struct { uint16_t session_id, chunk_size; uint32_t total_len, checksum; "
        "uint16_t schema_id; uint8_t version, flags; uint8_t bytes[17]; } hpv_openpipe_t;",
        "static const hpv_openpipe_t HPV_OPENPIPE[] = {",
    ]
    for case in open_vectors:
        b = bytes.fromhex(case["bytes"])
        L.append(f'  {{0x{case["session_id"]:04X},0x{case["chunk_size"]:04X},'
                  f'0x{case["total_len"]:08X}u,0x{case["checksum"]:08X}u,'
                  f'0x{case["schema_id"]:04X},0x{case["schema_version"]:02X},0x{case["flags"]:02X},'
                  f'{carr(b)}}},')
    L += ["};",
          "#define HPV_N_OPENPIPE (int)(sizeof(HPV_OPENPIPE)/sizeof(HPV_OPENPIPE[0]))",
          "",
          "#endif /* DCF_HYDRAPACK_VECTORS_GEN_H */", ""]
    return "\n".join(L)


# ── Write outputs ─────────────────────────────────────────────────────────────
out_path = sys.argv[1] if len(sys.argv) > 1 else "hydrapack_vectors.json"
out_dir = os.path.dirname(out_path) or "."
out_h = os.path.join(out_dir, "hydrapack_vectors.gen.h")

with open(out_path, "w") as fh:
    json.dump(hydrapack_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_path} ({os.path.getsize(out_path)} bytes, "
      f"{len(quantum_single)} single-quantum, {len(quantum_multi)} multi-quantum, "
      f"{len(pipe_cases)} pipe, {len(open_vectors)} openpipe)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL HYDRAPACK LAWS HOLD")