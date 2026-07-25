# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for the DCF-Pipe control codec.

Pins the control-message pack/unpack (OPEN/CREDIT/SACK/NACK/DONE/ABORT), the
data-chunk header, and the FNV-1a whole-object checksum so the Rust
(codec/src/pipe.rs), C (codec/demod_pipe.h), and Python reference
(python/MCP/pipelab_core.py) agree byte-for-byte.

Usage:  python3 gen_pipe_vectors.py [pipe_vectors.json]
Writes  <path>  and  <dir>/pipe_vectors.gen.h. Exit 0 iff every law holds.
Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import pipelab_core as p

ok = lambda name: print(f"  PASS  {name}")


def h(b):
    return bytes(b).hex()


# ── OPEN ──────────────────────────────────────────────────────────────────────
open_inputs = [
    (0, 0, 1, 0),
    (7, 100000, 1400, 0xDEADBEEF),
    (31, 0xFFFFFFFF, 8188, 0x00000000),
    (1, 17, 4, 0x811C9DC5),
]
opens = []
for sid, total, cs, ck in open_inputs:
    b = p.pack_open(sid, total, cs, ck)
    assert p.unpack_open(b) == (sid, total, cs, ck)
    opens.append({"session_id": sid, "total_len": total, "chunk_size": cs,
                  "checksum": ck, "bytes": h(b)})
ok(f"{len(opens)} OPEN cases")

# ── CREDIT ────────────────────────────────────────────────────────────────────
credit_inputs = [(0, 0), (7, 32), (31, 0xFFFFFFFF)]
credits = []
for sid, c in credit_inputs:
    b = p.pack_credit(sid, c)
    assert p.unpack_credit(b) == (sid, c)
    credits.append({"session_id": sid, "credit": c, "bytes": h(b)})
ok(f"{len(credits)} CREDIT cases")

# ── SACK ──────────────────────────────────────────────────────────────────────
sack_inputs = [
    (0, 0, []),
    (7, 10, [0b00000101]),
    (7, 0, [0xFF, 0x01]),
    (31, 1000000, [0x80]),
]
sacks = []
for sid, base, bm in sack_inputs:
    b = p.pack_sack(sid, base, bytes(bm))
    rsid, rbase, rbm = p.unpack_sack(b)
    assert (rsid, rbase, list(rbm)) == (sid, base, bm)
    sacks.append({"session_id": sid, "base": base, "bitmap": h(bm), "bytes": h(b)})
# sack_has law: base=10 bitmap 0b101 -> 10 and 12 present, 11 absent
assert p.sack_has(bytes([0b101]), 10, 10) and p.sack_has(bytes([0b101]), 10, 12)
assert not p.sack_has(bytes([0b101]), 10, 11)
assert p.sack_has(bytes([0]), 10, 9)  # below base = acked
ok(f"{len(sacks)} SACK cases")

# ── NACK ──────────────────────────────────────────────────────────────────────
nack_inputs = [(0, []), (7, [3, 9, 4000000]), (31, list(range(5)))]
nacks = []
for sid, miss in nack_inputs:
    b = p.pack_nack(sid, miss)
    assert p.unpack_nack(b) == (sid, miss)
    nacks.append({"session_id": sid, "missing": miss, "bytes": h(b)})
ok(f"{len(nacks)} NACK cases")

# ── DONE / ABORT ──────────────────────────────────────────────────────────────
dones = []
for sid in (0, 7, 31):
    b = p.pack_done(sid)
    assert p.unpack_done(b) == sid
    dones.append({"session_id": sid, "bytes": h(b)})
aborts = []
for sid, reason in [(0, p.ABORT_CHECKSUM), (7, p.ABORT_TIMEOUT), (31, p.ABORT_PEER)]:
    b = p.pack_abort(sid, reason)
    assert p.unpack_abort(b) == (sid, reason)
    aborts.append({"session_id": sid, "reason": reason, "bytes": h(b)})
ok(f"{len(dones)} DONE + {len(aborts)} ABORT cases")

# ── data chunk header ─────────────────────────────────────────────────────────
chunk_inputs = [
    (0, 0, b""),
    (7, 42, b"hello"),
    (31, 0xFFFFFFFF, bytes(range(16))),
]
chunks = []
for sid, seq, pl in chunk_inputs:
    b = p.pack_chunk(sid, seq, pl)
    assert p.unpack_chunk(b) == (sid, seq, pl)
    chunks.append({"session_id": sid, "chunk_seq": seq, "payload": h(pl), "bytes": h(b)})
ok(f"{len(chunks)} chunk-header cases")

# ── FNV-1a checksum + chunk-count anchors ─────────────────────────────────────
fnv_inputs = [b"", b"hello", b"123456789", bytes(range(32))]
fnvs = [{"data": h(d), "checksum": p.fnv1a32(d)} for d in fnv_inputs]
assert p.fnv1a32(b"") == 0x811C9DC5
assert p.fnv1a32(b"hello") == 0x4F9F2CAB
counts = [{"total_len": t, "chunk_size": c, "n": p.num_chunks(t, c)}
          for (t, c) in [(0, 1400), (1400, 1400), (1401, 1400), (100000, 1400), (17, 4)]]
ok(f"{len(fnvs)} FNV + {len(counts)} chunk-count anchors")

doc = {
    "format": "DCF-Pipe control codec v1",
    "anchors": {"fnv_empty": "0x811C9DC5", "fnv_hello": "0x4F9F2CAB"},
    "open": opens, "credit": credits, "sack": sacks, "nack": nacks,
    "done": dones, "abort": aborts, "chunk": chunks, "fnv": fnvs, "counts": counts,
}

out = sys.argv[1] if len(sys.argv) > 1 else "pipe_vectors.json"
with open(out, "w") as f:
    json.dump(doc, f, indent=2)
    f.write("\n")

# ── dependency-free C header ──────────────────────────────────────────────────
def barr(hexstr, width=64):
    """A C initializer for a fixed `width` uint8_t array, zero-padded, plus len."""
    raw = bytes.fromhex(hexstr)
    vals = list(raw) + [0] * (width - len(raw))
    return "{%s}" % ",".join(str(x) for x in vals), len(raw)


hdr = os.path.join(os.path.dirname(os.path.abspath(out)), "pipe_vectors.gen.h")
with open(hdr, "w") as hh:
    hh.write("/* GENERATED by python/MCP/gen_pipe_vectors.py — DO NOT EDIT. */\n")
    hh.write("#ifndef DCF_PIPE_VECTORS_GEN_H\n#define DCF_PIPE_VECTORS_GEN_H\n#include <stdint.h>\n\n")

    def emit_bytes_table(name, rows):
        hh.write("typedef struct { int n; uint8_t bytes[64]; } %s_t;\n" % name)
        hh.write("static const %s_t %s[] = {\n" % (name, name.upper()))
        for r in rows:
            arr, n = barr(r["bytes"])
            hh.write("  {%d,%s},\n" % (n, arr))
        hh.write("};\nstatic const int %s_N = (int)(sizeof(%s)/sizeof(%s[0]));\n\n"
                 % (name.upper(), name.upper(), name.upper()))

    emit_bytes_table("pipe_open", opens)
    emit_bytes_table("pipe_credit", credits)
    emit_bytes_table("pipe_sack", sacks)
    emit_bytes_table("pipe_nack", nacks)
    emit_bytes_table("pipe_done", dones)
    emit_bytes_table("pipe_abort", aborts)
    emit_bytes_table("pipe_chunk", chunks)

    hh.write("typedef struct { int n; uint8_t data[64]; uint32_t checksum; } pipe_fnv_t;\n")
    hh.write("static const pipe_fnv_t PIPE_FNV[] = {\n")
    for r in fnvs:
        arr, n = barr(r["data"])
        hh.write("  {%d,%s,0x%08XU},\n" % (n, arr, r["checksum"]))
    hh.write("};\nstatic const int PIPE_FNV_N = (int)(sizeof(PIPE_FNV)/sizeof(PIPE_FNV[0]));\n\n")

    hh.write("typedef struct { uint32_t total_len; uint16_t chunk_size; uint32_t n; } pipe_count_t;\n")
    hh.write("static const pipe_count_t PIPE_COUNT[] = {\n")
    for r in counts:
        hh.write("  {%dU,%dU,%dU},\n" % (r["total_len"], r["chunk_size"], r["n"]))
    hh.write("};\nstatic const int PIPE_COUNT_N = (int)(sizeof(PIPE_COUNT)/sizeof(PIPE_COUNT[0]));\n")
    hh.write("#endif\n")

print(f"  INFO  wrote {out} and {hdr}")
print("ALL PIPE LAWS HOLD")
