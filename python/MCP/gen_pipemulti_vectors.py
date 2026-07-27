# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for DCF-Pipe Multi-Control v0.1.

Mirrors gen_pipe_vectors.py / gen_hydrapack_vectors.py: asserts the packing,
round-trip, discriminator, and rejection laws, then emits the finite vectors the
C and Rust implementations certify against byte-for-byte.  Multi-Control is a
control adapter over the 17-byte DeModFrame; it does NOT touch the 246-vector
wire certificate or pipe_vectors.json.

Vector families:
  1. main      — count sweep + opcode sweep + param sweep (positive pack/unpack bytes)
  2. reject    — illegal inputs that must raise (reserved opcode, bad count, bad pad)
  3. discrim   — is_multicontrol over Multi-Control vectors + classic Pipe + audio desc

Usage:  python3 gen_pipemulti_vectors.py [pipemulti_vectors.json]
  Writes  <path>                       (main + reject + discrim vectors)
  and     <dir>/pipemulti_vectors.gen.h  (dependency-free C test header)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from pipemulti_core import (
    Cmd, pack_multicontrol, unpack_multicontrol,
    is_multicontrol, is_classic_pipe,
    MC_VERSION, MC_PAYLOAD_LEN, MC_MAGIC, MC_MAGIC_MASK, MC_MAX_COUNT,
    OP_NOP, OP_CREDIT_DELTA, OP_ACK_CUMUL, OP_ACK_SELECTIVE,
    OP_NACK_ONE, OP_DONE_HINT, OP_ABORT_HINT, OP_RESERVED,
    _ALL_OPS, _OPCODE_NAMES,
)
from pipelab_core import (
    PIPE_OPEN, PIPE_CREDIT, PIPE_SACK, PIPE_NACK, PIPE_DONE, PIPE_ABORT,
    pack_open, pack_credit, pack_sack, pack_nack, pack_done, pack_abort,
)

ok = lambda name: print(f"  PASS  {name}")


def _hex(b):
    return b.hex()


# ── Law A: minimal anchor (count=1 NOP -> [0xD0, 0, 0, 0]) ──────────────────
anchor = pack_multicontrol([Cmd(0, OP_NOP)])
assert anchor == bytes([0xD0, 0, 0, 0]), anchor.hex()
assert (anchor[0] & MC_MAGIC_MASK) == MC_MAGIC  # magic = 11b
ok(f"anchor: count=1 NOP -> {anchor.hex()} (magic pin: byte0=0xD0 >= 0xC0)")

# ── Law B: round-trip identity on count sweep 1..3 ────────────────────────────
trio = [Cmd(0, OP_CREDIT_DELTA, 1), Cmd(2, OP_ACK_CUMUL, 0), Cmd(3, OP_ABORT_HINT, 1)]
for n in (1, 2, 3):
    v = pack_multicontrol(trio[:n])
    assert len(v) == MC_PAYLOAD_LEN
    c, fl, cs = unpack_multicontrol(v)
    assert c == n and fl == 0 and cs == trio[:n]
    # higher cmd bytes zeroed
    for i in range(n, MC_MAX_COUNT):
        assert v[1 + i] == 0
ok("round-trip: unpack∘pack = id for count 1, 2, 3 (byte-aligned slots)")

# ── Law C: opcode sweep — all 7 legal opcodes encode/decode ──────────────────
for op in _ALL_OPS:
    v = pack_multicontrol([Cmd(1, op, 1)])
    _, _, cs = unpack_multicontrol(v)
    assert cs[0] == Cmd(1, op, 1)
ok(f"opcode sweep: all 7 ops ({', '.join(_OPCODE_NAMES[o] for o in _ALL_OPS)}) round-trip")

# ── Law D: reserved opcode rejected on pack and unpack ───────────────────────
try:
    Cmd(0, OP_RESERVED); assert False, "Cmd(0, OP_RESERVED) should raise"
except ValueError:
    pass
# crafted buffer with opcode 111 in slot 0 (cannot be produced by pack; legal to craft)
bad_op = bytes([0xD0, (0 << 6) | (OP_RESERVED << 3) | (0 << 2), 0, 0])
try:
    unpack_multicontrol(bad_op); assert False, "reserved opcode should raise"
except ValueError:
    pass
ok("reserved opcode 111 rejected on both Cmd construction and unpack")

# ── Law E: pad-bits and zero-fill rejected on unpack ────────────────────────
# nonzero pad bits in a cmd byte
bad_pad = bytes([0xD0, 0b000_000_01, 0, 0])
try:
    unpack_multicontrol(bad_pad); assert False
except ValueError:
    pass
# count=2 but slot 2 (byte 3) nonzero
good2 = pack_multicontrol(trio[:2])
assert good2[3] == 0
bad_fill = bytearray(good2); bad_fill[3] = 0x44
try:
    unpack_multicontrol(bytes(bad_fill)); assert False
except ValueError:
    pass
ok("pad bits + unused-slot zero-fill violations rejected on unpack")

# ── Law F: param LSB bit position (CREDIT_DELTA param 0 vs 1 -> diff bit 2) ─
p0 = pack_multicontrol([Cmd(0, OP_CREDIT_DELTA, 0)])
p1 = pack_multicontrol([Cmd(0, OP_CREDIT_DELTA, 1)])
assert (p0[1] ^ p1[1]) == 0x04
ok("param LSB bit position: param 0 vs 1 differ exactly in bit 2 (0x04)")

# ── Law G: discriminator vs classic Pipe + audio descriptor ─────────────────
classic_msgs = {
    "open":   pack_open(7, 65535, 1400, 0xDEADBEEF),
    "credit": pack_credit(7, 65535),
    "sack":   pack_sack(7, 12345, b""),
    "nack":   pack_nack(7, [0, 1, 2]),
    "done":   pack_done(7),
    "abort":  pack_abort(7, 0),
}
for name, msg in classic_msgs.items():
    assert not is_multicontrol(msg), f"classic {name} not MC"
    assert is_classic_pipe(msg), f"classic {name} detected"
audio_desc = bytes([124, 0xFF, 0xFF, 0xFF])  # audio CTRL frag-0 first byte = payload_len
assert not is_multicontrol(audio_desc)  # top 2 bits != 11
mc_sample = pack_multicontrol([Cmd(0, OP_NOP)])
assert is_multicontrol(mc_sample)
ok("discriminator: is_multicontrol cleanly partitions MC from classic Pipe + audio")

# ── Law H: header byte format — magic | count<<4 | flags ────────────────────
h1 = pack_multicontrol([Cmd(0, OP_NOP)], flags=0)
h2 = pack_multicontrol([Cmd(0, OP_NOP), Cmd(0, OP_NOP)], flags=0)
h3 = pack_multicontrol([Cmd(0, OP_NOP), Cmd(0, OP_NOP), Cmd(0, OP_NOP)], flags=0)
assert h1[0] == 0xD0 and h2[0] == 0xE0 and h3[0] == 0xF0
ok("header byte: 0xD0/0xE0/0xF0 for count 1/2/3 (magic|count<<4|flags)")


# ── Assemble MAIN vectors ────────────────────────────────────────────────────
main_vectors = []

# anchor vector
main_vectors.append({
    "name": "anchor_count1_NOP",
    "count": 1, "flags": 0,
    "cmds": [Cmd(0, OP_NOP).to_dict()],
    "bytes": _hex(anchor),
})

# count sweep with mixed opcodes
for n in (1, 2, 3):
    cmds = trio[:n]
    v = pack_multicontrol(cmds)
    main_vectors.append({
        "name": f"count{n}_mixed",
        "count": n, "flags": 0,
        "cmds": [c.to_dict() for c in cmds],
        "bytes": _hex(v),
    })

# opcode sweep (count=1, local_idx=2, param=1) — one vector per opcode
for op in _ALL_OPS:
    c = Cmd(2, op, 1)
    v = pack_multicontrol([c])
    main_vectors.append({
        "name": f"opcode_{_OPCODE_NAMES[op]}",
        "count": 1, "flags": 0,
        "cmds": [c.to_dict()],
        "bytes": _hex(v),
    })

# param sweep on CREDIT_DELTA: local_idx=0, param 0 vs 1
for p in (0, 1):
    c = Cmd(0, OP_CREDIT_DELTA, p)
    v = pack_multicontrol([c])
    main_vectors.append({
        "name": f"param_credit_delta_lsb{p}",
        "count": 1, "flags": 0,
        "cmds": [c.to_dict()],
        "bytes": _hex(v),
    })

# local_idx sweep: opcode ACK_SELECTIVE param=0, all four local_idx
for li in (0, 1, 2, 3):
    c = Cmd(li, OP_ACK_SELECTIVE, 0)
    v = pack_multicontrol([c])
    main_vectors.append({
        "name": f"local_idx_{li}",
        "count": 1, "flags": 0,
        "cmds": [c.to_dict()],
        "bytes": _hex(v),
    })


# ── REJECT vectors (illegal inputs that must raise) ─────────────────────────
reject_vectors = []

# reserved opcode on pack
try:
    pack_multicontrol([Cmd(0, OP_RESERVED)]); reject_vectors.append({"should_pass": False})
except ValueError:
    pass  # expected
reject_vectors.append({
    "name": "pack_reserved_opcode_raises",
    "byte_input": _hex(bytes([0xD0, (OP_RESERVED << 3), 0, 0])),
    "expect": "raises_on_unpack",
})
# unpack it
try:
    unpack_multicontrol(bytes.fromhex(reject_vectors[-1]["byte_input"]))
    reject_vectors[-1]["unpack_raised"] = False
except ValueError:
    reject_vectors[-1]["unpack_raised"] = True

# count 0 (craft byte 0 = 0xC0 only)
reject_vectors.append({
    "name": "unpack_count0_raises",
    "byte_input": _hex(bytes([0xC0, 0, 0, 0])),
    "expect": "raises_on_unpack",
})
try:
    unpack_multicontrol(bytes.fromhex(reject_vectors[-1]["byte_input"]))
    reject_vectors[-1]["unpack_raised"] = False
except ValueError:
    reject_vectors[-1]["unpack_raised"] = True

# bad magic (top 2 bits != 11)
reject_vectors.append({
    "name": "unpack_bad_magic_raises",
    "byte_input": _hex(bytes([0x40, 0, 0, 0])),
    "expect": "raises_on_unpack",
})
try:
    unpack_multicontrol(bytes.fromhex(reject_vectors[-1]["byte_input"]))
    reject_vectors[-1]["unpack_raised"] = False
except ValueError:
    reject_vectors[-1]["unpack_raised"] = True

# nonzero reserved flags
reject_vectors.append({
    "name": "unpack_nonzero_flags_raises",
    "byte_input": _hex(bytes([0xD1, 0, 0, 0])),  # flags=1
    "expect": "raises_on_unpack",
})
try:
    unpack_multicontrol(bytes.fromhex(reject_vectors[-1]["byte_input"]))
    reject_vectors[-1]["unpack_raised"] = False
except ValueError:
    reject_vectors[-1]["unpack_raised"] = True

# nonzero pad bits in cmd byte
reject_vectors.append({
    "name": "unpack_pad_bits_raises",
    "byte_input": _hex(bytes([0xD0, 0b000_000_01, 0, 0])),
    "expect": "raises_on_unpack",
})
try:
    unpack_multicontrol(bytes.fromhex(reject_vectors[-1]["byte_input"]))
    reject_vectors[-1]["unpack_raised"] = False
except ValueError:
    reject_vectors[-1]["unpack_raised"] = True

# count=2 but unused slot nonzero
bad_fill = bytearray(pack_multicontrol(trio[:2])); bad_fill[3] = 0x44
reject_vectors.append({
    "name": "unpack_unused_slot_nonzero_raises",
    "byte_input": _hex(bytes(bad_fill)),
    "expect": "raises_on_unpack",
})
try:
    unpack_multicontrol(bytes.fromhex(reject_vectors[-1]["byte_input"]))
    reject_vectors[-1]["unpack_raised"] = False
except ValueError:
    reject_vectors[-1]["unpack_raised"] = True

assert all(r.get("unpack_raised", True) for r in reject_vectors
           if r.get("expect") == "raises_on_unpack"), "a reject case didn't raise"
ok(f"reject: {len([r for r in reject_vectors if r.get('expect')=='raises_on_unpack'])} "
   f"illegal buffers all raise on unpack")


# ── DISCRIMINATOR vectors ───────────────────────────────────────────────────
discrim_vectors = []
# positive (MC) samples
for mv in main_vectors[:5]:
    discrim_vectors.append({
        "name": mv["name"], "byte0_hex": mv["bytes"][0:2],
        "is_multicontrol": True, "is_classic_pipe": False,
    })
# negative (classic pipe) samples
for name, msg in classic_msgs.items():
    discrim_vectors.append({
        "name": f"classic_{name}", "byte0_hex": msg.hex()[0:2],
        "is_multicontrol": False, "is_classic_pipe": True,
    })
# negative (audio descriptor)
discrim_vectors.append({
    "name": "audio_ctrl_desc", "byte0_hex": "7c",
    "is_multicontrol": False, "is_classic_pipe": False,
})
# verify each
for d in discrim_vectors:
    b = bytes.fromhex(d["byte0_hex"])
    assert is_multicontrol(b) == d["is_multicontrol"], d["name"]
    assert is_classic_pipe(b) == d["is_classic_pipe"], d["name"]
ok(f"discriminator: {len(discrim_vectors)} samples partition cleanly")


# ── Assemble the vector file ─────────────────────────────────────────────────
pipemulti_vectors = {
    "format": "DCF-Pipe Multi-Control v0.1 (byte-certified)",
    "spec": ("Parallel Pipe control bus: up to 3 steady-state commands in one "
             "4-byte DeModFrame payload. Byte-aligned layout: byte0 = magic|count|flags, "
             "bytes 1-3 = command slots. Magic=11b (byte0>=0xC0). Reserved opcode 111 "
             "rejected. Active-Set mapping and 1-bit param context are runtime state."),
    "constants": {
        "mc_version": MC_VERSION,
        "payload_len": MC_PAYLOAD_LEN,
        "magic_mask": MC_MAGIC_MASK,
        "magic": MC_MAGIC,
        "max_count": MC_MAX_COUNT,
        "opcodes": {n: _OPCODE_NAMES[n] for n in _ALL_OPS},
        "reserved_opcode": OP_RESERVED,
    },
    "theorem": (
        "Matching these main + reject + discriminator vectors pins the C and Rust "
        "implementations to this reference.  The magic anchor (byte0 >= 0xC0) cleanly "
        "partitions Multi-Control from classic Pipe control (byte0 0..5) and from audio "
        "CTRL descriptors (byte0 <= 124, top 2 bits 00/01).  The 246-vector wire "
        "certificate and pipe_vectors.json are untouched."),
    "main": main_vectors,
    "reject": reject_vectors,
    "discriminator": discrim_vectors,
}


# ── C header emitter ────────────────────────────────────────────────────────
def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"


def emit_c_header():
    L = [
        "/* GENERATED by python/MCP/gen_pipemulti_vectors.py — DO NOT EDIT. */",
        "#ifndef DCF_PIPEMULTI_VECTORS_GEN_H",
        "#define DCF_PIPEMULTI_VECTORS_GEN_H",
        "#include <stdint.h>",
        "",
        f"#define MCV_VERSION        {MC_VERSION}",
        f"#define MCV_PAYLOAD_LEN     {MC_PAYLOAD_LEN}",
        f"#define MCV_MAGIC_MASK      0x{MC_MAGIC_MASK:02X}",
        f"#define MCV_MAGIC           0x{MC_MAGIC:02X}",
        f"#define MCV_MAX_COUNT       {MC_MAX_COUNT}",
        "",
        "/* Opcodes (must match demod_pipemulti.h). */",
        "#define MCV_OP_NOP            0",
        "#define MCV_OP_CREDIT_DELTA   1",
        "#define MCV_OP_ACK_CUMUL      2",
        "#define MCV_OP_ACK_SELECTIVE  3",
        "#define MCV_OP_NACK_ONE       4",
        "#define MCV_OP_DONE_HINT      5",
        "#define MCV_OP_ABORT_HINT     6",
        "#define MCV_OP_RESERVED       7",
        "",
    ]

    # MAIN cases: expected output bytes + the cmd fields for reconstruction
    L += [
        "typedef struct { const char *name; uint16_t count; uint8_t flags; "
        "uint8_t bytes[4]; uint8_t local_idx[3]; uint8_t opcode[3]; "
        "uint8_t param_lsb[3]; } mc_main_t;",
        "static const mc_main_t MC_MAIN[] = {",
    ]
    for mv in main_vectors:
        b = bytes.fromhex(mv["bytes"])
        cmds = mv["cmds"]
        li = [c["local_idx"] for c in cmds] + [0] * (3 - len(cmds))
        op = [c["opcode"] for c in cmds] + [0] * (3 - len(cmds))
        pa = [c["param_lsb"] for c in cmds] + [0] * (3 - len(cmds))
        L.append(f'  {{"{mv["name"]}",{mv["count"]},0x{mv["flags"]:02X},'
                  f'{carr(b)},'
                  f'{{{li[0]},{li[1]},{li[2]}}},'
                  f'{{{op[0]},{op[1]},{op[2]}}},'
                  f'{{{pa[0]},{pa[1]},{pa[2]}}}}},')
    L += ["};",
          "#define MC_N_MAIN (int)(sizeof(MC_MAIN)/sizeof(MC_MAIN[0]))", ""]

    # REJECT cases: input bytes that must raise on unpack
    L += [
        "typedef struct { const char *name; uint8_t bytes[4]; int must_raise; } mc_reject_t;",
        "static const mc_reject_t MC_REJECT[] = {",
    ]
    for r in reject_vectors:
        if "byte_input" not in r:
            continue
        b = bytes.fromhex(r["byte_input"])
        L.append(f'  {{"{r["name"]}",{carr(b)},{1 if r.get("unpack_raised") else 0}}},')
    L += ["};",
          "#define MC_N_REJECT (int)(sizeof(MC_REJECT)/sizeof(MC_REJECT[0]))", ""]

    # DISCRIMINATOR cases: byte0 + expected is_multicontrol / is_classic_pipe
    L += [
        "typedef struct { const char *name; uint8_t byte0; int is_mc; int is_classic; } mc_discrim_t;",
        "static const mc_discrim_t MC_DISCRIM[] = {",
    ]
    for d in discrim_vectors:
        b0 = int(d["byte0_hex"], 16)
        L.append(f'  {{"{d["name"]}",0x{b0:02X},'
                  f'{1 if d["is_multicontrol"] else 0},'
                  f'{1 if d["is_classic_pipe"] else 0}}},')
    L += ["};",
          "#define MC_N_DISCRIM (int)(sizeof(MC_DISCRIM)/sizeof(MC_DISCRIM[0]))", "",
          "#endif /* DCF_PIPEMULTI_VECTORS_GEN_H */", ""]
    return "\n".join(L)


# ── Write outputs ─────────────────────────────────────────────────────────────
out_path = sys.argv[1] if len(sys.argv) > 1 else "pipemulti_vectors.json"
out_dir = os.path.dirname(out_path) or "."
out_h = os.path.join(out_dir, "pipemulti_vectors.gen.h")

with open(out_path, "w") as fh:
    json.dump(pipemulti_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_path} ({os.path.getsize(out_path)} bytes, "
      f"{len(main_vectors)} main, {len(reject_vectors)} reject, {len(discrim_vectors)} discrim)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL PIPEMULTI LAWS HOLD")