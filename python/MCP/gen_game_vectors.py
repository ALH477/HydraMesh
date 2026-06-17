# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for the DCF-Game L2 framing.

Mirrors gen_audio_vectors.py: it first asserts the framing/body laws hold, then emits
the finite vectors that the C and Rust implementations certify against byte-for-byte.

Usage:  python3 gen_game_vectors.py [game_vectors.json]
  Writes  <path>                  (game framing + reassembly + body round-trips)
  and     <dir>/game_vectors.gen.h  (dependency-free C test header)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gamelab_core import (
    packetize, GameReassembler,
    snapshot_pack, snapshot_unpack, SNAPSHOT_LEN,
    input_pack, input_unpack, INPUT_LEN,
    join_pack, join_unpack,
    FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_PACKET_ID, FDATA,
    GMSG_SNAPSHOT, GMSG_INPUT, GMSG_EVENT, GMSG_JOIN,
    FLAG_RELIABLE, FLAG_ORDERED, FLAG_END_TICK,
)
from wirelab_core import decode

random.seed(0x6A3E)
ok = lambda name: print(f"  PASS  {name}")


def hexframes(frames):
    return [f.hex() for f in frames]


# ── Law A: packetize → reassemble is identity (in-order) ──────────────────────
def reassemble_all(frames):
    r = GameReassembler()
    out = []
    for f in frames:
        out += r.push(f)
    out += r.finalize()
    return out


def make_case(msg_type_id, size, packet_id, ts_us, src, dst, flags=0):
    payload = bytes(random.randrange(256) for _ in range(size))
    frames = packetize(msg_type_id, payload, packet_id, ts_us, src, dst, flags)
    return payload, frames


framing_cases = []
sizes = [0, 1, 4, 7, 60, 120, MAX_PAYLOAD]
types = [GMSG_SNAPSHOT, GMSG_INPUT, GMSG_EVENT, GMSG_JOIN]
case_flags = [FLAG_RELIABLE, 0, FLAG_RELIABLE | FLAG_ORDERED, FLAG_END_TICK,
              0, FLAG_ORDERED, FLAG_RELIABLE]
for i, size in enumerate(sizes):
    msg_type_id = types[i % len(types)]
    packet_id = (i * 37) % (MAX_PACKET_ID + 1)
    ts_us = (i * 0x012345) % (1 << 24)
    flags = case_flags[i]
    payload, frames = make_case(msg_type_id, size, packet_id, ts_us, 0x0001, 0xFFFF, flags)

    # frame count law:  1 descriptor + ceil(len/4) data frames
    assert len(frames) == 1 + (len(payload) + 3) // 4
    # every emitted frame is an ordinary valid DATA DeModFrame
    for f in frames:
        d = decode(f)
        assert d["frame_type"] == FDATA
        assert (d["seq"] >> FRAG_BITS) == packet_id
    # identity
    events = reassemble_all(frames)
    assert events == [("packet", packet_id, ts_us, msg_type_id, payload, flags)], events

    framing_cases.append({
        "msg_type_id": msg_type_id, "src": 0x0001, "dst": 0xFFFF,
        "packet_id": packet_id, "ts_us": ts_us, "flags": flags,
        "payload": payload.hex(), "frames": hexframes(frames),
    })
ok(f"packetize→reassemble = id on {len(framing_cases)} framing cases (sizes {sizes})")

# bounds law
try:
    packetize(0, bytes(MAX_PAYLOAD + 1), 0, 0, 1, 1)
    raise AssertionError("oversize payload must be rejected")
except ValueError:
    pass
ok(f"payload > {MAX_PAYLOAD}B rejected; frag field is {FRAG_BITS} bits (max {MAX_FRAGS} frags)")


# ── Law B: reassembly under reorder / drop / duplicate ────────────────────────
def events_to_json(events):
    packets, lost = [], []
    for e in events:
        if e[0] == "packet":
            _, pid, ts, mid, payload, flags = e
            packets.append({"packet_id": pid, "ts_us": ts, "msg_type_id": mid,
                            "payload": payload.hex(), "flags": flags})
        else:
            lost.append(e[1])
    return packets, lost


reassembly_cases = []
# Two source messages to interleave.
p0_payload, p0 = make_case(GMSG_SNAPSHOT, 14, 5, 0x010203, 0x0001, 0x0002, FLAG_ORDERED)
p1_payload, p1 = make_case(GMSG_EVENT, 9, 6, 0x010210, 0x0001, 0x0002, FLAG_RELIABLE)

seq_in = p0 + p1                                    # 1. in-order
seq_re = list(seq_in)                               # 2. reordered (deterministic)
random.Random(0xBEEF).shuffle(seq_re)
seq_drop = [f for j, f in enumerate(p0) if j != 2] + p1   # 3. drop one frag of p0 -> lost
seq_dup = [p0[0], p0[0], p0[1], p0[1]] + p0[2:] + p1      # 4. duplicate

for name, stream in [("in_order", seq_in), ("reordered", seq_re),
                     ("frag_drop_lost", seq_drop), ("duplicate", seq_dup)]:
    events = reassemble_all(stream)
    packets, lost = events_to_json(events)
    reassembly_cases.append({"name": name, "input_frames": hexframes(stream),
                             "packets": packets, "lost": lost})

assert reassembly_cases[0]["lost"] == [] and len(reassembly_cases[0]["packets"]) == 2
assert reassembly_cases[1]["lost"] == [] and len(reassembly_cases[1]["packets"]) == 2
assert reassembly_cases[2]["lost"] == [5] and len(reassembly_cases[2]["packets"]) == 1
assert reassembly_cases[3]["lost"] == [] and len(reassembly_cases[3]["packets"]) == 2
ok("reassembly correct under reorder, fragment-drop→lost, and duplicate")


# ── Law C: SNAPSHOT/INPUT bodies are byte-lossless (pack∘unpack = id) ──────────
snapshot_blocks = [
    bytes(range(SNAPSHOT_LEN)),                   # ramp
    bytes([0xFF] * SNAPSHOT_LEN),                 # rail high (negative coords + max yaw)
    bytes(SNAPSHOT_LEN),                          # rail low (origin)
    bytes(random.randrange(256) for _ in range(SNAPSHOT_LEN)),
]
snapshot_roundtrip = []
for blk in snapshot_blocks:
    assert snapshot_pack(snapshot_unpack(blk)) == blk
    snapshot_roundtrip.append({"bytes": blk.hex(), "fields": snapshot_unpack(blk)})
ok(f"SNAPSHOT pack∘unpack = id on {len(snapshot_blocks)} blocks ({SNAPSHOT_LEN} B each)")

input_blocks = [
    bytes(range(INPUT_LEN)),
    bytes([0xFF] * INPUT_LEN),
    bytes(INPUT_LEN),
    bytes(random.randrange(256) for _ in range(INPUT_LEN)),
]
input_roundtrip = []
for blk in input_blocks:
    assert input_pack(input_unpack(blk)) == blk
    input_roundtrip.append({"bytes": blk.hex(), "fields": input_unpack(blk)})
ok(f"INPUT pack∘unpack = id on {len(input_blocks)} blocks ({INPUT_LEN} B each)")


# ── Law D: JOIN body round-trips both directions (Python/Rust JSON cert) ───────
join_cases_in = [
    {"player_id": 0x0001, "name": "p1"},
    {"player_id": 0xABCD, "name": "longerplayername"},
    {"player_id": 0xFFFE, "name": ""},
]
join_roundtrip = []
for j in join_cases_in:
    b = join_pack(j)
    assert join_unpack(b) == j
    join_roundtrip.append({"fields": j, "bytes": b.hex()})
ok(f"JOIN pack/unpack = id on {len(join_roundtrip)} cases")


# ── Anchor: a worked example game message ─────────────────────────────────────
anchor_payload = bytes.fromhex("CAFEBABE0042")        # 6 bytes
anchor_frames = packetize(GMSG_EVENT, anchor_payload, 0x10, 0x010203, 0x0001, 0xFFFF, FLAG_RELIABLE)

game_vectors = {
    "format": "DCF-Game L2 framing v1 (adapter over the 17-byte DeModFrame quantum)",
    "spec": "seq = packet_id[15:5] | frag_idx[4:0]; frag_idx 0 = [len,frag_total,msg_type_id,flags]; "
            "frag_idx 1..N = 4 payload bytes (last zero-padded); type=DATA(0), version=1",
    "constants": {"frag_bits": FRAG_BITS, "max_frags": MAX_FRAGS, "max_payload": MAX_PAYLOAD,
                  "max_packet_id": MAX_PACKET_ID, "frame_type_data": FDATA},
    "anchors": {
        "exampleGameMessage": {
            "msg_type_id": GMSG_EVENT, "packet_id": 0x10, "ts_us": 0x010203,
            "src": 0x0001, "dst": 0xFFFF, "flags": FLAG_RELIABLE,
            "payload": anchor_payload.hex(), "frames": hexframes(anchor_frames),
        }
    },
    "theorem": ("L2 framing is a fixed bit-placement adapter over the certified DeModFrame; matching "
                "these framing + reassembly + SNAPSHOT/INPUT/JOIN body vectors pins the C and Rust "
                "implementations to this reference. msg_type_id is opaque to L2, so the framing vectors "
                "are invariant to message-type choice and to the reliable/ordered flags."),
    "framing": framing_cases,
    "reassembly": reassembly_cases,
    "snapshot_roundtrip": snapshot_roundtrip,
    "input_roundtrip": input_roundtrip,
    "join_roundtrip": join_roundtrip,
}


# ── C header emitter (dependency-free vectors for the C cert test) ────────────
def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"


def emit_c_header():
    L = ['/* GENERATED by python/MCP/gen_game_vectors.py — DO NOT EDIT. */',
         '#ifndef DCF_GAME_VECTORS_GEN_H', '#define DCF_GAME_VECTORS_GEN_H',
         '#include <stdint.h>', '']
    # framing
    L += ['typedef struct { uint8_t msg_type_id; uint16_t src, dst, packet_id;',
          '  uint32_t ts_us; uint8_t flags, payload_len, payload[124];',
          '  uint8_t n_frames; uint8_t frames[32][17]; } gv_framing_t;',
          'static const gv_framing_t GV_FRAMING[] = {']
    for c in framing_cases:
        pl = bytes.fromhex(c["payload"])
        frs = [bytes.fromhex(h) for h in c["frames"]]
        fr = ",".join(carr(f) for f in frs)
        L.append(f'  {{{c["msg_type_id"]},0x{c["src"]:04X},0x{c["dst"]:04X},'
                 f'0x{c["packet_id"]:04X},0x{c["ts_us"]:06X}u,{c["flags"]},'
                 f'{len(pl)},{carr(pl) if pl else "{0}"},{len(frs)},{{{fr}}}}},')
    L += ['};', 'static const int GV_N_FRAMING = (int)(sizeof(GV_FRAMING)/sizeof(GV_FRAMING[0]));', '']
    # reassembly
    L += ['typedef struct { uint16_t packet_id; uint32_t ts_us; uint8_t msg_type_id,'
          ' flags, payload_len, payload[124]; } gv_pkt_t;',
          'typedef struct { const char *name; uint8_t n_in; uint8_t in_frames[16][17];',
          '  uint8_t n_pkt; gv_pkt_t pkts[4]; uint8_t n_lost; uint16_t lost[4]; } gv_reasm_t;',
          'static const gv_reasm_t GV_REASM[] = {']
    for rc in reassembly_cases:
        ins = [bytes.fromhex(h) for h in rc["input_frames"]]
        inf = ",".join(carr(f) for f in ins)
        pkts = []
        for p in rc["packets"]:
            pl = bytes.fromhex(p["payload"])
            pkts.append(f'{{0x{p["packet_id"]:04X},0x{p["ts_us"]:06X}u,{p["msg_type_id"]},'
                        f'{p["flags"]},{len(pl)},{carr(pl) if pl else "{0}"}}}')
        lost = ",".join(str(x) for x in rc["lost"]) or "0"
        L.append(f'  {{"{rc["name"]}",{len(ins)},{{{inf}}},{len(rc["packets"])},'
                 f'{{{",".join(pkts) if pkts else "{0}"}}},{len(rc["lost"])},{{{lost}}}}},')
    L += ['};', 'static const int GV_N_REASM = (int)(sizeof(GV_REASM)/sizeof(GV_REASM[0]));', '']
    # snapshot round-trip (raw bytes; C verifies pack(unpack(b)) == b)
    L += ['static const uint8_t GV_SNAPSHOT[][14] = {']
    for blk in snapshot_roundtrip:
        L.append('  ' + carr(bytes.fromhex(blk["bytes"])) + ',')
    L += ['};', 'static const int GV_N_SNAPSHOT = (int)(sizeof(GV_SNAPSHOT)/sizeof(GV_SNAPSHOT[0]));', '']
    # input round-trip
    L += ['static const uint8_t GV_INPUT[][6] = {']
    for blk in input_roundtrip:
        L.append('  ' + carr(bytes.fromhex(blk["bytes"])) + ',')
    L += ['};', 'static const int GV_N_INPUT = (int)(sizeof(GV_INPUT)/sizeof(GV_INPUT[0]));', '',
          '#endif /* DCF_GAME_VECTORS_GEN_H */', '']
    return "\n".join(L)


out_game = sys.argv[1] if len(sys.argv) > 1 else "game_vectors.json"
out_dir = os.path.dirname(out_game) or "."
out_h = os.path.join(out_dir, "game_vectors.gen.h")
with open(out_game, "w") as fh:
    json.dump(game_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_game} ({os.path.getsize(out_game)} bytes, "
      f"{len(framing_cases)} framing, {len(reassembly_cases)} reassembly, "
      f"{len(snapshot_roundtrip)} snapshot, {len(input_roundtrip)} input)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL GAME LAWS HOLD")
