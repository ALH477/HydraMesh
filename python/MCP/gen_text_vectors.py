# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for the DCF-Text L2 framing.

Mirrors gen_game_vectors.py: it first asserts the framing/reassembly laws hold, then
emits the finite vectors that the C and Rust implementations certify against
byte-for-byte.  Text is an adapter over the 17-byte DeModFrame, so none of this touches
the 246-vector wire certificate.

Usage:  python3 gen_text_vectors.py [text_vectors.json]
  Writes  <path>                  (text framing + reassembly)
  and     <dir>/text_vectors.gen.h  (dependency-free C test header)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from textlab_core import (
    packetize, TextReassembler, channel_id,
    FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_PACKET_ID, FDATA,
    FLAG_AGENT, FLAG_MORE, FLAG_RELIABLE,
)
from wirelab_core import decode

random.seed(0x7E47)
ok = lambda name: print(f"  PASS  {name}")

# Bound the dependency-free C header: cases larger than this are JSON-only (Python +
# Rust still assert them).  Keeps the static C arrays small / embeddable.
C_HEADER_MAX_PAYLOAD = 128


def hexframes(frames):
    return [f.hex() for f in frames]


# ── Law A: packetize → reassemble is identity (in-order) ──────────────────────
def reassemble_all(frames):
    r = TextReassembler()
    out = []
    for f in frames:
        out += r.push(f)
    out += r.finalize()
    return out


def make_case(size, packet_id, ts_us, src, dst, flags=0):
    # printable ASCII => always valid UTF-8 => the reassembled string round-trips
    # to exactly these bytes (the framing bytes depend only on the raw payload).
    payload = bytes(random.randrange(0x20, 0x7F) for _ in range(size))
    frames = packetize(payload, packet_id, ts_us, src, dst, flags)
    return payload, frames


framing_cases = []
# sizes exercise: empty, 1, exact 4, non-multiples of 4 (un-pad law), >64 frags, the
# 4092 B / 1023-frag rail.  Mixed flags prove flags are opaque to the framing bytes.
sizes = [0, 1, 4, 7, 13, 64, 124, 257, MAX_PAYLOAD]
case_flags = [0, FLAG_AGENT, FLAG_MORE, FLAG_AGENT | FLAG_RELIABLE, 0,
              FLAG_RELIABLE, FLAG_AGENT, FLAG_MORE, 0]
for i, size in enumerate(sizes):
    packet_id = (i * 7) % (MAX_PACKET_ID + 1)
    ts_us = (i * 0x012345) % (1 << 24)
    flags = case_flags[i]
    payload, frames = make_case(size, packet_id, ts_us, 0x0001, 0xFFFF, flags)

    # frame count law:  1 descriptor + ceil(len/4) data frames
    assert len(frames) == 1 + (len(payload) + 3) // 4
    # every emitted frame is an ordinary valid DATA DeModFrame on this packet_id
    for f in frames:
        d = decode(f)
        assert d["frame_type"] == FDATA
        assert (d["seq"] >> FRAG_BITS) == packet_id
    # descriptor 2-byte length law: byte 0/1 are the true (unpadded) length, hi/lo
    d0 = decode(frames[0])
    desc = bytes.fromhex(d0["payload"])
    assert (desc[0] << 8 | desc[1]) == len(payload) and desc[2] == flags
    # identity (text round-trips through UTF-8 exactly for these inputs)
    events = reassemble_all(frames)
    assert events == [("message", packet_id, ts_us, 0x0001, 0xFFFF,
                       payload.decode("utf-8"), flags)], events

    framing_cases.append({
        "src": 0x0001, "dst": 0xFFFF, "packet_id": packet_id, "ts_us": ts_us,
        "flags": flags, "payload": payload.hex(), "frames": hexframes(frames),
    })
ok(f"packetize→reassemble = id on {len(framing_cases)} framing cases (sizes {sizes})")

# bounds law
try:
    packetize(bytes(MAX_PAYLOAD + 1), 0, 0, 1, 1)
    raise AssertionError("oversize payload must be rejected")
except ValueError:
    pass
ok(f"payload > {MAX_PAYLOAD}B rejected; frag field is {FRAG_BITS} bits (max {MAX_FRAGS} frags)")

# channel rendezvous anchor (same crc16 hash the rest of the repo uses)
assert channel_id("123456789") == 0x29B1
assert channel_id(None) == 0xFFFF
ok("channel_id = crc16_ccitt rendezvous hash (anchor \"123456789\" -> 0x29B1)")


# ── Law B: reassembly under reorder / drop / duplicate ────────────────────────
def events_to_json(events):
    messages, lost = [], []
    for e in events:
        if e[0] == "message":
            _, pid, ts, src, dst, text, flags = e
            messages.append({"packet_id": pid, "ts_us": ts, "src": src, "dst": dst,
                             "flags": flags, "text": text,
                             "payload": text.encode("utf-8").hex()})
        else:
            lost.append(e[1])
    return messages, lost


reassembly_cases = []
# Two source messages to interleave (distinct packet_ids on the same channel).
m0 = "hello over DeModFrame"          # 21 bytes -> 6 data frags
m1 = "reply!"                          # 6 bytes  -> 2 data frags
p0 = packetize(m0, 5, 0x010203, 0x0001, 0x0002, FLAG_AGENT)
p1 = packetize(m1, 6, 0x010210, 0x0001, 0x0002, FLAG_AGENT | FLAG_RELIABLE)

seq_in = p0 + p1                                    # 1. in-order
seq_re = list(seq_in)                               # 2. reordered (deterministic)
random.Random(0xBEEF).shuffle(seq_re)
seq_drop = [f for j, f in enumerate(p0) if j != 2] + p1   # 3. drop one frag of m0 -> lost
seq_dup = [p0[0], p0[0], p0[1], p0[1]] + p0[2:] + p1      # 4. duplicate

for name, stream in [("in_order", seq_in), ("reordered", seq_re),
                     ("frag_drop_lost", seq_drop), ("duplicate", seq_dup)]:
    events = reassemble_all(stream)
    messages, lost = events_to_json(events)
    reassembly_cases.append({"name": name, "input_frames": hexframes(stream),
                             "messages": messages, "lost": lost})

assert reassembly_cases[0]["lost"] == [] and len(reassembly_cases[0]["messages"]) == 2
assert reassembly_cases[1]["lost"] == [] and len(reassembly_cases[1]["messages"]) == 2
assert reassembly_cases[2]["lost"] == [5] and len(reassembly_cases[2]["messages"]) == 1
assert reassembly_cases[3]["lost"] == [] and len(reassembly_cases[3]["messages"]) == 2
ok("reassembly correct under reorder, fragment-drop→lost, and duplicate")


# ── Anchor: a worked example with multibyte UTF-8 ─────────────────────────────
anchor_text = "agent ⇄ agent 🚀"
anchor_frames = packetize(anchor_text, 0x0A, 0x010203, 0x00A1, channel_id("duet"),
                          FLAG_AGENT | FLAG_RELIABLE)

text_vectors = {
    "format": "DCF-Text L2 framing v1 (adapter over the 17-byte DeModFrame quantum)",
    "spec": "seq = packet_id[15:10] | frag_idx[9:0]; frag_idx 0 = [len_hi,len_lo,flags,0]; "
            "frag_idx 1..N = 4 payload bytes (last zero-padded); type=DATA(0), version=1",
    "constants": {"frag_bits": FRAG_BITS, "max_frags": MAX_FRAGS, "max_payload": MAX_PAYLOAD,
                  "max_packet_id": MAX_PACKET_ID, "frame_type_data": FDATA,
                  "flag_agent": FLAG_AGENT, "flag_more": FLAG_MORE,
                  "flag_reliable": FLAG_RELIABLE},
    "anchors": {
        "exampleTextMessage": {
            "packet_id": 0x0A, "ts_us": 0x010203, "src": 0x00A1,
            "dst": channel_id("duet"), "flags": FLAG_AGENT | FLAG_RELIABLE,
            "text": anchor_text, "payload": anchor_text.encode("utf-8").hex(),
            "frames": hexframes(anchor_frames),
        }
    },
    "theorem": ("L2 framing is a fixed bit-placement adapter over the certified DeModFrame; "
                "matching these framing + reassembly vectors pins the C and Rust implementations "
                "to this reference.  The descriptor flags are opaque to L2, so the framing vectors "
                "are invariant to the agent/more/reliable flag choice."),
    "framing": framing_cases,
    "reassembly": reassembly_cases,
}


# ── C header emitter (dependency-free vectors for the C cert test) ────────────
def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"


def emit_c_header():
    # only cases that fit the bounded C arrays (the 4092 B rail is JSON-only)
    cframing = [c for c in framing_cases
                if len(bytes.fromhex(c["payload"])) <= C_HEADER_MAX_PAYLOAD]
    fcap = C_HEADER_MAX_PAYLOAD                       # payload bytes per case
    framecap = 1 + (C_HEADER_MAX_PAYLOAD + 3) // 4    # frames per case (1 + ceil/4)
    L = ['/* GENERATED by python/MCP/gen_text_vectors.py — DO NOT EDIT. */',
         '#ifndef DCF_TEXT_VECTORS_GEN_H', '#define DCF_TEXT_VECTORS_GEN_H',
         '#include <stdint.h>', '']
    # framing
    L += [f'typedef struct {{ uint16_t src, dst, packet_id; uint32_t ts_us;',
          f'  uint8_t flags; uint16_t payload_len; uint8_t payload[{fcap}];',
          f'  uint8_t n_frames; uint8_t frames[{framecap}][17]; }} tv_framing_t;',
          'static const tv_framing_t TV_FRAMING[] = {']
    for c in cframing:
        pl = bytes.fromhex(c["payload"])
        frs = [bytes.fromhex(h) for h in c["frames"]]
        fr = ",".join(carr(f) for f in frs)
        L.append(f'  {{0x{c["src"]:04X},0x{c["dst"]:04X},0x{c["packet_id"]:04X},'
                 f'0x{c["ts_us"]:06X}u,{c["flags"]},{len(pl)},'
                 f'{carr(pl) if pl else "{0}"},{len(frs)},{{{fr}}}}},')
    L += ['};', 'static const int TV_N_FRAMING = (int)(sizeof(TV_FRAMING)/sizeof(TV_FRAMING[0]));', '']
    # reassembly
    L += ['typedef struct { uint16_t packet_id; uint32_t ts_us; uint16_t src, dst;',
          f'  uint8_t flags; uint16_t payload_len; uint8_t payload[{fcap}]; }} tv_msg_t;',
          'typedef struct { const char *name; uint8_t n_in; uint8_t in_frames[16][17];',
          '  uint8_t n_msg; tv_msg_t msgs[4]; uint8_t n_lost; uint16_t lost[4]; } tv_reasm_t;',
          'static const tv_reasm_t TV_REASM[] = {']
    for rc in reassembly_cases:
        ins = [bytes.fromhex(h) for h in rc["input_frames"]]
        inf = ",".join(carr(f) for f in ins)
        msgs = []
        for m in rc["messages"]:
            pl = bytes.fromhex(m["payload"])
            msgs.append(f'{{0x{m["packet_id"]:04X},0x{m["ts_us"]:06X}u,0x{m["src"]:04X},'
                        f'0x{m["dst"]:04X},{m["flags"]},{len(pl)},'
                        f'{carr(pl) if pl else "{0}"}}}')
        lost = ",".join(str(x) for x in rc["lost"]) or "0"
        L.append(f'  {{"{rc["name"]}",{len(ins)},{{{inf}}},{len(rc["messages"])},'
                 f'{{{",".join(msgs) if msgs else "{0}"}}},{len(rc["lost"])},{{{lost}}}}},')
    L += ['};', 'static const int TV_N_REASM = (int)(sizeof(TV_REASM)/sizeof(TV_REASM[0]));', '',
          '#endif /* DCF_TEXT_VECTORS_GEN_H */', '']
    return "\n".join(L)


out_text = sys.argv[1] if len(sys.argv) > 1 else "text_vectors.json"
out_dir = os.path.dirname(out_text) or "."
out_h = os.path.join(out_dir, "text_vectors.gen.h")
with open(out_text, "w") as fh:
    json.dump(text_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_text} ({os.path.getsize(out_text)} bytes, "
      f"{len(framing_cases)} framing, {len(reassembly_cases)} reassembly)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL TEXT LAWS HOLD")
