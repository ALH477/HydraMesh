# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for the DCF-Cue (cue-plane) L2 framing.

Mirrors gen_snake_vectors.py: it asserts the framing/reassembly laws hold, then emits the
finite vectors that the C and Rust implementations certify against byte-for-byte.  The cue
plane is an adapter over the 17-byte DeModFrame, so none of this touches the 246-vector
wire certificate.

Usage:  python3 gen_monitor_vectors.py [monitor_vectors.json]
  Writes  <path>                       (cue framing + reassembly)
  and     <dir>/monitor_vectors.gen.h  (dependency-free C test header)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from monitorlab_core import (
    packetize, CueReassembler, channel_id, pcm_len, bytes_per_sample,
    FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_BLOCK_SEQ, BLOCK_SEQ_MOD, FCTRL,
    FMT_S16, FMT_S24, FMT_F32, FLAG_CUE_RETURN, FLAG_END,
)
from wirelab_core import decode

random.seed(0x4355)   # "CU"
ok = lambda name: print(f"  PASS  {name}")

# All cue blocks are small (<= 508 B), so the dependency-free C header covers every case.
C_HEADER_MAX_PAYLOAD = 512


def hexframes(frames):
    return [f.hex() for f in frames]


def reassemble_all(frames):
    r = CueReassembler()
    out = []
    for f in frames:
        out += r.push(f)
    out += r.finalize()
    return out


# ── Law A: packetize → reassemble is identity (in-order) ──────────────────────
framing_cases = []
# geometry exercises: 1 ms (48) & 0.5 ms (24) blocks, mono/stereo, S16/S24/F32, a 1-sample
# edge, and the 508-byte rail (254 mono S16 samples).  format/channels never touch the framing
# beyond the derived byte-count; flags are opaque.
geoms = [(48, FMT_S16, 1), (48, FMT_S16, 2), (48, FMT_S24, 1), (24, FMT_F32, 2),
         (1, FMT_S16, 1), (254, FMT_S16, 1)]
case_flags = [0, FLAG_CUE_RETURN, FLAG_END, FLAG_CUE_RETURN | FLAG_END, 0, FLAG_CUE_RETURN]
for i, (bs, fmt, ch) in enumerate(geoms):
    block_seq = (i * 37) % (MAX_BLOCK_SEQ + 1)
    ts_us = (i * 0x001111) % (1 << 24)
    flags = case_flags[i]
    n = pcm_len(bs, fmt, ch)
    pcm = bytes(random.randrange(0, 256) for _ in range(n))
    frames = packetize(pcm, block_seq, ts_us, 0x0001, 0xFFFF, bs, fmt, ch, flags)

    assert len(frames) == 1 + (n + 3) // 4
    for f in frames:
        d = decode(f)
        assert d["frame_type"] == FCTRL
        assert (d["seq"] >> FRAG_BITS) == block_seq
    d0 = decode(frames[0])
    desc = bytes.fromhex(d0["payload"])
    assert desc[0] == bs and desc[1] == fmt and desc[2] == ch and desc[3] == flags
    events = reassemble_all(frames)
    assert events == [("block", block_seq, ts_us, 0x0001, 0xFFFF, bs, fmt, ch, flags, pcm)]

    framing_cases.append({
        "src": 0x0001, "dst": 0xFFFF, "block_seq": block_seq, "ts_us": ts_us,
        "block_samples": bs, "format": fmt, "channels": ch, "flags": flags,
        "payload": pcm.hex(), "frames": hexframes(frames),
    })
ok(f"packetize→reassemble = id on {len(framing_cases)} framing cases (geoms {geoms})")

# bounds law
try:
    packetize(bytes(10), 0, 0, 1, 1, 48, FMT_S16, 1)      # geometry mismatch
    raise AssertionError("geometry mismatch must be rejected")
except ValueError:
    pass
try:
    packetize(bytes(512), 0, 0, 1, 1, 128, FMT_S16, 2)    # 512 > 508 cap
    raise AssertionError("oversize block must be rejected")
except ValueError:
    pass
try:
    packetize(bytes(pcm_len(4, FMT_S16, 1)), MAX_BLOCK_SEQ + 1, 0, 1, 1, 4, FMT_S16, 1)
    raise AssertionError("block_seq past the 9-bit field must be rejected")
except ValueError:
    pass
ok(f"geometry-mismatch + block > {MAX_PAYLOAD}B rejected; frag field {FRAG_BITS} bits "
   f"(max {MAX_FRAGS} frags); block_seq capped at {MAX_BLOCK_SEQ}")

assert channel_id("123456789") == 0x29B1 and channel_id(None) == 0xFFFF
ok("channel_id = crc16_ccitt rendezvous hash (anchor \"123456789\" -> 0x29B1)")


# ── Law B: reassembly under reorder / drop / duplicate ────────────────────────
def events_to_json(events):
    blocks, lost = [], []
    for e in events:
        if e[0] == "block":
            _, bseq, ts, src, dst, bs, fmt, ch, fl, data = e
            blocks.append({"block_seq": bseq, "ts_us": ts, "src": src, "dst": dst,
                           "block_samples": bs, "format": fmt, "channels": ch,
                           "flags": fl, "payload": data.hex()})
        else:
            lost.append(e[1])
    return blocks, lost


reassembly_cases = []
b0 = bytes((i * 13) & 0xFF for i in range(pcm_len(20, FMT_S16, 1)))   # 40 B -> 10 data frags
b1 = bytes((i * 7) & 0xFF for i in range(pcm_len(4, FMT_S16, 1)))     # 8 B  -> 2 data frags
p0 = packetize(b0, 5, 0x010203, 0x0001, 0x0002, 20, FMT_S16, 1, FLAG_CUE_RETURN)
p1 = packetize(b1, 6, 0x010210, 0x0001, 0x0002, 4, FMT_S16, 1, FLAG_CUE_RETURN | FLAG_END)

seq_in = p0 + p1
seq_re = list(seq_in)
random.Random(0xC0FFEE).shuffle(seq_re)
seq_drop = [f for j, f in enumerate(p0) if j != 2] + p1
seq_dup = [p0[0], p0[0], p0[1], p0[1]] + p0[2:] + p1

for name, stream in [("in_order", seq_in), ("reordered", seq_re),
                     ("frag_drop_lost", seq_drop), ("duplicate", seq_dup)]:
    events = reassemble_all(stream)
    blocks, lost = events_to_json(events)
    reassembly_cases.append({"name": name, "input_frames": hexframes(stream),
                             "blocks": blocks, "lost": lost})

assert reassembly_cases[0]["lost"] == [] and len(reassembly_cases[0]["blocks"]) == 2
assert reassembly_cases[2]["lost"] == [5] and len(reassembly_cases[2]["blocks"]) == 1
ok("reassembly correct under reorder, fragment-drop→lost, and duplicate")


# ── Anchor ────────────────────────────────────────────────────────────────────
anchor_pcm = bytes(range(96))    # 48 samples mono S16 = a 1 ms block
anchor_frames = packetize(anchor_pcm, 0x07, 0x010203, 0x00A1, channel_id("cue"),
                          48, FMT_S16, 1, FLAG_CUE_RETURN)

monitor_vectors = {
    "format": "DCF-Cue (cue-plane) L2 framing v1 (adapter over the 17-byte DeModFrame quantum)",
    "spec": "seq = block_seq[15:7] | frag_idx[6:0]; frag_idx 0 = [block_samples,format,channels,flags]; "
            "frag_idx 1..N = 4 PCM bytes (last zero-padded); type=CTRL(3), version=1.  length = "
            "block_samples*channels*bytes_per_sample(format) is derived (self-describing).",
    "constants": {"frag_bits": FRAG_BITS, "max_frags": MAX_FRAGS, "max_payload": MAX_PAYLOAD,
                  "max_block_seq": MAX_BLOCK_SEQ, "block_seq_mod": BLOCK_SEQ_MOD,
                  "frame_type_ctrl": FCTRL, "fmt_s16": FMT_S16, "fmt_s24": FMT_S24,
                  "fmt_f32": FMT_F32, "flag_cue_return": FLAG_CUE_RETURN, "flag_end": FLAG_END},
    "anchors": {
        "exampleBlock": {
            "block_seq": 0x07, "ts_us": 0x010203, "src": 0x00A1, "dst": channel_id("cue"),
            "block_samples": 48, "format": FMT_S16, "channels": 1, "flags": FLAG_CUE_RETURN,
            "payload": anchor_pcm.hex(), "frames": hexframes(anchor_frames),
        }
    },
    "theorem": ("L2 framing is a fixed bit-placement adapter over the certified DeModFrame; "
                "matching these framing + reassembly vectors pins the C and Rust implementations "
                "to this reference.  The PCM block length is derived from the descriptor geometry, "
                "so the framing bytes are self-describing."),
    "framing": framing_cases,
    "reassembly": reassembly_cases,
}


# ── C header emitter ──────────────────────────────────────────────────────────
def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"


def emit_c_header():
    cframing = [c for c in framing_cases
                if len(bytes.fromhex(c["payload"])) <= C_HEADER_MAX_PAYLOAD]
    fcap = C_HEADER_MAX_PAYLOAD
    framecap = 1 + (C_HEADER_MAX_PAYLOAD + 3) // 4
    L = ['/* GENERATED by python/MCP/gen_monitor_vectors.py — DO NOT EDIT. */',
         '#ifndef DCF_MONITOR_VECTORS_GEN_H', '#define DCF_MONITOR_VECTORS_GEN_H',
         '#include <stdint.h>', '']
    L += [f'typedef struct {{ uint16_t src, dst, block_seq; uint32_t ts_us;',
          f'  uint8_t block_samples, format, channels, flags; uint16_t payload_len;',
          f'  uint8_t payload[{fcap}]; uint8_t n_frames; uint8_t frames[{framecap}][17]; }} mn_framing_t;',
          'static const mn_framing_t MN_FRAMING[] = {']
    for c in cframing:
        pl = bytes.fromhex(c["payload"])
        frs = [bytes.fromhex(h) for h in c["frames"]]
        fr = ",".join(carr(f) for f in frs)
        L.append(f'  {{0x{c["src"]:04X},0x{c["dst"]:04X},0x{c["block_seq"]:04X},'
                 f'0x{c["ts_us"]:06X}u,{c["block_samples"]},{c["format"]},{c["channels"]},'
                 f'{c["flags"]},{len(pl)},{carr(pl) if pl else "{0}"},{len(frs)},{{{fr}}}}},')
    L += ['};', 'static const int MN_N_FRAMING = (int)(sizeof(MN_FRAMING)/sizeof(MN_FRAMING[0]));', '']
    L += ['typedef struct { uint16_t block_seq; uint32_t ts_us; uint16_t src, dst;',
          f'  uint8_t block_samples, format, channels, flags; uint16_t payload_len;',
          f'  uint8_t payload[{fcap}]; }} mn_block_t;',
          'typedef struct { const char *name; uint8_t n_in; uint8_t in_frames[24][17];',
          '  uint8_t n_blk; mn_block_t blks[4]; uint8_t n_lost; uint16_t lost[4]; } mn_reasm_t;',
          'static const mn_reasm_t MN_REASM[] = {']
    for rc in reassembly_cases:
        ins = [bytes.fromhex(h) for h in rc["input_frames"]]
        inf = ",".join(carr(f) for f in ins)
        blks = []
        for m in rc["blocks"]:
            pl = bytes.fromhex(m["payload"])
            blks.append(f'{{0x{m["block_seq"]:04X},0x{m["ts_us"]:06X}u,0x{m["src"]:04X},'
                        f'0x{m["dst"]:04X},{m["block_samples"]},{m["format"]},{m["channels"]},'
                        f'{m["flags"]},{len(pl)},{carr(pl) if pl else "{0}"}}}')
        lost = ",".join(str(x) for x in rc["lost"]) or "0"
        L.append(f'  {{"{rc["name"]}",{len(ins)},{{{inf}}},{len(rc["blocks"])},'
                 f'{{{",".join(blks) if blks else "{0}"}}},{len(rc["lost"])},{{{lost}}}}},')
    L += ['};', 'static const int MN_N_REASM = (int)(sizeof(MN_REASM)/sizeof(MN_REASM[0]));', '',
          '#endif /* DCF_MONITOR_VECTORS_GEN_H */', '']
    return "\n".join(L)


out_mon = sys.argv[1] if len(sys.argv) > 1 else "monitor_vectors.json"
out_dir = os.path.dirname(out_mon) or "."
out_h = os.path.join(out_dir, "monitor_vectors.gen.h")
with open(out_mon, "w") as fh:
    json.dump(monitor_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_mon} ({os.path.getsize(out_mon)} bytes, "
      f"{len(framing_cases)} framing, {len(reassembly_cases)} reassembly)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL CUE LAWS HOLD")
