"""Executable laws + golden-vector generator for the DCF-Audio L2 framing.

Mirrors verify_laws.py: it first asserts the framing/codec laws hold, then emits the
finite vectors that the C and Rust implementations certify against byte-for-byte.

Usage:  python3 gen_audio_vectors.py [audio_vectors.json]
  Writes  <path>                       (audio framing + reassembly + PCM round-trip)
  and     <dir>/pm_param_vectors.json  (PM 8-byte param round-trip)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from audiolab_core import (
    packetize, AudioReassembler,
    pcm_diag_encode, pcm_diag_decode, PCM_DIAG_BLOCK,
    pm_pack, pm_unpack,
    FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_PACKET_ID, FCTRL,
    CODEC_OPUS, CODEC_PCM_DIAG, CODEC_FAUST_PM, FLAG_END_TALKSPURT,
)
from wirelab_core import decode

random.seed(0xA0D10)
ok = lambda name: print(f"  PASS  {name}")


def hexframes(frames):
    return [f.hex() for f in frames]


# ── Law A: packetize → reassemble is identity (in-order) ──────────────────────
def reassemble_all(frames):
    r = AudioReassembler()
    out = []
    for f in frames:
        out += r.push(f)
    out += r.finalize()
    return out


def make_case(codec_id, size, packet_id, ts_us, src, dst, flags=0):
    payload = bytes(random.randrange(256) for _ in range(size))
    frames = packetize(codec_id, payload, packet_id, ts_us, src, dst, flags)
    return payload, frames


framing_cases = []
sizes = [0, 1, 4, 7, 60, 120, MAX_PAYLOAD]
codecs = [CODEC_OPUS, CODEC_PCM_DIAG, CODEC_FAUST_PM]
for i, size in enumerate(sizes):
    codec_id = codecs[i % len(codecs)]
    packet_id = (i * 37) % (MAX_PACKET_ID + 1)
    ts_us = (i * 0x012345) % (1 << 24)
    flags = FLAG_END_TALKSPURT if size == 0 else 0
    payload, frames = make_case(codec_id, size, packet_id, ts_us, 0x0001, 0xFFFF, flags)

    # frame count law:  1 descriptor + ceil(len/4) data frames
    assert len(frames) == 1 + (len(payload) + 3) // 4
    # every emitted frame is an ordinary valid CTRL DeModFrame
    for f in frames:
        d = decode(f)
        assert d["frame_type"] == FCTRL
        assert (d["seq"] >> FRAG_BITS) == packet_id
    # identity
    events = reassemble_all(frames)
    assert events == [("packet", packet_id, ts_us, codec_id, payload, flags)], events

    framing_cases.append({
        "codec_id": codec_id, "src": 0x0001, "dst": 0xFFFF,
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
            _, pid, ts, cid, payload, flags = e
            packets.append({"packet_id": pid, "ts_us": ts, "codec_id": cid,
                            "payload": payload.hex(), "flags": flags})
        else:
            lost.append(e[1])
    return packets, lost


reassembly_cases = []
# Two source packets to interleave.
p0_payload, p0 = make_case(CODEC_PCM_DIAG, 10, 5, 0x010203, 0x0001, 0x0002)
p1_payload, p1 = make_case(CODEC_FAUST_PM, 8, 6, 0x010210, 0x0001, 0x0002)

# 1. in-order
seq_in = p0 + p1
# 2. reordered (deterministic shuffle)
seq_re = list(seq_in)
random.Random(0xBEEF).shuffle(seq_re)
# 3. frag drop -> lost (drop one data fragment of p0)
seq_drop = [f for j, f in enumerate(p0) if j != 2] + p1
# 4. duplicate (dup the descriptor and one data frag of p0)
seq_dup = [p0[0], p0[0], p0[1], p0[1]] + p0[2:] + p1

for name, stream in [("in_order", seq_in), ("reordered", seq_re),
                     ("frag_drop_lost", seq_drop), ("duplicate", seq_dup)]:
    events = reassemble_all(stream)
    packets, lost = events_to_json(events)
    reassembly_cases.append({"name": name, "input_frames": hexframes(stream),
                             "packets": packets, "lost": lost})

# sanity on the four scenarios
assert reassembly_cases[0]["lost"] == [] and len(reassembly_cases[0]["packets"]) == 2
assert reassembly_cases[1]["lost"] == [] and len(reassembly_cases[1]["packets"]) == 2
assert reassembly_cases[2]["lost"] == [5] and len(reassembly_cases[2]["packets"]) == 1
assert reassembly_cases[3]["lost"] == [] and len(reassembly_cases[3]["packets"]) == 2
ok("reassembly correct under reorder, fragment-drop→lost, and duplicate")


# ── Law C: PCM-diag decode∘encode is byte-lossless ────────────────────────────
pcm_blocks = [
    bytes(range(PCM_DIAG_BLOCK)),                 # ramp 0..119
    bytes([0xFF] * PCM_DIAG_BLOCK),               # rail high
    bytes([0x00] * PCM_DIAG_BLOCK),               # rail low
    bytes(random.randrange(256) for _ in range(PCM_DIAG_BLOCK)),
]
pcm_roundtrip = []
for blk in pcm_blocks:
    assert pcm_diag_encode(pcm_diag_decode(blk)) == blk
    pcm_roundtrip.append({"bytes": blk.hex()})
ok(f"PCM-diag decode∘encode = id on {len(pcm_blocks)} blocks ({PCM_DIAG_BLOCK} B each)")


# ── Law D: PM param pack/unpack round-trip ────────────────────────────────────
pm_cases_in = [
    {"f0": 0, "amp": 0, "mod_index": 0, "mod_ratio": 0, "bright": 0, "env": 0, "flags": 0},
    {"f0": 0xFFFF, "amp": 255, "mod_index": 255, "mod_ratio": 255, "bright": 255, "env": 255, "flags": 255},
    {"f0": 440, "amp": 200, "mod_index": 64, "mod_ratio": 2, "bright": 128, "env": 96, "flags": 0x02},
    {"f0": 0x1234, "amp": 17, "mod_index": 99, "mod_ratio": 5, "bright": 33, "env": 7, "flags": 1},
]
pm_param_cases = []
for p in pm_cases_in:
    b = pm_pack(p)
    assert pm_unpack(b) == p
    assert len(b) == 8
    pm_param_cases.append({"params": p, "bytes": b.hex()})
ok(f"PM param pack/unpack = id on {len(pm_param_cases)} cases (8 B block)")


# ── Anchor: a worked example audio packet ─────────────────────────────────────
anchor_payload = bytes.fromhex("DEADBEEF1234")        # 6 bytes
anchor_frames = packetize(CODEC_PCM_DIAG, anchor_payload, 0x10, 0x010203, 0x0001, 0xFFFF)

audio_vectors = {
    "format": "DCF-Audio L2 framing v1 (adapter over the 17-byte DeModFrame quantum)",
    "spec": "seq = packet_id[15:5] | frag_idx[4:0]; frag_idx 0 = [len,frag_total,codec_id,flags]; "
            "frag_idx 1..N = 4 payload bytes (last zero-padded); type=CTRL(3), version=1",
    "constants": {"frag_bits": FRAG_BITS, "max_frags": MAX_FRAGS, "max_payload": MAX_PAYLOAD,
                  "max_packet_id": MAX_PACKET_ID, "frame_type_ctrl": FCTRL},
    "anchors": {
        "exampleAudioPacket": {
            "codec_id": CODEC_PCM_DIAG, "packet_id": 0x10, "ts_us": 0x010203,
            "src": 0x0001, "dst": 0xFFFF, "payload": anchor_payload.hex(),
            "frames": hexframes(anchor_frames),
        }
    },
    "theorem": ("L2 framing is a fixed bit-placement adapter over the certified DeModFrame; matching "
                "these framing + reassembly + PCM + (separately) PM-param vectors pins the C and Rust "
                "implementations to this reference. codec_id is opaque to L2, so the framing vectors are "
                "invariant to codec choice."),
    "framing": framing_cases,
    "reassembly": reassembly_cases,
    "pcm_roundtrip": pcm_roundtrip,
}

pm_param_vectors = {
    "format": "DCF-Audio PM (Faust phase-mod, codec_id=2) 8-byte parameter block v1",
    "layout": ["f0_hi", "f0_lo", "amp", "mod_index", "mod_ratio", "bright", "env", "flags"],
    "cases": pm_param_cases,
}

# ── C header emitter (dependency-free vectors for the C cert test) ────────────
def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"


def emit_c_header():
    L = ['/* GENERATED by python/MCP/gen_audio_vectors.py — DO NOT EDIT. */',
         '#ifndef DCF_AUDIO_VECTORS_GEN_H', '#define DCF_AUDIO_VECTORS_GEN_H',
         '#include <stdint.h>', '']
    # framing
    L += ['typedef struct { uint8_t codec_id; uint16_t src, dst, packet_id;',
          '  uint32_t ts_us; uint8_t flags, payload_len, payload[124];',
          '  uint8_t n_frames; uint8_t frames[32][17]; } av_framing_t;',
          'static const av_framing_t AV_FRAMING[] = {']
    for c in framing_cases:
        pl = bytes.fromhex(c["payload"])
        frs = [bytes.fromhex(h) for h in c["frames"]]
        fr = ",".join(carr(f) for f in frs)
        L.append(f'  {{{c["codec_id"]},0x{c["src"]:04X},0x{c["dst"]:04X},'
                 f'0x{c["packet_id"]:04X},0x{c["ts_us"]:06X}u,{c["flags"]},'
                 f'{len(pl)},{carr(pl) if pl else "{0}"},{len(frs)},{{{fr}}}}},')
    L += ['};', 'static const int AV_N_FRAMING = (int)(sizeof(AV_FRAMING)/sizeof(AV_FRAMING[0]));', '']
    # reassembly
    L += ['typedef struct { uint16_t packet_id; uint32_t ts_us; uint8_t codec_id,'
          ' flags, payload_len, payload[124]; } av_pkt_t;',
          'typedef struct { const char *name; uint8_t n_in; uint8_t in_frames[16][17];',
          '  uint8_t n_pkt; av_pkt_t pkts[4]; uint8_t n_lost; uint16_t lost[4]; } av_reasm_t;',
          'static const av_reasm_t AV_REASM[] = {']
    for rc in reassembly_cases:
        ins = [bytes.fromhex(h) for h in rc["input_frames"]]
        inf = ",".join(carr(f) for f in ins)
        pkts = []
        for p in rc["packets"]:
            pl = bytes.fromhex(p["payload"])
            pkts.append(f'{{0x{p["packet_id"]:04X},0x{p["ts_us"]:06X}u,{p["codec_id"]},'
                        f'{p["flags"]},{len(pl)},{carr(pl) if pl else "{0}"}}}')
        lost = ",".join(str(x) for x in rc["lost"]) or "0"
        L.append(f'  {{"{rc["name"]}",{len(ins)},{{{inf}}},{len(rc["packets"])},'
                 f'{{{",".join(pkts) if pkts else "{0}"}}},{len(rc["lost"])},{{{lost}}}}},')
    L += ['};', 'static const int AV_N_REASM = (int)(sizeof(AV_REASM)/sizeof(AV_REASM[0]));', '']
    # pcm round-trip
    L += ['static const uint8_t AV_PCM[][120] = {']
    for blk in pcm_roundtrip:
        L.append('  ' + carr(bytes.fromhex(blk["bytes"])) + ',')
    L += ['};', 'static const int AV_N_PCM = (int)(sizeof(AV_PCM)/sizeof(AV_PCM[0]));', '']
    # pm params
    L += ['typedef struct { uint16_t f0; uint8_t amp, mod_index, mod_ratio, bright,'
          ' env, flags; uint8_t bytes[8]; } av_pm_t;',
          'static const av_pm_t AV_PM[] = {']
    for pc in pm_param_cases:
        p = pc["params"]
        L.append(f'  {{0x{p["f0"]:04X},{p["amp"]},{p["mod_index"]},{p["mod_ratio"]},'
                 f'{p["bright"]},{p["env"]},{p["flags"]},{carr(bytes.fromhex(pc["bytes"]))}}},')
    L += ['};', 'static const int AV_N_PM = (int)(sizeof(AV_PM)/sizeof(AV_PM[0]));', '',
          '#endif /* DCF_AUDIO_VECTORS_GEN_H */', '']
    return "\n".join(L)


out_audio = sys.argv[1] if len(sys.argv) > 1 else "audio_vectors.json"
out_dir = os.path.dirname(out_audio) or "."
out_pm = os.path.join(out_dir, "pm_param_vectors.json")
out_h = os.path.join(out_dir, "audio_vectors.gen.h")
with open(out_audio, "w") as fh:
    json.dump(audio_vectors, fh, indent=1)
with open(out_pm, "w") as fh:
    json.dump(pm_param_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_audio} ({os.path.getsize(out_audio)} bytes, "
      f"{len(framing_cases)} framing, {len(reassembly_cases)} reassembly, {len(pcm_roundtrip)} pcm)")
print(f"  INFO  wrote {out_pm} ({os.path.getsize(out_pm)} bytes, {len(pm_param_cases)} pm cases)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL AUDIO LAWS HOLD")
