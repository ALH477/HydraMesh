# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for the DCF-SSTV L2 framing.

Mirrors gen_text_vectors.py: it first asserts the framing/reassembly laws hold, then
emits the finite vectors that the C, Rust, Go and Node implementations certify against
byte-for-byte.  SSTV is an adapter over the 17-byte DeModFrame, so none of this touches
the 246-vector wire certificate.

Usage:  python3 gen_sstv_vectors.py [sstv_vectors.json]
  Writes  <path>                    (sstv framing + reassembly)
  and     <dir>/sstv_vectors.gen.h  (dependency-free C test header)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from sstvlab_core import (
    packetize, SstvReassembler, channel_id,
    FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_IMAGE_ID, FDATA,
    FMT_RAW, FMT_JPEG, FMT_PNG, FMT_WEBP, FMT_RGB565,
    FLAG_MORE, FLAG_KEYFRAME, FLAG_RELIABLE,
)
from wirelab_core import decode

random.seed(0x5527)
ok = lambda name: print(f"  PASS  {name}")

# Bound the dependency-free C header: cases larger than this are JSON-only (Python +
# Rust still assert them).  Keeps the static C arrays small / embeddable.
C_HEADER_MAX_PAYLOAD = 128


def hexframes(frames):
    return [f.hex() for f in frames]


# ── Law A: packetize → reassemble is identity (in-order) ──────────────────────
def reassemble_all(frames):
    r = SstvReassembler()
    out = []
    for f in frames:
        out += r.push(f)
    out += r.finalize()
    return out


def make_case(size, image_id, ts_us, src, dst, format_id, flags=0):
    payload = bytes(random.randrange(0, 256) for _ in range(size))
    frames = packetize(payload, image_id, ts_us, src, dst, format_id, flags)
    return payload, frames


framing_cases = []
# sizes exercise: empty, 1, exact 4, non-multiples of 4 (un-pad law), the 124 B (text/game
# cross-over), >1024 frags (11-bit index), and the 8188 B / 2047-frag rail.  Mixed
# format_id + flags prove both are opaque to the framing bytes.
sizes = [0, 1, 4, 7, 13, 124, 257, 5000, MAX_PAYLOAD]
case_fmt = [FMT_RAW, FMT_JPEG, FMT_PNG, FMT_WEBP, FMT_RGB565, FMT_JPEG, FMT_RAW, FMT_PNG, FMT_JPEG]
case_flags = [0, FLAG_KEYFRAME, FLAG_MORE, FLAG_KEYFRAME | FLAG_RELIABLE, 0,
              FLAG_RELIABLE, FLAG_KEYFRAME, FLAG_MORE, FLAG_KEYFRAME]
for i, size in enumerate(sizes):
    image_id = (i * 5) % (MAX_IMAGE_ID + 1)
    ts_us = (i * 0x012345) % (1 << 24)
    fmt = case_fmt[i]
    flags = case_flags[i]
    payload, frames = make_case(size, image_id, ts_us, 0x0001, 0xFFFF, fmt, flags)

    # frame count law:  1 descriptor + ceil(len/4) data frames
    assert len(frames) == 1 + (len(payload) + 3) // 4
    # every emitted frame is an ordinary valid DATA DeModFrame on this image_id
    for f in frames:
        d = decode(f)
        assert d["frame_type"] == FDATA
        assert (d["seq"] >> FRAG_BITS) == image_id
    # descriptor law: byte 0/1 are the true (unpadded) length hi/lo, byte 2 format_id, 3 flags
    d0 = decode(frames[0])
    desc = bytes.fromhex(d0["payload"])
    assert (desc[0] << 8 | desc[1]) == len(payload) and desc[2] == fmt and desc[3] == flags
    # identity
    events = reassemble_all(frames)
    assert events == [("image", image_id, ts_us, 0x0001, 0xFFFF, fmt, payload, flags)], \
        (len(payload), events[:1])

    framing_cases.append({
        "src": 0x0001, "dst": 0xFFFF, "image_id": image_id, "ts_us": ts_us,
        "format_id": fmt, "flags": flags, "payload": payload.hex(),
        "frames": hexframes(frames),
    })
ok(f"packetize→reassemble = id on {len(framing_cases)} framing cases (sizes {sizes})")

# bounds law
try:
    packetize(bytes(MAX_PAYLOAD + 1), 0, 0, 1, 1)
    raise AssertionError("oversize image must be rejected")
except ValueError:
    pass
try:
    packetize(bytes(4), MAX_IMAGE_ID + 1, 0, 1, 1)
    raise AssertionError("image_id past the 5-bit field must be rejected")
except ValueError:
    pass
ok(f"image > {MAX_PAYLOAD}B rejected; frag field is {FRAG_BITS} bits (max {MAX_FRAGS} frags); "
   f"image_id capped at {MAX_IMAGE_ID}")

# channel rendezvous anchor (same crc16 hash the rest of the repo uses)
assert channel_id("123456789") == 0x29B1
assert channel_id(None) == 0xFFFF
ok("channel_id = crc16_ccitt rendezvous hash (anchor \"123456789\" -> 0x29B1)")


# ── Law B: reassembly under reorder / drop / duplicate ────────────────────────
def events_to_json(events):
    images, lost = [], []
    for e in events:
        if e[0] == "image":
            _, iid, ts, src, dst, fmt, data, flags = e
            images.append({"image_id": iid, "ts_us": ts, "src": src, "dst": dst,
                           "format_id": fmt, "flags": flags, "payload": data.hex()})
        else:
            lost.append(e[1])
    return images, lost


reassembly_cases = []
# Two source images to interleave (distinct image_ids on the same channel).
img0 = bytes((i * 11) & 0xFF for i in range(30))   # 30 bytes -> 8 data frags
img1 = b"\xff\xd8\xff\xe0tiny"                       # 8 bytes  -> 2 data frags
p0 = packetize(img0, 5, 0x010203, 0x0001, 0x0002, FMT_JPEG, FLAG_KEYFRAME)
p1 = packetize(img1, 6, 0x010210, 0x0001, 0x0002, FMT_JPEG, FLAG_KEYFRAME | FLAG_RELIABLE)

seq_in = p0 + p1                                    # 1. in-order
seq_re = list(seq_in)                               # 2. reordered (deterministic)
random.Random(0xBEEF).shuffle(seq_re)
seq_drop = [f for j, f in enumerate(p0) if j != 2] + p1   # 3. drop one frag of img0 -> lost
seq_dup = [p0[0], p0[0], p0[1], p0[1]] + p0[2:] + p1      # 4. duplicate

for name, stream in [("in_order", seq_in), ("reordered", seq_re),
                     ("frag_drop_lost", seq_drop), ("duplicate", seq_dup)]:
    events = reassemble_all(stream)
    images, lost = events_to_json(events)
    reassembly_cases.append({"name": name, "input_frames": hexframes(stream),
                             "images": images, "lost": lost})

assert reassembly_cases[0]["lost"] == [] and len(reassembly_cases[0]["images"]) == 2
assert reassembly_cases[1]["lost"] == [] and len(reassembly_cases[1]["images"]) == 2
assert reassembly_cases[2]["lost"] == [5] and len(reassembly_cases[2]["images"]) == 1
assert reassembly_cases[3]["lost"] == [] and len(reassembly_cases[3]["images"]) == 2
ok("reassembly correct under reorder, fragment-drop→lost, and duplicate")


# ── Anchor: a worked example ──────────────────────────────────────────────────
anchor_image = bytes.fromhex("ffd8ffe000104a46494600010100")  # a JPEG SOI/APP0 stub
anchor_frames = packetize(anchor_image, 0x0A, 0x010203, 0x00A1, channel_id("sstv"),
                          FMT_JPEG, FLAG_KEYFRAME | FLAG_RELIABLE)

sstv_vectors = {
    "format": "DCF-SSTV L2 framing v1 (adapter over the 17-byte DeModFrame quantum)",
    "spec": "seq = image_id[15:11] | frag_idx[10:0]; frag_idx 0 = [len_hi,len_lo,format_id,flags]; "
            "frag_idx 1..N = 4 payload bytes (last zero-padded); type=DATA(0), version=1",
    "constants": {"frag_bits": FRAG_BITS, "max_frags": MAX_FRAGS, "max_payload": MAX_PAYLOAD,
                  "max_image_id": MAX_IMAGE_ID, "frame_type_data": FDATA,
                  "format_raw": FMT_RAW, "format_jpeg": FMT_JPEG, "format_png": FMT_PNG,
                  "format_webp": FMT_WEBP, "format_rgb565": FMT_RGB565,
                  "flag_more": FLAG_MORE, "flag_keyframe": FLAG_KEYFRAME,
                  "flag_reliable": FLAG_RELIABLE},
    "anchors": {
        "exampleImage": {
            "image_id": 0x0A, "ts_us": 0x010203, "src": 0x00A1,
            "dst": channel_id("sstv"), "format_id": FMT_JPEG,
            "flags": FLAG_KEYFRAME | FLAG_RELIABLE,
            "payload": anchor_image.hex(), "frames": hexframes(anchor_frames),
        }
    },
    "theorem": ("L2 framing is a fixed bit-placement adapter over the certified DeModFrame; "
                "matching these framing + reassembly vectors pins the C, Rust, Go and Node "
                "implementations to this reference.  The descriptor format_id and flags are "
                "opaque to L2, so the framing vectors are invariant to the format/flag choice."),
    "framing": framing_cases,
    "reassembly": reassembly_cases,
}


# ── C header emitter (dependency-free vectors for the C cert test) ────────────
def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"


def emit_c_header():
    # only cases that fit the bounded C arrays (the 8188 B rail is JSON-only)
    cframing = [c for c in framing_cases
                if len(bytes.fromhex(c["payload"])) <= C_HEADER_MAX_PAYLOAD]
    fcap = C_HEADER_MAX_PAYLOAD                       # payload bytes per case
    framecap = 1 + (C_HEADER_MAX_PAYLOAD + 3) // 4    # frames per case (1 + ceil/4)
    L = ['/* GENERATED by python/MCP/gen_sstv_vectors.py — DO NOT EDIT. */',
         '#ifndef DCF_SSTV_VECTORS_GEN_H', '#define DCF_SSTV_VECTORS_GEN_H',
         '#include <stdint.h>', '']
    # framing
    L += [f'typedef struct {{ uint16_t src, dst, image_id; uint32_t ts_us;',
          f'  uint8_t format_id, flags; uint16_t payload_len; uint8_t payload[{fcap}];',
          f'  uint8_t n_frames; uint8_t frames[{framecap}][17]; }} sv_framing_t;',
          'static const sv_framing_t SV_FRAMING[] = {']
    for c in cframing:
        pl = bytes.fromhex(c["payload"])
        frs = [bytes.fromhex(h) for h in c["frames"]]
        fr = ",".join(carr(f) for f in frs)
        L.append(f'  {{0x{c["src"]:04X},0x{c["dst"]:04X},0x{c["image_id"]:04X},'
                 f'0x{c["ts_us"]:06X}u,{c["format_id"]},{c["flags"]},{len(pl)},'
                 f'{carr(pl) if pl else "{0}"},{len(frs)},{{{fr}}}}},')
    L += ['};', 'static const int SV_N_FRAMING = (int)(sizeof(SV_FRAMING)/sizeof(SV_FRAMING[0]));', '']
    # reassembly
    L += ['typedef struct { uint16_t image_id; uint32_t ts_us; uint16_t src, dst;',
          f'  uint8_t format_id, flags; uint16_t payload_len; uint8_t payload[{fcap}]; }} sv_img_t;',
          'typedef struct { const char *name; uint8_t n_in; uint8_t in_frames[16][17];',
          '  uint8_t n_img; sv_img_t imgs[4]; uint8_t n_lost; uint16_t lost[4]; } sv_reasm_t;',
          'static const sv_reasm_t SV_REASM[] = {']
    for rc in reassembly_cases:
        ins = [bytes.fromhex(h) for h in rc["input_frames"]]
        inf = ",".join(carr(f) for f in ins)
        imgs = []
        for m in rc["images"]:
            pl = bytes.fromhex(m["payload"])
            imgs.append(f'{{0x{m["image_id"]:04X},0x{m["ts_us"]:06X}u,0x{m["src"]:04X},'
                        f'0x{m["dst"]:04X},{m["format_id"]},{m["flags"]},{len(pl)},'
                        f'{carr(pl) if pl else "{0}"}}}')
        lost = ",".join(str(x) for x in rc["lost"]) or "0"
        L.append(f'  {{"{rc["name"]}",{len(ins)},{{{inf}}},{len(rc["images"])},'
                 f'{{{",".join(imgs) if imgs else "{0}"}}},{len(rc["lost"])},{{{lost}}}}},')
    L += ['};', 'static const int SV_N_REASM = (int)(sizeof(SV_REASM)/sizeof(SV_REASM[0]));', '',
          '#endif /* DCF_SSTV_VECTORS_GEN_H */', '']
    return "\n".join(L)


out_sstv = sys.argv[1] if len(sys.argv) > 1 else "sstv_vectors.json"
out_dir = os.path.dirname(out_sstv) or "."
out_h = os.path.join(out_dir, "sstv_vectors.gen.h")
with open(out_sstv, "w") as fh:
    json.dump(sstv_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_sstv} ({os.path.getsize(out_sstv)} bytes, "
      f"{len(framing_cases)} framing, {len(reassembly_cases)} reassembly)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL SSTV LAWS HOLD")
