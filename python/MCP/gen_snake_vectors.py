# SPDX-License-Identifier: LGPL-3.0-only
"""Executable laws + golden-vector generator for the DCF-Snake record-plane L2 framing,
the BEACON grandmaster media clock, and the ``unwrap_pid`` timeline primitive.

Mirrors gen_sstv_vectors.py: it first asserts the framing/reassembly/clock/unwrap laws
hold, then emits the finite vectors that the C and Rust implementations certify against
byte-for-byte.  DCF-Snake is an adapter over the 17-byte DeModFrame, so none of this
touches the 246-vector wire certificate.

Usage:  python3 gen_snake_vectors.py [snake_vectors.json]
  Writes  <path>                     (snake framing + reassembly + clock + unwrap)
  and     <dir>/snake_vectors.gen.h  (dependency-free C test header)
Exit 0 iff every law holds.  Commit identical copies to Documentation/ and python/MCP/.
"""
import json
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from snakelab_core import (
    packetize, beacon_packetize, SnakeReassembler, channel_id,
    pack_clock, unpack_clock, unwrap_pid,
    FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_STREAM_ID, FCTRL, FBEACON,
    MODE_LIVE, MODE_NEAR, MODE_RELAXED,
    FLAG_MORE, FLAG_ANCHOR, FLAG_END,
    CLOCK_LEN, CLOCK_VER, NOMINAL_RATE_MHZ_48K, PID_MOD,
)
from wirelab_core import decode

random.seed(0x5A4E)   # "SN"
ok = lambda name: print(f"  PASS  {name}")

# Bound the dependency-free C header: cases larger than this are JSON-only (Python + Rust
# still assert them).  Keeps the static C arrays small / embeddable — the QSS-scale packets
# (live ~784 / near ~1328 / relaxed ~2048 / the 8188 rail) exercise nothing new in the
# size-independent framing algorithm, so they stay JSON-only exactly like the SSTV rail.
C_HEADER_MAX_PAYLOAD = 128


def hexframes(frames):
    return [f.hex() for f in frames]


# ── Law A: packetize → reassemble is identity (in-order) ──────────────────────
def reassemble_all(frames, frame_type=FCTRL):
    r = SnakeReassembler(frame_type=frame_type)
    out = []
    for f in frames:
        out += r.push(f)
    out += r.finalize()
    return out


framing_cases = []
# sizes exercise: empty, 1, exact 4, non-multiples of 4 (un-pad law), the 124 B audio
# cross-over, the three QSS mode packet sizes (live ~784 / near ~1328 / relaxed ~2048), and
# the 8188 B / 2047-frag rail.  Mixed mode_id + flags prove both are opaque to the framing.
sizes = [0, 1, 4, 7, 13, 124, 784, 1328, 2048, MAX_PAYLOAD]
case_mode = [MODE_LIVE, MODE_NEAR, MODE_RELAXED, MODE_LIVE, MODE_NEAR,
             MODE_RELAXED, MODE_LIVE, MODE_NEAR, MODE_RELAXED, MODE_LIVE]
case_flags = [0, FLAG_ANCHOR, FLAG_MORE, FLAG_ANCHOR | FLAG_END, 0,
              FLAG_END, FLAG_ANCHOR, FLAG_MORE, FLAG_ANCHOR, FLAG_ANCHOR | FLAG_MORE]
for i, size in enumerate(sizes):
    stream_id = (i * 3) % (MAX_STREAM_ID + 1)
    ts_us = (i * 0x012345) % (1 << 24)
    mode = case_mode[i]
    flags = case_flags[i]
    payload = bytes(random.randrange(0, 256) for _ in range(size))
    frames = packetize(payload, stream_id, ts_us, 0x0001, 0xFFFF, mode, flags)

    # frame count law:  1 descriptor + ceil(len/4) data frames
    assert len(frames) == 1 + (len(payload) + 3) // 4
    # every emitted frame is an ordinary valid CTRL DeModFrame on this stream_id
    for f in frames:
        d = decode(f)
        assert d["frame_type"] == FCTRL
        assert (d["seq"] >> FRAG_BITS) == stream_id
    # descriptor law: byte 0/1 = true (unpadded) length hi/lo, byte 2 mode_id, byte 3 flags
    d0 = decode(frames[0])
    desc = bytes.fromhex(d0["payload"])
    assert (desc[0] << 8 | desc[1]) == len(payload) and desc[2] == mode and desc[3] == flags
    # identity
    events = reassemble_all(frames)
    assert events == [("message", stream_id, ts_us, 0x0001, 0xFFFF, mode, payload, flags)], \
        (len(payload), events[:1])

    framing_cases.append({
        "src": 0x0001, "dst": 0xFFFF, "stream_id": stream_id, "ts_us": ts_us,
        "mode_id": mode, "flags": flags, "payload": payload.hex(),
        "frames": hexframes(frames),
    })
ok(f"packetize→reassemble = id on {len(framing_cases)} framing cases (sizes {sizes})")

# bounds law
try:
    packetize(bytes(MAX_PAYLOAD + 1), 0, 0, 1, 1)
    raise AssertionError("oversize message must be rejected")
except ValueError:
    pass
try:
    packetize(bytes(4), MAX_STREAM_ID + 1, 0, 1, 1)
    raise AssertionError("stream_id past the 5-bit field must be rejected")
except ValueError:
    pass
ok(f"message > {MAX_PAYLOAD}B rejected; frag field is {FRAG_BITS} bits (max {MAX_FRAGS} frags); "
   f"stream_id capped at {MAX_STREAM_ID}")

# channel rendezvous anchor (same crc16 hash the rest of the repo uses)
assert channel_id("123456789") == 0x29B1
assert channel_id(None) == 0xFFFF
ok("channel_id = crc16_ccitt rendezvous hash (anchor \"123456789\" -> 0x29B1)")


# ── Law B: reassembly under reorder / drop / duplicate ────────────────────────
def events_to_json(events):
    msgs, lost = [], []
    for e in events:
        if e[0] == "message":
            _, sid, ts, src, dst, mode, data, flags = e
            msgs.append({"stream_id": sid, "ts_us": ts, "src": src, "dst": dst,
                         "mode_id": mode, "flags": flags, "payload": data.hex()})
        else:
            lost.append(e[1])
    return msgs, lost


reassembly_cases = []
msg0 = bytes((i * 11) & 0xFF for i in range(30))   # 30 bytes -> 8 data frags
msg1 = b"\xa5\x5a\x00tinyQSS"                        # 10 bytes -> 3 data frags
p0 = packetize(msg0, 5, 0x010203, 0x0001, 0x0002, MODE_LIVE, FLAG_ANCHOR)
p1 = packetize(msg1, 6, 0x010210, 0x0001, 0x0002, MODE_LIVE, FLAG_ANCHOR | FLAG_END)

seq_in = p0 + p1                                    # 1. in-order
seq_re = list(seq_in)                               # 2. reordered (deterministic)
random.Random(0xBEEF).shuffle(seq_re)
seq_drop = [f for j, f in enumerate(p0) if j != 2] + p1   # 3. drop one frag of msg0 -> lost
seq_dup = [p0[0], p0[0], p0[1], p0[1]] + p0[2:] + p1      # 4. duplicate

for name, stream in [("in_order", seq_in), ("reordered", seq_re),
                     ("frag_drop_lost", seq_drop), ("duplicate", seq_dup)]:
    events = reassemble_all(stream)
    msgs, lost = events_to_json(events)
    reassembly_cases.append({"name": name, "input_frames": hexframes(stream),
                             "messages": msgs, "lost": lost})

assert reassembly_cases[0]["lost"] == [] and len(reassembly_cases[0]["messages"]) == 2
assert reassembly_cases[1]["lost"] == [] and len(reassembly_cases[1]["messages"]) == 2
assert reassembly_cases[2]["lost"] == [5] and len(reassembly_cases[2]["messages"]) == 1
assert reassembly_cases[3]["lost"] == [] and len(reassembly_cases[3]["messages"]) == 2
ok("reassembly correct under reorder, fragment-drop→lost, and duplicate")


# ── Law C: BEACON grandmaster clock pack/unpack + fragmented framing ──────────
clock_cases = []
clock_inputs = [
    (0, NOMINAL_RATE_MHZ_48K, 0, 0),
    (0x0123456789ABCDEF, NOMINAL_RATE_MHZ_48K, 0x1234, 0x0007),
    (48_000, 96_000_000, 0xFFFF, 0x0001),                     # 96 kHz, 1 s of samples
    ((1 << 64) - 1, (1 << 32) - 1, 0xABCD, 0xFFFF),           # saturation
]
for j, (gsc, rate, txs, ep) in enumerate(clock_inputs):
    payload = pack_clock(gsc, rate, txs, ep)
    assert len(payload) == CLOCK_LEN
    assert unpack_clock(payload) == {"gm_sample_count": gsc, "nominal_rate_mhz": rate,
                                     "tx_seq": txs, "epoch": ep}
    beacon_slot = j % (MAX_STREAM_ID + 1)
    ts_us = (0x00A000 + j) & 0xFFFFFF
    frames = beacon_packetize(payload, beacon_slot, ts_us, 0x00A1, 0xFFFF)
    assert len(frames) == 1 + CLOCK_LEN // 4          # descriptor + 4 data = 5 frames
    for f in frames:
        assert decode(f)["frame_type"] == FBEACON
    ev = reassemble_all(frames, frame_type=FBEACON)
    assert ev == [("message", beacon_slot, ts_us, 0x00A1, 0xFFFF, CLOCK_VER, payload, 0)]
    clock_cases.append({
        "gm_sample_count": gsc, "nominal_rate_mhz": rate, "tx_seq": txs, "epoch": ep,
        "payload": payload.hex(), "beacon_slot": beacon_slot, "ts_us": ts_us,
        "src": 0x00A1, "dst": 0xFFFF, "frames": hexframes(frames),
    })
ok(f"clock pack/unpack identity + fragmented BEACON framing on {len(clock_cases)} cases")


# ── Law D: unwrap_pid (ported from sync.rs) — forward / rollover / reorder ─────
unwrap_cases = []
unwrap_inputs = [
    (0, 1, PID_MOD),          # forward
    (5, 5, PID_MOD),          # same id -> no progress
    (2047, 0, PID_MOD),       # rollover forward -> 2048
    (2040, 5, PID_MOD),       # wrap-around forward
    (100, 98, PID_MOD),       # small backward -> reorder step-back
    (0, 2047, PID_MOD),       # far backward -> saturating_sub to 0
    (1000, 1500, PID_MOD),    # forward jump (gap)
    (5000, 3, PID_MOD),       # high prev, forward across wrap
]
for prev_abs, raw, mod in unwrap_inputs:
    expect = unwrap_pid(prev_abs, raw, mod)
    unwrap_cases.append({"prev_abs": prev_abs, "raw": raw, "mod": mod, "expect": expect})
# spot-check the known values
assert unwrap_pid(0, 1) == 1 and unwrap_pid(2047, 0) == 2048
assert unwrap_pid(100, 98) == 98 and unwrap_pid(0, 2047) == 0
ok(f"unwrap_pid matches sync.rs on {len(unwrap_cases)} forward/rollover/reorder cases")


# ── Anchor: a worked example ──────────────────────────────────────────────────
anchor_pkt = bytes.fromhex("a55a0000000100050003000a" + "de" * 8)   # a QSS-ish stub packet
anchor_frames = packetize(anchor_pkt, 0x05, 0x010203, 0x00A1, channel_id("snake"),
                          MODE_LIVE, FLAG_ANCHOR)

snake_vectors = {
    "format": "DCF-Snake record-plane L2 framing v1 (adapter over the 17-byte DeModFrame quantum)",
    "spec": "seq = stream_id[15:11] | frag_idx[10:0]; frag_idx 0 = [len_hi,len_lo,mode_id,flags]; "
            "frag_idx 1..N = 4 payload bytes (last zero-padded); type=CTRL(3), version=1.  "
            "BEACON clock = same 5:11 fragmenter on type=BEACON(2) carrying a 16-byte payload "
            "[gm_sample_count u64 | nominal_rate_mHz u32 | tx_seq u16 | epoch u16].",
    "constants": {"frag_bits": FRAG_BITS, "max_frags": MAX_FRAGS, "max_payload": MAX_PAYLOAD,
                  "max_stream_id": MAX_STREAM_ID, "frame_type_ctrl": FCTRL,
                  "frame_type_beacon": FBEACON, "mode_live": MODE_LIVE, "mode_near": MODE_NEAR,
                  "mode_relaxed": MODE_RELAXED, "flag_more": FLAG_MORE, "flag_anchor": FLAG_ANCHOR,
                  "flag_end": FLAG_END, "clock_len": CLOCK_LEN, "clock_ver": CLOCK_VER,
                  "nominal_rate_mhz_48k": NOMINAL_RATE_MHZ_48K, "pid_mod": PID_MOD},
    "anchors": {
        "examplePacket": {
            "stream_id": 0x05, "ts_us": 0x010203, "src": 0x00A1,
            "dst": channel_id("snake"), "mode_id": MODE_LIVE, "flags": FLAG_ANCHOR,
            "payload": anchor_pkt.hex(), "frames": hexframes(anchor_frames),
        }
    },
    "theorem": ("L2 framing is a fixed bit-placement adapter over the certified DeModFrame; "
                "matching these framing + reassembly + clock + unwrap vectors pins the C and "
                "Rust implementations to this reference.  The descriptor mode_id and flags are "
                "opaque to L2, so the framing vectors are invariant to the mode/flag choice."),
    "framing": framing_cases,
    "reassembly": reassembly_cases,
    "clock": clock_cases,
    "unwrap": unwrap_cases,
}


# ── C header emitter (dependency-free vectors for the C cert test) ────────────
def carr(b):
    return "{" + ",".join(f"0x{x:02X}" for x in b) + "}"


def emit_c_header():
    cframing = [c for c in framing_cases
                if len(bytes.fromhex(c["payload"])) <= C_HEADER_MAX_PAYLOAD]
    fcap = C_HEADER_MAX_PAYLOAD
    framecap = 1 + (C_HEADER_MAX_PAYLOAD + 3) // 4
    L = ['/* GENERATED by python/MCP/gen_snake_vectors.py — DO NOT EDIT. */',
         '#ifndef DCF_SNAKE_VECTORS_GEN_H', '#define DCF_SNAKE_VECTORS_GEN_H',
         '#include <stdint.h>', '']
    # framing
    L += [f'typedef struct {{ uint16_t src, dst, stream_id; uint32_t ts_us;',
          f'  uint8_t mode_id, flags; uint16_t payload_len; uint8_t payload[{fcap}];',
          f'  uint8_t n_frames; uint8_t frames[{framecap}][17]; }} sn_framing_t;',
          'static const sn_framing_t SN_FRAMING[] = {']
    for c in cframing:
        pl = bytes.fromhex(c["payload"])
        frs = [bytes.fromhex(h) for h in c["frames"]]
        fr = ",".join(carr(f) for f in frs)
        L.append(f'  {{0x{c["src"]:04X},0x{c["dst"]:04X},0x{c["stream_id"]:04X},'
                 f'0x{c["ts_us"]:06X}u,{c["mode_id"]},{c["flags"]},{len(pl)},'
                 f'{carr(pl) if pl else "{0}"},{len(frs)},{{{fr}}}}},')
    L += ['};', 'static const int SN_N_FRAMING = (int)(sizeof(SN_FRAMING)/sizeof(SN_FRAMING[0]));', '']
    # reassembly
    L += ['typedef struct { uint16_t stream_id; uint32_t ts_us; uint16_t src, dst;',
          f'  uint8_t mode_id, flags; uint16_t payload_len; uint8_t payload[{fcap}]; }} sn_msg_t;',
          'typedef struct { const char *name; uint8_t n_in; uint8_t in_frames[16][17];',
          '  uint8_t n_msg; sn_msg_t msgs[4]; uint8_t n_lost; uint16_t lost[4]; } sn_reasm_t;',
          'static const sn_reasm_t SN_REASM[] = {']
    for rc in reassembly_cases:
        ins = [bytes.fromhex(h) for h in rc["input_frames"]]
        inf = ",".join(carr(f) for f in ins)
        msgs = []
        for m in rc["messages"]:
            pl = bytes.fromhex(m["payload"])
            msgs.append(f'{{0x{m["stream_id"]:04X},0x{m["ts_us"]:06X}u,0x{m["src"]:04X},'
                        f'0x{m["dst"]:04X},{m["mode_id"]},{m["flags"]},{len(pl)},'
                        f'{carr(pl) if pl else "{0}"}}}')
        lost = ",".join(str(x) for x in rc["lost"]) or "0"
        L.append(f'  {{"{rc["name"]}",{len(ins)},{{{inf}}},{len(rc["messages"])},'
                 f'{{{",".join(msgs) if msgs else "{0}"}}},{len(rc["lost"])},{{{lost}}}}},')
    L += ['};', 'static const int SN_N_REASM = (int)(sizeof(SN_REASM)/sizeof(SN_REASM[0]));', '']
    # clock
    L += ['typedef struct { uint64_t gm_sample_count; uint32_t nominal_rate_mhz;',
          '  uint16_t tx_seq, epoch; uint8_t payload[16]; uint16_t beacon_slot;',
          '  uint32_t ts_us; uint16_t src, dst; uint8_t n_frames; uint8_t frames[5][17]; } sn_clock_t;',
          'static const sn_clock_t SN_CLOCK[] = {']
    for c in clock_cases:
        pl = bytes.fromhex(c["payload"])
        frs = [bytes.fromhex(h) for h in c["frames"]]
        fr = ",".join(carr(f) for f in frs)
        L.append(f'  {{0x{c["gm_sample_count"]:016X}ULL,0x{c["nominal_rate_mhz"]:08X}u,'
                 f'0x{c["tx_seq"]:04X},0x{c["epoch"]:04X},{carr(pl)},0x{c["beacon_slot"]:04X},'
                 f'0x{c["ts_us"]:06X}u,0x{c["src"]:04X},0x{c["dst"]:04X},{len(frs)},{{{fr}}}}},')
    L += ['};', 'static const int SN_N_CLOCK = (int)(sizeof(SN_CLOCK)/sizeof(SN_CLOCK[0]));', '']
    # unwrap
    L += ['typedef struct { uint64_t prev_abs; uint16_t raw; uint32_t mod; uint64_t expect; } sn_unwrap_t;',
          'static const sn_unwrap_t SN_UNWRAP[] = {']
    for u in unwrap_cases:
        L.append(f'  {{{u["prev_abs"]}ULL,{u["raw"]},{u["mod"]}u,{u["expect"]}ULL}},')
    L += ['};', 'static const int SN_N_UNWRAP = (int)(sizeof(SN_UNWRAP)/sizeof(SN_UNWRAP[0]));', '',
          '#endif /* DCF_SNAKE_VECTORS_GEN_H */', '']
    return "\n".join(L)


out_snake = sys.argv[1] if len(sys.argv) > 1 else "snake_vectors.json"
out_dir = os.path.dirname(out_snake) or "."
out_h = os.path.join(out_dir, "snake_vectors.gen.h")
with open(out_snake, "w") as fh:
    json.dump(snake_vectors, fh, indent=1)
with open(out_h, "w") as fh:
    fh.write(emit_c_header())

print(f"  INFO  wrote {out_snake} ({os.path.getsize(out_snake)} bytes, "
      f"{len(framing_cases)} framing, {len(reassembly_cases)} reassembly, "
      f"{len(clock_cases)} clock, {len(unwrap_cases)} unwrap)")
print(f"  INFO  wrote {out_h} ({os.path.getsize(out_h)} bytes)")
print("ALL SNAKE LAWS HOLD")
