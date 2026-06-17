# SPDX-License-Identifier: LGPL-3.0-only
"""DCF GameLab core — reference for the DCF-Game L2 framing (the certified layer).

DCF-Game is an *adapter* over the 17-byte DeModFrame wire quantum, exactly like
DCF-Audio: one game message (a state snapshot, an input frame, or an opaque event)
is serialised into 1 + frag_total ordinary DATA frames.  This layer is message-type
agnostic and byte-deterministic across C / Rust / Python — `msg_type_id` lives only
in the descriptor frame, so adding message types never changes these vectors.

L2 framing (all frames version=1, type=DATA(0), big-endian; see WIRE_QUANTUM_SPEC.md):
  seq (u16) = packet_id[15:5] (11 bits) | frag_idx[4:0] (5 bits)
  frag_idx == 0  descriptor : payload = [payload_len, frag_total, msg_type_id, flags]
  frag_idx 1..N  data       : payload = bytes[(k-1)*4 .. +4], last frame zero-padded
  timestamp_us   = 24-bit send time, identical across a message's frames
  frag_total     = ceil(payload_len / 4)   (<= 31  =>  payload_len <= 124 bytes / message)

Audio uses CTRL(3); game uses DATA(0) so the two adapters never collide on the wire.
This module imports the certified wire codec from wirelab_core so the emitted bytes
are provably the same DeModFrame the 246-vector wire certificate already pins.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from wirelab_core import encode, decode  # certified DeModFrame codec

# ── L2 framing constants ──────────────────────────────────────────────────────
FDATA = 0              # frame type carrying fragmented game messages
FRAG_BITS = 5          # low bits of seq hold the fragment index
FRAG_MASK = (1 << FRAG_BITS) - 1        # 0x1F
MAX_FRAGS = FRAG_MASK                   # 31 data fragments
MAX_PAYLOAD = MAX_FRAGS * 4             # 124 bytes / message
MAX_PACKET_ID = (1 << (16 - FRAG_BITS)) - 1   # 2047

# ── Message-type registry ids (profiles live in DCF_GAME_SPEC.md) ─────────────
GMSG_SNAPSHOT = 0      # packed player state (pos/vel/yaw)   — byte-deterministic
GMSG_INPUT = 1         # input bitfield + tick               — byte-deterministic
GMSG_EVENT = 2         # opaque application bytes (score, hit, chat) — not certified
GMSG_JOIN = 3          # lobby membership (player id + name) — byte-deterministic
# ids 4..255 reserved for future message types

# Descriptor flags — request transport behaviour; do not change the L2 bytes.
FLAG_RELIABLE = 0x01   # ask the transport for ARQ retransmit
FLAG_ORDERED = 0x02    # deliver in packet_id order
FLAG_END_TICK = 0x04   # last message of a simulation tick


def _ceil_div(a, b):
    return (a + b - 1) // b


def packetize(msg_type_id, payload, packet_id, ts_us, src, dst, flags=0):
    """Serialise one game message (`payload`, 0..124 bytes) into a list of 17-byte
    DeModFrame DATA frames.  Returns [descriptor_frame, data_frame, ...]."""
    if not (0 <= msg_type_id < 256):
        raise ValueError("msg_type_id must be u8")
    if not (0 <= flags < 256):
        raise ValueError("flags must be u8")
    if not (0 <= packet_id <= MAX_PACKET_ID):
        raise ValueError(f"packet_id must be 0..{MAX_PACKET_ID}")
    if len(payload) > MAX_PAYLOAD:
        raise ValueError(f"payload {len(payload)}B exceeds {MAX_PAYLOAD}B/message cap")

    frag_total = _ceil_div(len(payload), 4)        # data fragments after descriptor
    frames = []

    # frag_idx 0 — descriptor
    desc_seq = (packet_id << FRAG_BITS) | 0
    desc_payload = bytes([len(payload), frag_total, msg_type_id, flags])
    frames.append(encode(FDATA, desc_seq, src, dst, desc_payload, ts_us))

    # frag_idx 1..frag_total — data, last chunk zero-padded to 4 bytes
    for k in range(1, frag_total + 1):
        chunk = payload[(k - 1) * 4:(k - 1) * 4 + 4]
        chunk = chunk + bytes(4 - len(chunk))
        seq = (packet_id << FRAG_BITS) | k
        frames.append(encode(FDATA, seq, src, dst, chunk, ts_us))

    return frames


class GameReassembler:
    """Stateful reassembler.  push() emits a completed message as soon as its
    descriptor and every data fragment have arrived; duplicates are ignored;
    finalize() reports any message still incomplete as lost.

    Emitted packet event: ("packet", packet_id, ts_us, msg_type_id, payload_bytes, flags)
    Lost event:           ("lost", packet_id)
    """

    def __init__(self):
        self._pkts = {}    # packet_id -> dict(desc, ts, frags{idx:4B})

    def push(self, frame):
        try:
            d = decode(frame)
        except ValueError:
            return []
        if d["frame_type"] != FDATA:
            return []
        seq = d["seq"]
        packet_id = seq >> FRAG_BITS
        frag_idx = seq & FRAG_MASK
        body = bytes.fromhex(d["payload"])

        entry = self._pkts.setdefault(packet_id, {"desc": None, "ts": d["ts_us"],
                                                  "frags": {}})
        entry["ts"] = d["ts_us"]
        if frag_idx == 0:
            if entry["desc"] is None:
                entry["desc"] = {"payload_len": body[0], "frag_total": body[1],
                                 "msg_type_id": body[2], "flags": body[3]}
        elif frag_idx not in entry["frags"]:
            entry["frags"][frag_idx] = body

        return self._try_emit(packet_id)

    def _try_emit(self, packet_id):
        entry = self._pkts.get(packet_id)
        if entry is None or entry["desc"] is None:
            return []
        desc = entry["desc"]
        need = set(range(1, desc["frag_total"] + 1))
        if not need.issubset(entry["frags"].keys()):
            return []
        raw = b"".join(entry["frags"][k] for k in range(1, desc["frag_total"] + 1))
        payload = raw[:desc["payload_len"]]
        del self._pkts[packet_id]
        return [("packet", packet_id, entry["ts"], desc["msg_type_id"], payload, desc["flags"])]

    def finalize(self):
        events = [("lost", pid) for pid in sorted(self._pkts.keys())]
        self._pkts.clear()
        return events


# ── SNAPSHOT body (msg_type 0): 14-byte player state, Q8.8 fixed-point ────────
# Layout (big-endian): x y z vx vy vz as i16 Q8.8 metres / metres-per-tick, then
# yaw as u16 mapping 0..2π over 0..65535.  Byte-deterministic — this layout is
# certified (unlike the opaque EVENT body).
SNAPSHOT_LEN = 14
SNAPSHOT_FIELDS = ("x", "y", "z", "vx", "vy", "vz", "yaw")


def _q88(v):
    """float metres -> i16 Q8.8, clamped."""
    q = int(round(v * 256.0))
    return max(-32768, min(32767, q)) & 0xFFFF


def _unq88(u):
    """u16 two's-complement Q8.8 -> float metres."""
    return (u - 0x10000 if u >= 0x8000 else u) / 256.0


def snapshot_pack(s):
    """dict{x,y,z,vx,vy,vz,yaw} -> 14 bytes.  yaw is an integer u16."""
    out = bytearray()
    for f in ("x", "y", "z", "vx", "vy", "vz"):
        out += _q88(float(s[f])).to_bytes(2, "big")
    out += (int(s["yaw"]) & 0xFFFF).to_bytes(2, "big")
    return bytes(out)


def snapshot_unpack(b):
    """14 bytes -> dict.  unpack∘pack is exact for Q8.8-representable inputs."""
    if len(b) != SNAPSHOT_LEN:
        raise ValueError("snapshot body must be exactly 14 bytes")
    vals = [int.from_bytes(b[i:i + 2], "big") for i in range(0, 12, 2)]
    return {
        "x": _unq88(vals[0]), "y": _unq88(vals[1]), "z": _unq88(vals[2]),
        "vx": _unq88(vals[3]), "vy": _unq88(vals[4]), "vz": _unq88(vals[5]),
        "yaw": int.from_bytes(b[12:14], "big"),
    }


# ── INPUT body (msg_type 1): 6 bytes — tick u32 + buttons u16 bitfield ────────
INPUT_LEN = 6


def input_pack(p):
    """dict{tick,buttons} -> 6 bytes.  Byte-deterministic."""
    return ((int(p["tick"]) & 0xFFFFFFFF).to_bytes(4, "big") +
            (int(p["buttons"]) & 0xFFFF).to_bytes(2, "big"))


def input_unpack(b):
    if len(b) != INPUT_LEN:
        raise ValueError("input body must be exactly 6 bytes")
    return {"tick": int.from_bytes(b[0:4], "big"),
            "buttons": int.from_bytes(b[4:6], "big")}


# ── JOIN body (msg_type 3): player_id u16 + len-prefixed UTF-8 name ───────────
def join_pack(j):
    """dict{player_id,name} -> 3 + len(name) bytes (name truncated to 121)."""
    name = j["name"].encode("utf-8")[:MAX_PAYLOAD - 3]
    return (int(j["player_id"]) & 0xFFFF).to_bytes(2, "big") + bytes([len(name)]) + name


def join_unpack(b):
    if len(b) < 3:
        raise ValueError("join body must be at least 3 bytes")
    n = b[2]
    return {"player_id": int.from_bytes(b[0:2], "big"),
            "name": b[3:3 + n].decode("utf-8", "replace")}
