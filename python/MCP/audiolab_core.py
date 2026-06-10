"""DCF AudioLab core — reference for the DCF-Audio L2 framing (the certified layer).

Audio is an *adapter* over the 17-byte DeModFrame wire quantum: a 20 ms codec block
is serialised into 1 + frag_total ordinary CTRL frames.  This layer is codec-agnostic
and byte-deterministic across C / Rust / Python — `codec_id` lives only in the
descriptor frame, so adding codecs never changes these vectors.

L2 framing (all frames version=1, type=CTRL(3), big-endian; see WIRE_QUANTUM_SPEC.md):
  seq (u16) = packet_id[15:5] (11 bits) | frag_idx[4:0] (5 bits)
  frag_idx == 0  descriptor : payload = [payload_len, frag_total, codec_id, flags]
  frag_idx 1..N  data       : payload = bytes[(k-1)*4 .. +4], last frame zero-padded
  timestamp_us   = 24-bit block capture time, identical across a packet's frames
  frag_total     = ceil(payload_len / 4)   (<= 31  =>  payload_len <= 124 bytes / 20 ms)

This module imports the certified wire codec from wirelab_core so the emitted bytes are
provably the same DeModFrame the 246-vector wire certificate already pins.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from wirelab_core import encode, decode  # certified DeModFrame codec

# ── L2 framing constants ──────────────────────────────────────────────────────
FCTRL = 3              # frame type carrying fragmented audio
FRAG_BITS = 5          # low bits of seq hold the fragment index
FRAG_MASK = (1 << FRAG_BITS) - 1        # 0x1F
MAX_FRAGS = FRAG_MASK                   # 31 data fragments
MAX_PAYLOAD = MAX_FRAGS * 4             # 124 bytes / 20 ms block
MAX_PACKET_ID = (1 << (16 - FRAG_BITS)) - 1   # 2047

# ── Codec registry ids (profiles live in DCF_AUDIO_SPEC.md) ───────────────────
CODEC_OPUS = 0         # 48 kHz mono ~24 kbps  (not byte-deterministic)
CODEC_PCM_DIAG = 1     # 6 kHz 8-bit mono, 120 B/block  (byte-deterministic)
CODEC_FAUST_PM = 2     # phase-mod synthesis, 8-byte param block
# id 3 reserved (declined low-bitrate speech codec)

# Descriptor flags
FLAG_END_TALKSPURT = 0x01
FLAG_PM_VOICE = 0x02


def _ceil_div(a, b):
    return (a + b - 1) // b


def packetize(codec_id, payload, packet_id, ts_us, src, dst, flags=0):
    """Serialise one codec block (`payload`, 0..124 bytes) into a list of 17-byte
    DeModFrame CTRL frames.  Returns [descriptor_frame, data_frame, ...]."""
    if not (0 <= codec_id < 256):
        raise ValueError("codec_id must be u8")
    if not (0 <= flags < 256):
        raise ValueError("flags must be u8")
    if not (0 <= packet_id <= MAX_PACKET_ID):
        raise ValueError(f"packet_id must be 0..{MAX_PACKET_ID}")
    if len(payload) > MAX_PAYLOAD:
        raise ValueError(f"payload {len(payload)}B exceeds {MAX_PAYLOAD}B/block cap")

    frag_total = _ceil_div(len(payload), 4)        # data fragments after descriptor
    frames = []

    # frag_idx 0 — descriptor
    desc_seq = (packet_id << FRAG_BITS) | 0
    desc_payload = bytes([len(payload), frag_total, codec_id, flags])
    frames.append(encode(FCTRL, desc_seq, src, dst, desc_payload, ts_us))

    # frag_idx 1..frag_total — data, last chunk zero-padded to 4 bytes
    for k in range(1, frag_total + 1):
        chunk = payload[(k - 1) * 4:(k - 1) * 4 + 4]
        chunk = chunk + bytes(4 - len(chunk))
        seq = (packet_id << FRAG_BITS) | k
        frames.append(encode(FCTRL, seq, src, dst, chunk, ts_us))

    return frames


class AudioReassembler:
    """Stateful reassembler.  push() emits a completed packet as soon as its
    descriptor and every data fragment have arrived; duplicates are ignored;
    finalize() reports any packet still incomplete as lost.

    Emitted packet event: ("packet", packet_id, ts_us, codec_id, payload_bytes, flags)
    Lost event:           ("lost", packet_id)
    """

    def __init__(self):
        self._pkts = {}    # packet_id -> dict(desc, ts, flags, frags{idx:4B})

    def push(self, frame):
        try:
            d = decode(frame)
        except ValueError:
            return []
        if d["frame_type"] != FCTRL:
            return []
        seq = d["seq"]
        packet_id = seq >> FRAG_BITS
        frag_idx = seq & FRAG_MASK
        body = bytes.fromhex(d["payload"])

        entry = self._pkts.setdefault(packet_id, {"desc": None, "ts": d["ts_us"],
                                                  "flags": 0, "frags": {}})
        entry["ts"] = d["ts_us"]
        if frag_idx == 0:
            if entry["desc"] is None:
                entry["desc"] = {"payload_len": body[0], "frag_total": body[1],
                                 "codec_id": body[2], "flags": body[3]}
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
        return [("packet", packet_id, entry["ts"], desc["codec_id"], payload, desc["flags"])]

    def finalize(self):
        events = [("lost", pid) for pid in sorted(self._pkts.keys())]
        self._pkts.clear()
        return events


# ── PCM-diagnostic codec (id 1): 6 kHz, 8-bit linear, mono, 120 samples/block ──
PCM_DIAG_RATE = 6000
PCM_DIAG_BLOCK = 120        # samples == bytes per 20 ms block


def pcm_diag_encode(samples):
    """float [-1,1] -> unsigned 8-bit (mid 128).  Byte-deterministic."""
    out = bytearray(len(samples))
    for i, s in enumerate(samples):
        v = int(round(s * 128.0)) + 128
        out[i] = 0 if v < 0 else 255 if v > 255 else v
    return bytes(out)


def pcm_diag_decode(data):
    """unsigned 8-bit -> float [-1,1].  decode∘encode is byte-lossless."""
    return [(b - 128) / 128.0 for b in data]


# ── Faust phase-mod codec (id 2): 8-byte synthesis parameter block ────────────
# Layout: [f0_hi, f0_lo, amp, mod_index, mod_ratio, bright, env, flags]
PM_PARAM_FIELDS = ("f0", "amp", "mod_index", "mod_ratio", "bright", "env", "flags")


def pm_pack(params):
    """dict -> 8 bytes.  f0 is a u16 (quantised fundamental, cents-mapped);
    the rest are u8.  Byte-deterministic — this layout is certified."""
    f0 = int(params["f0"]) & 0xFFFF
    b = bytes([
        (f0 >> 8) & 0xFF, f0 & 0xFF,
        int(params["amp"]) & 0xFF,
        int(params["mod_index"]) & 0xFF,
        int(params["mod_ratio"]) & 0xFF,
        int(params["bright"]) & 0xFF,
        int(params["env"]) & 0xFF,
        int(params["flags"]) & 0xFF,
    ])
    return b


def pm_unpack(data):
    """8 bytes -> dict.  unpack∘pack = id."""
    if len(data) != 8:
        raise ValueError("PM param block must be exactly 8 bytes")
    return {
        "f0": (data[0] << 8) | data[1],
        "amp": data[2],
        "mod_index": data[3],
        "mod_ratio": data[4],
        "bright": data[5],
        "env": data[6],
        "flags": data[7],
    }
