"""DCF TextLab core — reference for the DCF-Text L2 framing (the certified layer).

DCF-Text is an *adapter* over the 17-byte DeModFrame wire quantum, exactly like
DCF-Audio and DCF-Game: one UTF-8 message is fragmented into ordinary DeModFrame
DATA frames whose 4-byte payloads carry the bytes, with the fragment bookkeeping
packed into the `seq` field.  The frames are byte-for-byte valid DeModFrames
(sync 0xD3, version nibble 1, CRC-16/CCITT), so they satisfy the same wire
invariant the 246-vector certificate pins — this module reuses the certified codec
in ``wirelab_core.py`` so nothing about the quantum is reinvented here, and the L2
bytes are pinned cross-language by ``Documentation/text_vectors.json``.

Why a separate adapter from audio/game:
  * Audio rides on CTRL frames (type 3).  Text and game ride on DATA frames
    (type 0); a text fragment is told apart from a game fragment by its seq split
    (text uses a 10-bit fragment index, game a 5-bit one) — the descriptor that
    starts every message disambiguates which adapter owns the packet_id.
  * Chat/agent messages are larger and lower-rate than 20 ms audio blocks, so the
    seq split gives the fragment index more room (10 bits) than audio's 5.

L2 framing (all frames version=1, type=DATA(0), big-endian; see WIRE_QUANTUM_SPEC.md):
  seq (u16) = packet_id[15:10] (6 bits, 0..63) | frag_idx[9:0] (10 bits, 0..1023)
  frag_idx == 0  descriptor : payload = [len_hi, len_lo, flags, reserved]
  frag_idx 1..N  data       : payload = utf8[(k-1)*4 .. +4], last frame zero-padded
  frag_total     = ceil(len / 4)            (<= 1023  =>  len <= 4092 bytes / message)
  src            = local node id (u16)
  dst            = rendezvous channel (u16): 0xFFFF broadcast, else crc16(passphrase)
  ts_us          = 24-bit microsecond timestamp, identical across a message's frames

This is the canonical source of truth; ``matrix-bridge/dcf_text.py`` re-exports it.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from wirelab_core import encode, decode, crc16_ccitt  # certified DeModFrame codec

# ── L2 framing constants ──────────────────────────────────────────────────────
FDATA = 0                       # frame type carrying fragmented chat text
FRAG_BITS = 10                  # low bits of seq hold the fragment index
FRAG_MASK = (1 << FRAG_BITS) - 1            # 0x3FF
MAX_FRAGS = FRAG_MASK                       # 1023 data fragments
MAX_PAYLOAD = MAX_FRAGS * 4                 # 4092 bytes / message
MAX_PACKET_ID = (1 << (16 - FRAG_BITS)) - 1  # 63

BROADCAST = 0xFFFF              # dst meaning "every node on the wire"

# Descriptor flags — opaque to L2 (they do not change the framing bytes).
FLAG_AGENT = 0x01              # message is addressed to / from the LLM agent
FLAG_MORE = 0x02              # this message is one chunk of a longer Matrix event
FLAG_RELIABLE = 0x04          # ask the receiver to ACK (DCF-Text reliability layer)


def _ceil_div(a, b):
    return (a + b - 1) // b


def channel_id(name):
    """Map a human channel/passphrase to a 16-bit rendezvous ``dst`` (the same
    frequency-channel trick the rest of the repo uses).  ``None``/"" => broadcast."""
    if not name:
        return BROADCAST
    return crc16_ccitt(name.encode("utf-8"))


def packetize(text, packet_id, ts_us, src, dst, flags=0):
    """Serialise one UTF-8 message (<= 4092 bytes) into a list of 17-byte
    DeModFrame DATA frames.  Returns [descriptor_frame, data_frame, ...]."""
    payload = text.encode("utf-8") if isinstance(text, str) else bytes(text)
    if not (0 <= flags < 256):
        raise ValueError("flags must be u8")
    if not (0 <= packet_id <= MAX_PACKET_ID):
        raise ValueError(f"packet_id must be 0..{MAX_PACKET_ID}")
    if len(payload) > MAX_PAYLOAD:
        raise ValueError(f"message {len(payload)}B exceeds {MAX_PAYLOAD}B cap "
                         "(split into multiple messages)")

    frag_total = _ceil_div(len(payload), 4)        # data fragments after descriptor
    frames = []

    # frag_idx 0 — descriptor (carries the true byte length so we can unpad)
    desc_seq = (packet_id << FRAG_BITS) | 0
    desc_payload = bytes([(len(payload) >> 8) & 0xFF, len(payload) & 0xFF, flags, 0])
    frames.append(encode(FDATA, desc_seq, src, dst, desc_payload, ts_us))

    # frag_idx 1..frag_total — data, last chunk zero-padded to 4 bytes
    for k in range(1, frag_total + 1):
        chunk = payload[(k - 1) * 4:(k - 1) * 4 + 4]
        chunk = chunk + bytes(4 - len(chunk))
        seq = (packet_id << FRAG_BITS) | k
        frames.append(encode(FDATA, seq, src, dst, chunk, ts_us))

    return frames


class TextReassembler:
    """Stateful reassembler.  push() emits a completed message as soon as its
    descriptor and every data fragment have arrived; duplicates are ignored;
    finalize() reports any message still incomplete as lost.

    Emitted message event: ("message", packet_id, ts_us, src, dst, text, flags)
    Lost event (finalize): ("lost", packet_id)
    """

    def __init__(self, accept_dst=None):
        # accept_dst: None => accept every channel; else only this dst + BROADCAST
        self._accept = accept_dst
        self._pkts = {}    # packet_id -> dict(desc, ts, src, dst, frags{idx:4B})

    def push(self, frame):
        try:
            d = decode(frame)
        except ValueError:
            return []
        if d["frame_type"] != FDATA:
            return []
        if self._accept is not None and d["dst"] not in (self._accept, BROADCAST):
            return []
        seq = d["seq"]
        packet_id = seq >> FRAG_BITS
        frag_idx = seq & FRAG_MASK
        body = bytes.fromhex(d["payload"])

        entry = self._pkts.setdefault(packet_id, {"desc": None, "ts": d["ts_us"],
                                                  "src": d["src"], "dst": d["dst"],
                                                  "frags": {}})
        entry["ts"] = d["ts_us"]
        entry["src"] = d["src"]
        entry["dst"] = d["dst"]
        if frag_idx == 0:
            if entry["desc"] is None:
                length = (body[0] << 8) | body[1]
                entry["desc"] = {"len": length, "flags": body[2],
                                 "frag_total": _ceil_div(length, 4)}
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
        payload = raw[:desc["len"]]
        del self._pkts[packet_id]
        try:
            text = payload.decode("utf-8")
        except UnicodeDecodeError:
            text = payload.decode("utf-8", "replace")
        return [("message", packet_id, entry["ts"], entry["src"], entry["dst"],
                 text, desc["flags"])]

    def finalize(self):
        """Report every still-incomplete message as lost (ascending packet_id),
        clearing state.  Mirrors GameReassembler.finalize / the C and Rust refs."""
        events = [("lost", pid) for pid in sorted(self._pkts.keys())]
        self._pkts.clear()
        return events


# ── self-test (round trips a few messages through encode -> wire -> decode) ────
def _selftest():
    cases = ["hi", "DeModFrame over Matrix",
             "unicode ❤️ \U0001f680" * 5,
             "x" * MAX_PAYLOAD]
    for pid, msg in enumerate(cases):
        frames = packetize(msg, pid % (MAX_PACKET_ID + 1), 123456, src=7, dst=42)
        for f in frames:
            assert len(f) == 17 and f[0] == 0xD3 and (f[1] >> 4) == 1
        r = TextReassembler()
        out = []
        for f in frames:                       # in-order delivery
            out += r.push(f)
        assert len(out) == 1, f"expected 1 message, got {len(out)}"
        _, _, _, src, dst, text, _ = out[0]
        assert text == msg and src == 7 and dst == 42, (text, msg)
        # replaying any already-consumed data fragment must not double-emit the message
        assert r.push(frames[-1]) == []

    # out-of-order + duplicate delivery still reassembles exactly once
    frames = packetize("reorder me please", 3, 1, src=1, dst=2)
    r = TextReassembler()
    out = []
    for f in [frames[2], frames[2], frames[0], frames[1]] + frames[3:]:
        out += r.push(f)
    assert len(out) == 1 and out[0][5] == "reorder me please"

    # a dropped data fragment leaves the message incomplete -> finalize reports it lost
    frames = packetize("dropme over the wire", 9, 1, src=1, dst=2)
    r = TextReassembler()
    for j, f in enumerate(frames):
        if j == 2:
            continue
        assert r.push(f) == []
    assert r.finalize() == [("lost", 9)]
    print("textlab_core selftest: CERTIFIED")


if __name__ == "__main__":
    _selftest()
