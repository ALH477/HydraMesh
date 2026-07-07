# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Cue core — reference for the DCF-Snake cue-plane L2 framing (the certified layer).

The *cue plane* is the low-latency PCM monitor bus of the audio snake: a bidirectional
uncompressed-PCM loop on a second cat5e wire.  Source nodes send tiny (~1 ms) PCM blocks
up to the mixer; the mixer sums a per-node cue mix and returns each performer their own
monitor feed.  No atom-pursuit encode, so it escapes the quanta record plane's ≥64 ms
latency floor (~4–6 ms round-trip).

Like the record plane it is an *adapter* over the 17-byte DeModFrame quantum — one PCM
block is fragmented into ordinary CTRL(3) frames whose 4-byte payloads carry raw
little-endian PCM.  The frames are byte-valid DeModFrames, so the 246-vector wire
certificate is untouched; the L2 bytes are pinned by Documentation/monitor_vectors.json.

The cue plane rides its *own* cat5e wire (a dedicated EtherType), so it never contends
with — or is confused for — the record plane, even though both use CTRL(3).  Uplink
(source→mixer) and downlink (mixer→node cue) share this framing, distinguished by
direction (src/dst) and the FLAG_CUE_RETURN descriptor bit.

L2 framing (all frames version=1, type=CTRL(3), big-endian; see WIRE_QUANTUM_SPEC.md):
  seq (u16) = block_seq[15:7] (9 bits, 0..511) | frag_idx[6:0] (7 bits, 0..127)
  frag_idx == 0  descriptor : payload = [block_samples, format, channels, flags]
  frag_idx 1..N  data       : payload = pcm[(k-1)*4 .. +4], last frame zero-padded
  length         = block_samples * channels * bytes_per_sample(format)  (derived, self-describing)
  frag_total     = ceil(length / 4)         (<= 127  =>  length <= 508 bytes / block)

The block geometry lives in the descriptor, so a receiver reconstructs the exact PCM
length without any explicit length field.  block_seq is a rolling 9-bit counter the mixer
unwraps (snakelab_core.unwrap_pid, mod 512) for its jitter buffer / timeline.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from wirelab_core import encode, decode, crc16_ccitt  # certified DeModFrame codec

# ── L2 framing constants ──────────────────────────────────────────────────────
FCTRL = 3                       # cue PCM rides CTRL(3) on its own (wire-B) EtherType
FRAG_BITS = 7                   # low bits of seq hold the fragment index
FRAG_MASK = (1 << FRAG_BITS) - 1            # 0x7F
MAX_FRAGS = FRAG_MASK                       # 127 data fragments
MAX_PAYLOAD = MAX_FRAGS * 4                 # 508 bytes / block
MAX_BLOCK_SEQ = (1 << (16 - FRAG_BITS)) - 1  # 511 (9-bit rolling block counter)
BLOCK_SEQ_MOD = MAX_BLOCK_SEQ + 1           # 512 (unwrap_pid modulus for the cue plane)

BROADCAST = 0xFFFF

# PCM sample formats (little-endian on the wire) — opaque to the framing byte-count.
FMT_S16 = 0                     # signed 16-bit  (2 bytes/sample)
FMT_S24 = 1                     # signed 24-bit  (3 bytes/sample, packed)
FMT_F32 = 2                     # 32-bit float   (4 bytes/sample)
_FMT_BYTES = {FMT_S16: 2, FMT_S24: 3, FMT_F32: 4}

# Descriptor flags — opaque to L2 (they do not change the framing bytes).
FLAG_CUE_RETURN = 0x01         # downlink: mixer → node cue/monitor feed (vs uplink)
FLAG_END = 0x02                # last block of a stream / talkspurt


def _ceil_div(a, b):
    return (a + b - 1) // b


def bytes_per_sample(format_id):
    if format_id not in _FMT_BYTES:
        raise ValueError(f"unknown PCM format_id {format_id}")
    return _FMT_BYTES[format_id]


def pcm_len(block_samples, format_id, channels):
    """Bytes of PCM in a block of the given geometry (the self-describing length)."""
    return block_samples * channels * bytes_per_sample(format_id)


def channel_id(name):
    """Map a human channel/passphrase to a 16-bit rendezvous ``dst`` (crc16).  None/"" => broadcast."""
    if not name:
        return BROADCAST
    return crc16_ccitt(name.encode("utf-8"))


def packetize(pcm, block_seq, ts_us, src, dst, block_samples, format_id, channels, flags=0):
    """Serialise one PCM block into CTRL(3) frames.  ``len(pcm)`` must equal the geometry-
    derived length ``block_samples*channels*bytes_per_sample(format_id)``.  Returns
    [descriptor_frame, data_frame, ...]."""
    pcm = bytes(pcm)
    if not (0 <= block_samples < 256 and 0 <= channels < 256):
        raise ValueError("block_samples and channels must be u8")
    if not (0 <= flags < 256):
        raise ValueError("flags must be u8")
    if not (0 <= block_seq <= MAX_BLOCK_SEQ):
        raise ValueError(f"block_seq must be 0..{MAX_BLOCK_SEQ}")
    expect = pcm_len(block_samples, format_id, channels)
    if len(pcm) != expect:
        raise ValueError(f"pcm is {len(pcm)}B but geometry implies {expect}B")
    if len(pcm) > MAX_PAYLOAD:
        raise ValueError(f"block {len(pcm)}B exceeds {MAX_PAYLOAD}B cap (use a smaller block)")

    frag_total = _ceil_div(len(pcm), 4)
    frames = []

    desc_seq = (block_seq << FRAG_BITS) | 0
    desc_payload = bytes([block_samples & 0xFF, format_id & 0xFF, channels & 0xFF, flags & 0xFF])
    frames.append(encode(FCTRL, desc_seq, src, dst, desc_payload, ts_us))

    for k in range(1, frag_total + 1):
        chunk = pcm[(k - 1) * 4:(k - 1) * 4 + 4]
        chunk = chunk + bytes(4 - len(chunk))
        seq = (block_seq << FRAG_BITS) | k
        frames.append(encode(FCTRL, seq, src, dst, chunk, ts_us))

    return frames


class CueReassembler:
    """Stateful reassembler.  push() emits a completed PCM block as soon as its descriptor
    and every data fragment have arrived; duplicates are ignored; finalize() reports any
    block still incomplete as lost.

    Emitted event: ("block", block_seq, ts_us, src, dst, block_samples, format, channels,
                    flags, pcm_bytes)
    Lost event   : ("lost", block_seq)
    """

    def __init__(self, accept_dst=None):
        self._accept = accept_dst
        self._pkts = {}    # block_seq -> dict(desc, ts, src, dst, frags{idx:4B})

    def push(self, frame):
        try:
            d = decode(frame)
        except ValueError:
            return []
        if d["frame_type"] != FCTRL:
            return []
        if self._accept is not None and d["dst"] not in (self._accept, BROADCAST):
            return []
        seq = d["seq"]
        block_seq = seq >> FRAG_BITS
        frag_idx = seq & FRAG_MASK
        body = bytes.fromhex(d["payload"])

        entry = self._pkts.setdefault(block_seq, {"desc": None, "ts": d["ts_us"],
                                                  "src": d["src"], "dst": d["dst"],
                                                  "frags": {}})
        entry["ts"] = d["ts_us"]
        entry["src"] = d["src"]
        entry["dst"] = d["dst"]
        if frag_idx == 0:
            if entry["desc"] is None:
                bs, fmt, ch, fl = body[0], body[1], body[2], body[3]
                length = pcm_len(bs, fmt, ch)
                entry["desc"] = {"block_samples": bs, "format": fmt, "channels": ch,
                                 "flags": fl, "len": length, "frag_total": _ceil_div(length, 4)}
        elif frag_idx not in entry["frags"]:
            entry["frags"][frag_idx] = body

        return self._try_emit(block_seq)

    def _try_emit(self, block_seq):
        entry = self._pkts.get(block_seq)
        if entry is None or entry["desc"] is None:
            return []
        desc = entry["desc"]
        need = set(range(1, desc["frag_total"] + 1))
        if not need.issubset(entry["frags"].keys()):
            return []
        raw = b"".join(entry["frags"][k] for k in range(1, desc["frag_total"] + 1))
        pcm = raw[:desc["len"]]
        del self._pkts[block_seq]
        return [("block", block_seq, entry["ts"], entry["src"], entry["dst"],
                 desc["block_samples"], desc["format"], desc["channels"], desc["flags"], pcm)]

    def finalize(self):
        events = [("lost", bs) for bs in sorted(self._pkts.keys())]
        self._pkts.clear()
        return events


# ── self-test ─────────────────────────────────────────────────────────────────
def _selftest():
    # 1 ms @ 48 kHz blocks across formats/channel counts
    cases = [(48, FMT_S16, 1), (48, FMT_S16, 2), (48, FMT_S24, 1), (24, FMT_F32, 2), (1, FMT_S16, 1)]
    for i, (bs, fmt, ch) in enumerate(cases):
        n = pcm_len(bs, fmt, ch)
        pcm = bytes((j * 31) & 0xFF for j in range(n))
        frames = packetize(pcm, i % (MAX_BLOCK_SEQ + 1), 1000 * i, src=7, dst=42,
                           block_samples=bs, format_id=fmt, channels=ch, flags=FLAG_CUE_RETURN)
        for f in frames:
            assert len(f) == 17 and f[0] == 0xD3 and (f[1] >> 4) == 1 and (f[1] & 0x0F) == FCTRL
        assert len(frames) == 1 + _ceil_div(n, 4)
        r = CueReassembler()
        out = []
        for f in frames:
            out += r.push(f)
        assert len(out) == 1
        _, _, _, src, dst, o_bs, o_fmt, o_ch, o_fl, data = out[0]
        assert (src, dst, o_bs, o_fmt, o_ch, o_fl) == (7, 42, bs, fmt, ch, FLAG_CUE_RETURN)
        assert data == pcm

    # out-of-order + duplicate reassembles once
    pcm = bytes(range(96))       # 48 samples mono S16
    frames = packetize(pcm, 3, 1, 1, 2, 48, FMT_S16, 1)
    r = CueReassembler()
    out = []
    stream = [frames[3], frames[3], frames[0], frames[1], frames[2]] + frames[4:]
    for f in stream:
        out += r.push(f)
    assert len(out) == 1 and out[0][9] == pcm

    # dropped fragment -> lost
    frames = packetize(bytes(96), 9, 1, 1, 2, 48, FMT_S16, 1)
    r = CueReassembler()
    for j, f in enumerate(frames):
        if j == 2:
            continue
        assert r.push(f) == []
    assert r.finalize() == [("lost", 9)]

    # bounds: geometry mismatch + oversize block rejected
    try:
        packetize(bytes(10), 0, 0, 1, 1, 48, FMT_S16, 1)
        raise AssertionError("geometry mismatch must be rejected")
    except ValueError:
        pass
    try:
        packetize(bytes(512), 0, 0, 1, 1, 128, FMT_S16, 2)   # 128*2*2=512 > 508
        raise AssertionError("oversize block must be rejected")
    except ValueError:
        pass
    print("monitorlab_core selftest: CERTIFIED")


if __name__ == "__main__":
    _selftest()
