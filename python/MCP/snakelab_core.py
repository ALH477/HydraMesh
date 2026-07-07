# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Snake core — reference for the DCF-Snake record-plane L2 framing (certified layer).

DCF-Snake is the *record plane* of a software audio snake: a star of source ("spoke")
nodes streaming synchronized, quanta-coded audio over a dedicated cat5e link to one
central "mixer" hub, for studio multitrack capture.  Like DCF-SSTV/Text/Game it is an
*adapter* over the 17-byte DeModFrame wire quantum — one self-delimiting quanta **QSS
commit-hop packet** is fragmented into ordinary DeModFrame frames whose 4-byte payloads
carry the (opaque) QSS bytes, with the fragment bookkeeping packed into ``seq``.  The
frames are byte-for-byte valid DeModFrames (sync 0xD3, version 1, CRC-16/CCITT), so the
246-vector wire certificate is untouched; the L2 bytes are pinned cross-language by
``Documentation/snake_vectors.json``.

Why a separate adapter — and why CTRL(3), not DATA(0):
  * The record plane rides **CTRL(3)** frames (the audio/control family, alongside
    DCF-Audio) so it never collides with the DATA(0) adapters (text/game/sstv).  It uses
    a *different* seq split from DCF-Audio (which is 11:5) — DCF-Snake widens the fragment
    index to 11 bits (2047 fragments => 8188 bytes per stream_id) so a whole QSS packet
    (up to ~2048 B in the relaxed mode) fits one message, at the cost of a narrower 5-bit
    stream_id.  As with DCF-Audio's ``codec_id`` / DCF-SSTV's ``format_id``, the descriptor
    ``mode_id`` (quanta live/near/relaxed) is a hint that never changes these vectors.

Disambiguation: CTRL(3) now carries DCF-Audio (11:5) and DCF-Snake (5:11).  Neither is
tagged in-band; a node runs exactly one reassembler per ``dst`` channel — never multiplex
audio and snake on the same ``dst`` (see DCF_SNAKE_SPEC.md, mirroring DCF_SSTV_SPEC.md).

L2 framing (all frames version=1, type=CTRL(3), big-endian; see WIRE_QUANTUM_SPEC.md):
  seq (u16) = stream_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
  frag_idx == 0  descriptor : payload = [len_hi, len_lo, mode_id, flags]
  frag_idx 1..N  data       : payload = qss[(k-1)*4 .. +4], last frame zero-padded
  frag_total     = ceil(len / 4)            (<= 2047  =>  len <= 8188 bytes / message)
  src            = local node id (u16)
  dst            = rendezvous channel (u16): 0xFFFF broadcast, else crc16(passphrase)
  ts_us          = 24-bit microsecond timestamp, identical across a message's frames

The real monotone clock is the QSS ``hop_index`` carried *inside* the decoded stream (and
the BEACON grandmaster ``gm_sample_count`` below), never the 5-bit ``stream_id`` (a rolling
slot) or the 24-bit frame ``ts_us`` (wraps ~16.7 s).

This module also hosts the **BEACON media-clock** pack/unpack (the mixer's grandmaster
clock, byte-certified like the DCF-Mesh REPORT/ROLE control bytes) and the certified
``unwrap_pid`` primitive ported from client/src-tauri/src/sync.rs (the mixer's timeline).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from wirelab_core import encode, decode, crc16_ccitt  # certified DeModFrame codec

# ── L2 framing constants ──────────────────────────────────────────────────────
FCTRL = 3                       # frame type carrying the fragmented QSS bytes (CTRL)
FBEACON = 2                     # frame type carrying the grandmaster media clock
FRAG_BITS = 11                  # low bits of seq hold the fragment index
FRAG_MASK = (1 << FRAG_BITS) - 1            # 0x7FF
MAX_FRAGS = FRAG_MASK                       # 2047 data fragments
MAX_PAYLOAD = MAX_FRAGS * 4                 # 8188 bytes / message
MAX_STREAM_ID = (1 << (16 - FRAG_BITS)) - 1  # 31 (5-bit stream_id)

BROADCAST = 0xFFFF              # dst meaning "every node on the wire"

# quanta streaming modes — opaque hints in the descriptor (L2 never parses the QSS bytes);
# adding a mode never changes the vectors, exactly like DCF-Audio's codec_id.
MODE_LIVE = 0                   # 64 ms hop  (~98 kbps, ~784 B/pkt) — lowest snake latency
MODE_NEAR = 1                   # 128 ms hop (~83 kbps, ~1328 B/pkt)
MODE_RELAXED = 2                # 256 ms hop (~64 kbps, ~2048 B/pkt) — lowest bitrate

# Descriptor flags — opaque to L2 (they do not change the framing bytes).
FLAG_MORE = 0x01               # the talkspurt/stream continues in the next stream_id
FLAG_ANCHOR = 0x02             # this QSS packet re-anchors (low prediction dependency)
FLAG_END = 0x04                # end-of-talkspurt / end-of-stream

# BEACON media-clock payload
CLOCK_LEN = 16                 # bytes in a packed clock payload
CLOCK_VER = 1                  # clk_ver descriptor byte for a beacon message
NOMINAL_RATE_MHZ_48K = 48_000_000  # 48 kHz expressed in milli-Hz

# unwrap_pid rolling space (ported verbatim from sync.rs PID_MOD)
PID_MOD = 2048                 # 11-bit rolling packet_id space


def _ceil_div(a, b):
    return (a + b - 1) // b


def channel_id(name):
    """Map a human channel/passphrase to a 16-bit rendezvous ``dst`` (the same crc16
    frequency-channel trick the rest of the repo uses).  ``None``/"" => broadcast."""
    if not name:
        return BROADCAST
    return crc16_ccitt(name.encode("utf-8"))


def _frag_packetize(payload, stream_id, ts_us, src, dst, frame_type, kind, flags):
    """Generic 5:11 fragmenter shared by the record plane (CTRL) and the BEACON clock
    (BEACON): descriptor [len_hi, len_lo, kind, flags] then ceil(len/4) data frames."""
    payload = bytes(payload)
    if not (0 <= kind < 256):
        raise ValueError("kind (mode_id/clk_ver) must be u8")
    if not (0 <= flags < 256):
        raise ValueError("flags must be u8")
    if not (0 <= stream_id <= MAX_STREAM_ID):
        raise ValueError(f"stream_id must be 0..{MAX_STREAM_ID}")
    if len(payload) > MAX_PAYLOAD:
        raise ValueError(f"message {len(payload)}B exceeds {MAX_PAYLOAD}B cap "
                         "(chain across stream_ids with FLAG_MORE)")

    frag_total = _ceil_div(len(payload), 4)
    frames = []

    desc_seq = (stream_id << FRAG_BITS) | 0
    desc_payload = bytes([(len(payload) >> 8) & 0xFF, len(payload) & 0xFF, kind, flags])
    frames.append(encode(frame_type, desc_seq, src, dst, desc_payload, ts_us))

    for k in range(1, frag_total + 1):
        chunk = payload[(k - 1) * 4:(k - 1) * 4 + 4]
        chunk = chunk + bytes(4 - len(chunk))
        seq = (stream_id << FRAG_BITS) | k
        frames.append(encode(frame_type, seq, src, dst, chunk, ts_us))

    return frames


def packetize(qss_packet, stream_id, ts_us, src, dst, mode_id=MODE_LIVE, flags=0):
    """Serialise one self-delimiting QSS commit-hop packet (<= 8188 bytes of opaque bytes)
    into a list of 17-byte DeModFrame CTRL frames.  Returns [descriptor_frame, data...]."""
    return _frag_packetize(qss_packet, stream_id, ts_us, src, dst, FCTRL, mode_id, flags)


def beacon_packetize(clock_bytes, beacon_slot, ts_us, src, dst, clk_ver=CLOCK_VER, flags=0):
    """Serialise a packed 16-byte grandmaster clock payload into BEACON(2) frames (5 frames:
    descriptor + 4 data).  ``ts_us`` carries the mixer TX instant (T1) for delay estimation."""
    return _frag_packetize(clock_bytes, beacon_slot, ts_us, src, dst, FBEACON, clk_ver, flags)


class SnakeReassembler:
    """Stateful reassembler.  push() emits a completed message as soon as its descriptor and
    every data fragment have arrived; duplicates are ignored; finalize() reports any message
    still incomplete as lost.  Set ``frame_type=FBEACON`` to reassemble the clock beacons.

    Emitted event:  ("message", stream_id, ts_us, src, dst, kind, payload_bytes, flags)
    Lost event   :  ("lost", stream_id)
    """

    def __init__(self, accept_dst=None, frame_type=FCTRL):
        self._accept = accept_dst
        self._ft = frame_type
        self._pkts = {}    # stream_id -> dict(desc, ts, src, dst, frags{idx:4B})

    def push(self, frame):
        try:
            d = decode(frame)
        except ValueError:
            return []
        if d["frame_type"] != self._ft:
            return []
        if self._accept is not None and d["dst"] not in (self._accept, BROADCAST):
            return []
        seq = d["seq"]
        stream_id = seq >> FRAG_BITS
        frag_idx = seq & FRAG_MASK
        body = bytes.fromhex(d["payload"])

        entry = self._pkts.setdefault(stream_id, {"desc": None, "ts": d["ts_us"],
                                                  "src": d["src"], "dst": d["dst"],
                                                  "frags": {}})
        entry["ts"] = d["ts_us"]
        entry["src"] = d["src"]
        entry["dst"] = d["dst"]
        if frag_idx == 0:
            if entry["desc"] is None:
                length = (body[0] << 8) | body[1]
                entry["desc"] = {"len": length, "kind": body[2], "flags": body[3],
                                 "frag_total": _ceil_div(length, 4)}
        elif frag_idx not in entry["frags"]:
            entry["frags"][frag_idx] = body

        return self._try_emit(stream_id)

    def _try_emit(self, stream_id):
        entry = self._pkts.get(stream_id)
        if entry is None or entry["desc"] is None:
            return []
        desc = entry["desc"]
        need = set(range(1, desc["frag_total"] + 1))
        if not need.issubset(entry["frags"].keys()):
            return []
        raw = b"".join(entry["frags"][k] for k in range(1, desc["frag_total"] + 1))
        payload = raw[:desc["len"]]
        del self._pkts[stream_id]
        return [("message", stream_id, entry["ts"], entry["src"], entry["dst"],
                 desc["kind"], payload, desc["flags"])]

    def finalize(self):
        events = [("lost", sid) for sid in sorted(self._pkts.keys())]
        self._pkts.clear()
        return events


# ── BEACON grandmaster media clock (byte-certified 16-byte payload) ────────────
def pack_clock(gm_sample_count, nominal_rate_mhz, tx_seq, epoch):
    """Pack the grandmaster media clock into the 16-byte BEACON payload (big-endian):
      [0..7]   gm_sample_count  u64   monotonic capture-sample index (never wraps)
      [8..11]  nominal_rate_mHz u32   nominal sample rate in milli-Hz (48000000 = 48 kHz)
      [12..13] tx_seq           u16   beacon sequence (PLL discipline / loss detect)
      [14..15] epoch            u16   session epoch (bumps on rate change / mixer restart)
    """
    if not (0 <= gm_sample_count < (1 << 64)):
        raise ValueError("gm_sample_count must be u64")
    if not (0 <= nominal_rate_mhz < (1 << 32)):
        raise ValueError("nominal_rate_mhz must be u32")
    if not (0 <= tx_seq < (1 << 16) and 0 <= epoch < (1 << 16)):
        raise ValueError("tx_seq/epoch must be u16")
    return (gm_sample_count.to_bytes(8, "big") + nominal_rate_mhz.to_bytes(4, "big")
            + tx_seq.to_bytes(2, "big") + epoch.to_bytes(2, "big"))


def unpack_clock(b):
    """Inverse of pack_clock; returns a dict.  ``decode(pack_clock(...)) == fields``."""
    b = bytes(b)
    if len(b) != CLOCK_LEN:
        raise ValueError(f"clock payload must be {CLOCK_LEN} bytes")
    return {"gm_sample_count": int.from_bytes(b[0:8], "big"),
            "nominal_rate_mhz": int.from_bytes(b[8:12], "big"),
            "tx_seq": int.from_bytes(b[12:14], "big"),
            "epoch": int.from_bytes(b[14:16], "big")}


# ── unwrap_pid — ported verbatim from client/src-tauri/src/sync.rs ────────────
def unwrap_pid(prev_abs, raw, mod=PID_MOD):
    """Unwrap a rolling packet_id to a monotonic absolute index relative to a previous
    absolute value (forward progress dominates; a small backward delta = reorder → step
    back).  Byte/behaviour-identical to sync.rs::unwrap_pid (the mixer's timeline uses it)."""
    prev_lo = prev_abs % mod
    raw = raw % mod
    fwd = (raw + mod - prev_lo) % mod
    if fwd <= mod // 2:
        return prev_abs + fwd
    return max(prev_abs - (mod - fwd), 0)   # saturating_sub


# ── self-test (round trips a few messages through encode -> wire -> decode) ────
def _selftest():
    cases = [b"", b"\x00", b"\xff\xd8a QSS-ish blob",
             bytes((i * 37) & 0xFF for i in range(1500)),
             bytes(MAX_PAYLOAD)]
    for sid, msg in enumerate(cases):
        frames = packetize(msg, sid % (MAX_STREAM_ID + 1), 123456, src=7, dst=42,
                           mode_id=MODE_LIVE, flags=FLAG_ANCHOR)
        for f in frames:
            assert len(f) == 17 and f[0] == 0xD3 and (f[1] >> 4) == 1 and (f[1] & 0x0F) == FCTRL
        r = SnakeReassembler()
        out = []
        for f in frames:
            out += r.push(f)
        assert len(out) == 1, f"expected 1 message, got {len(out)}"
        _, _, _, src, dst, mode, data, flags = out[0]
        assert data == msg and src == 7 and dst == 42 and mode == MODE_LIVE
        if len(frames) > 1:
            assert r.push(frames[-1]) == []

    # out-of-order + duplicate delivery still reassembles exactly once
    msg = bytes(range(50))
    frames = packetize(msg, 3, 1, src=1, dst=2)
    r = SnakeReassembler()
    out = []
    stream = [frames[2], frames[2], frames[0], frames[1]] + frames[3:]
    for f in stream:
        out += r.push(f)
    assert len(out) == 1 and out[0][6] == msg

    # a dropped data fragment leaves the message incomplete -> finalize reports it lost
    frames = packetize(bytes(40), 9, 1, src=1, dst=2)
    r = SnakeReassembler()
    for j, f in enumerate(frames):
        if j == 2:
            continue
        assert r.push(f) == []
    assert r.finalize() == [("lost", 9)]

    # BEACON clock: pack -> frames -> reassemble -> unpack is identity
    clk = pack_clock(0x0123456789ABCDEF, NOMINAL_RATE_MHZ_48K, 0x1234, 0x0007)
    assert unpack_clock(clk) == {"gm_sample_count": 0x0123456789ABCDEF,
                                 "nominal_rate_mhz": NOMINAL_RATE_MHZ_48K,
                                 "tx_seq": 0x1234, "epoch": 0x0007}
    bframes = beacon_packetize(clk, 1, 0x00ABCD, src=0x00A1, dst=BROADCAST)
    assert len(bframes) == 1 + CLOCK_LEN // 4          # 5 frames
    for f in bframes:
        assert (f[1] & 0x0F) == FBEACON
    br = SnakeReassembler(frame_type=FBEACON)
    bout = []
    for f in bframes:
        bout += br.push(f)
    assert len(bout) == 1 and bout[0][6] == clk and bout[0][5] == CLOCK_VER

    # unwrap_pid matches sync.rs behaviour: forward progress + reorder step-back + wrap
    assert unwrap_pid(0, 1) == 1
    assert unwrap_pid(2047, 0) == 2048            # rollover forward
    assert unwrap_pid(100, 98) == 98              # small backward -> reorder
    assert unwrap_pid(0, 2047) == 0               # far-backward saturates at 0
    print("snakelab_core selftest: CERTIFIED")


if __name__ == "__main__":
    _selftest()
