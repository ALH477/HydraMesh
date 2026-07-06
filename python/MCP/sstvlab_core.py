# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-SSTV core — reference for the DCF-SSTV L2 framing (the certified layer).

DCF-SSTV ("slow-scan television") is an *adapter* over the 17-byte DeModFrame wire
quantum, exactly like DCF-Text and DCF-Game: one still image is fragmented into
ordinary DeModFrame DATA frames whose 4-byte payloads carry the (opaque) image bytes,
with the fragment bookkeeping packed into the `seq` field.  The frames are byte-for-byte
valid DeModFrames (sync 0xD3, version nibble 1, CRC-16/CCITT), so they satisfy the same
wire invariant the 246-vector certificate pins — this module reuses the certified codec
in ``wirelab_core.py`` so nothing about the quantum is reinvented here, and the L2 bytes
are pinned cross-language by ``Documentation/sstv_vectors.json``.

Why a separate adapter from text/game:
  * Text and game ride DATA(0) too, but an image is *larger* than a chat line or a game
    snapshot, so DCF-SSTV widens the fragment index to 11 bits (2047 fragments =>
    8188 bytes per image_id) at the cost of a narrower 5-bit image_id space.  The image
    payload is treated as opaque bytes; a `format_id` byte in the descriptor is only a
    hint (JPEG/PNG/raw), exactly as DCF-Audio's `codec_id` is — so adding image formats
    never changes these vectors.

Disambiguation: DATA(0) now carries game (11:5), text (6:10) and sstv (5:11).  None are
tagged in-band; a node runs exactly one reassembler per `dst` channel — never multiplex
SSTV with text/game on the same `dst` (see DCF_SSTV_SPEC.md, mirroring DCF_TEXT_SPEC.md).

L2 framing (all frames version=1, type=DATA(0), big-endian; see WIRE_QUANTUM_SPEC.md):
  seq (u16) = image_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
  frag_idx == 0  descriptor : payload = [len_hi, len_lo, format_id, flags]
  frag_idx 1..N  data       : payload = bytes[(k-1)*4 .. +4], last frame zero-padded
  frag_total     = ceil(len / 4)            (<= 2047  =>  len <= 8188 bytes / image)
  src            = local node id (u16)
  dst            = rendezvous channel (u16): 0xFFFF broadcast, else crc16(passphrase)
  ts_us          = 24-bit microsecond timestamp, identical across an image's frames

Images larger than 8188 bytes are chained app-side across successive image_ids with the
MORE flag; a single image_id is a <=8188 B still (already minutes of air time on the slow
HydraModem acoustic link).
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from wirelab_core import encode, decode, crc16_ccitt  # certified DeModFrame codec

# ── L2 framing constants ──────────────────────────────────────────────────────
FDATA = 0                       # frame type carrying fragmented image bytes
FRAG_BITS = 11                  # low bits of seq hold the fragment index
FRAG_MASK = (1 << FRAG_BITS) - 1            # 0x7FF
MAX_FRAGS = FRAG_MASK                       # 2047 data fragments
MAX_PAYLOAD = MAX_FRAGS * 4                 # 8188 bytes / image
MAX_IMAGE_ID = (1 << (16 - FRAG_BITS)) - 1  # 31

BROADCAST = 0xFFFF              # dst meaning "every node on the wire"

# Image format ids — opaque hints (L2 never parses the bytes); adding one never changes
# the vectors, exactly like DCF-Audio's codec_id / DCF-Game's msg_type_id.
FMT_RAW = 0                     # raw bytes, app-defined geometry in its own leading header
FMT_JPEG = 1
FMT_PNG = 2
FMT_WEBP = 3
FMT_RGB565 = 4                  # width*height*2 little-endian RGB565, geometry app-side

# Descriptor flags — opaque to L2 (they do not change the framing bytes).
FLAG_MORE = 0x01               # image continues in the next image_id (chained big image)
FLAG_KEYFRAME = 0x02           # start of a fresh image (vs a continuation)
FLAG_RELIABLE = 0x04           # ask the receiver to ACK (DCF-SSTV reliability layer)


def _ceil_div(a, b):
    return (a + b - 1) // b


def channel_id(name):
    """Map a human channel/passphrase to a 16-bit rendezvous ``dst`` (the same
    frequency-channel trick the rest of the repo uses).  ``None``/"" => broadcast."""
    if not name:
        return BROADCAST
    return crc16_ccitt(name.encode("utf-8"))


def packetize(image, image_id, ts_us, src, dst, format_id=0, flags=0):
    """Serialise one image (<= 8188 bytes of opaque bytes) into a list of 17-byte
    DeModFrame DATA frames.  Returns [descriptor_frame, data_frame, ...]."""
    payload = bytes(image)
    if not (0 <= format_id < 256):
        raise ValueError("format_id must be u8")
    if not (0 <= flags < 256):
        raise ValueError("flags must be u8")
    if not (0 <= image_id <= MAX_IMAGE_ID):
        raise ValueError(f"image_id must be 0..{MAX_IMAGE_ID}")
    if len(payload) > MAX_PAYLOAD:
        raise ValueError(f"image {len(payload)}B exceeds {MAX_PAYLOAD}B cap "
                         "(chain across image_ids with FLAG_MORE)")

    frag_total = _ceil_div(len(payload), 4)        # data fragments after descriptor
    frames = []

    # frag_idx 0 — descriptor (carries the true byte length so we can unpad, + format_id)
    desc_seq = (image_id << FRAG_BITS) | 0
    desc_payload = bytes([(len(payload) >> 8) & 0xFF, len(payload) & 0xFF,
                          format_id, flags])
    frames.append(encode(FDATA, desc_seq, src, dst, desc_payload, ts_us))

    # frag_idx 1..frag_total — data, last chunk zero-padded to 4 bytes
    for k in range(1, frag_total + 1):
        chunk = payload[(k - 1) * 4:(k - 1) * 4 + 4]
        chunk = chunk + bytes(4 - len(chunk))
        seq = (image_id << FRAG_BITS) | k
        frames.append(encode(FDATA, seq, src, dst, chunk, ts_us))

    return frames


class SstvReassembler:
    """Stateful reassembler.  push() emits a completed image as soon as its descriptor
    and every data fragment have arrived; duplicates are ignored; finalize() reports any
    image still incomplete as lost.

    Emitted image event: ("image", image_id, ts_us, src, dst, format_id, image_bytes, flags)
    Lost event (finalize): ("lost", image_id)

    Slow-scan extra (opt-in via progress=True): each push that grows the largest contiguous
    received byte-prefix also yields ("progress", image_id, prefix_len) so a viewer can
    paint the image top-down as fragments trickle in.  Progress events are a convenience
    and are NOT part of the byte certificate (the vectors use progress=False).
    """

    def __init__(self, accept_dst=None, progress=False):
        # accept_dst: None => accept every channel; else only this dst + BROADCAST
        self._accept = accept_dst
        self._progress = progress
        self._pkts = {}    # image_id -> dict(desc, ts, src, dst, frags{idx:4B}, seen_prefix)

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
        image_id = seq >> FRAG_BITS
        frag_idx = seq & FRAG_MASK
        body = bytes.fromhex(d["payload"])

        entry = self._pkts.setdefault(image_id, {"desc": None, "ts": d["ts_us"],
                                                 "src": d["src"], "dst": d["dst"],
                                                 "frags": {}, "seen_prefix": 0})
        entry["ts"] = d["ts_us"]
        entry["src"] = d["src"]
        entry["dst"] = d["dst"]
        if frag_idx == 0:
            if entry["desc"] is None:
                length = (body[0] << 8) | body[1]
                entry["desc"] = {"len": length, "format_id": body[2], "flags": body[3],
                                 "frag_total": _ceil_div(length, 4)}
        elif frag_idx not in entry["frags"]:
            entry["frags"][frag_idx] = body

        events = self._progress_events(image_id) if self._progress else []
        events += self._try_emit(image_id)
        return events

    def _progress_events(self, image_id):
        entry = self._pkts.get(image_id)
        if entry is None or entry["desc"] is None:
            return []
        # largest contiguous run of present data fragments 1..c
        c = 0
        while (c + 1) in entry["frags"]:
            c += 1
        prefix = min(c * 4, entry["desc"]["len"])
        if prefix > entry["seen_prefix"]:
            entry["seen_prefix"] = prefix
            return [("progress", image_id, prefix)]
        return []

    def _try_emit(self, image_id):
        entry = self._pkts.get(image_id)
        if entry is None or entry["desc"] is None:
            return []
        desc = entry["desc"]
        need = set(range(1, desc["frag_total"] + 1))
        if not need.issubset(entry["frags"].keys()):
            return []
        raw = b"".join(entry["frags"][k] for k in range(1, desc["frag_total"] + 1))
        payload = raw[:desc["len"]]
        del self._pkts[image_id]
        return [("image", image_id, entry["ts"], entry["src"], entry["dst"],
                 desc["format_id"], payload, desc["flags"])]

    def finalize(self):
        """Report every still-incomplete image as lost (ascending image_id), clearing
        state.  Mirrors TextReassembler.finalize / the C, Rust and Go refs."""
        events = [("lost", iid) for iid in sorted(self._pkts.keys())]
        self._pkts.clear()
        return events


# ── self-test (round trips a few images through encode -> wire -> decode) ──────
def _selftest():
    cases = [b"", b"\x00", b"\xff\xd8\xff\xe0jpeg-ish header bytes",
             bytes((i * 37) & 0xFF for i in range(1500)),
             bytes(MAX_PAYLOAD)]
    for iid, img in enumerate(cases):
        frames = packetize(img, iid % (MAX_IMAGE_ID + 1), 123456, src=7, dst=42,
                           format_id=FMT_JPEG, flags=FLAG_KEYFRAME)
        for f in frames:
            assert len(f) == 17 and f[0] == 0xD3 and (f[1] >> 4) == 1
        r = SstvReassembler()
        out = []
        for f in frames:                       # in-order delivery
            out += r.push(f)
        assert len(out) == 1, f"expected 1 image, got {len(out)}"
        _, _, _, src, dst, fmt, data, flags = out[0]
        assert data == img and src == 7 and dst == 42 and fmt == FMT_JPEG, (len(data), len(img))
        # replaying any already-consumed data fragment must not double-emit the image
        if len(frames) > 1:
            assert r.push(frames[-1]) == []

    # out-of-order + duplicate delivery still reassembles exactly once
    img = bytes(range(50))
    frames = packetize(img, 3, 1, src=1, dst=2)
    r = SstvReassembler()
    out = []
    stream = [frames[2], frames[2], frames[0], frames[1]] + frames[3:]
    for f in stream:
        out += r.push(f)
    assert len(out) == 1 and out[0][6] == img

    # a dropped data fragment leaves the image incomplete -> finalize reports it lost
    frames = packetize(bytes(40), 9, 1, src=1, dst=2)
    r = SstvReassembler()
    for j, f in enumerate(frames):
        if j == 2:
            continue
        assert r.push(f) == []
    assert r.finalize() == [("lost", 9)]

    # progressive render: prefix grows monotonically as contiguous fragments arrive
    img = bytes(range(24))                     # 6 data fragments
    frames = packetize(img, 1, 1, src=1, dst=2)
    r = SstvReassembler(progress=True)
    prefixes = []
    for f in frames:
        for ev in r.push(f):
            if ev[0] == "progress":
                prefixes.append(ev[2])
    assert prefixes == [4, 8, 12, 16, 20, 24], prefixes
    print("sstvlab_core selftest: CERTIFIED")


if __name__ == "__main__":
    _selftest()
