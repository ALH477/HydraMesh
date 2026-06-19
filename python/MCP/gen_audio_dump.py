#!/usr/bin/env python3
# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""Generate a ``.dcf`` audio frame-dump for the FFmpeg ``dcf`` demuxer / ``dcf-rec``.

A ``.dcf`` dump is the canonical offline container the ``ff_dcf_demuxer`` reads: a
**flat concatenation of 17-byte DeModFrames** — exactly the AUDIO-adapter frames
that ride on the wire (one ``DeModFrame`` per UDP ``MSG_AUDIO`` payload). It is
self-synchronising (every record starts with sync ``0xD3`` and ends with a
CRC-16/CCITT over bytes ``[0..14]``), so the demuxer can probe + resync on it.

This tool emits a **byte-deterministic** PCM-diag (codec_id 1) tone so the wire →
ffmpeg → file path can be certified end-to-end: the bytes the demuxer hands to
FFmpeg's ``pcm_u8`` decoder are *exactly* the bytes written here. It reuses the
certified L2 framing (``audiolab_core.packetize``) and PCM-diag codec, so a dump is
provably the same ``DeModFrame``s the 246-vector wire certificate pins.

Each ``--src`` becomes one audio stream in the demuxer (keyed by the frame ``src``
id). For each source we also write a sidecar ``<out>.src<ID>.u8`` holding the raw
unsigned-8 PCM the tone encoded to — the ground truth for a byte-exact round-trip:

    ffmpeg -f dcf -i tone.dcf -map 0:a:0 -f u8 -ac 1 -ar 6000 out.u8
    cmp out.u8 tone.dcf.src1.u8          # must be identical

Usage:
    python3 python/MCP/gen_audio_dump.py tone.dcf
    python3 python/MCP/gen_audio_dump.py two.dcf --src 1 --src 7 --blocks 25
"""
import argparse
import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import audiolab_core as al  # certified L2 framing + PCM-diag codec

TS_MASK = (1 << 24) - 1  # frame timestamp is 24-bit microseconds


def _tone_block(freq, rate, block, phase):
    """One `block`-sample mono sine block at `freq` Hz, returning (samples, phase)."""
    samples = []
    for _ in range(block):
        samples.append(0.6 * math.sin(phase))
        phase += 2.0 * math.pi * freq / rate
    return samples, phase


def gen_source(frames_out, pcm_out, src_id, freq, blocks):
    """Append `blocks` PCM-diag frames for one source; return the raw u8 PCM bytes."""
    rate, block = al.PCM_DIAG_RATE, al.PCM_DIAG_BLOCK  # 6000 Hz, 120 samples/block
    phase = 0.0
    pcm_u8 = bytearray()
    for pid in range(blocks):
        samples, phase = _tone_block(freq, rate, block, phase)
        payload = al.pcm_diag_encode(samples)          # 120 bytes, byte-deterministic
        pcm_u8 += payload
        ts_us = (pid * 20_000) & TS_MASK
        flags = al.FLAG_END_TALKSPURT if pid == blocks - 1 else 0
        for fr in al.packetize(al.CODEC_PCM_DIAG, payload, pid, ts_us,
                               src_id, 0xFFFF, flags):
            frames_out.append((pid, fr))
    pcm_out[src_id] = bytes(pcm_u8)
    return pcm_out[src_id]


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("out", help="output .dcf path")
    ap.add_argument("--src", type=lambda s: int(s, 0), action="append",
                    metavar="ID", help="source node id (repeatable; default 1)")
    ap.add_argument("--blocks", type=int, default=50,
                    help="20 ms blocks per source (default 50 = 1.0 s)")
    ap.add_argument("--freq", type=float, default=440.0,
                    help="base tone Hz for the first source (default 440)")
    ap.add_argument("--no-sidecar", action="store_true",
                    help="skip writing the per-source raw u8 PCM ground-truth files")
    args = ap.parse_args(argv)

    srcs = args.src or [1]
    frames = []          # list of (block_index, 17-byte frame)
    pcm = {}             # src_id -> raw u8 PCM bytes
    for i, sid in enumerate(srcs):
        gen_source(frames, pcm, sid, args.freq * (i + 1), args.blocks)

    # Interleave by block so multiple sources are multiplexed like the live wire.
    frames.sort(key=lambda bf: bf[0])
    with open(args.out, "wb") as f:
        for _, fr in frames:
            f.write(fr)

    n_audio_frames = len(frames)
    print(f"wrote {args.out}: {n_audio_frames} frames "
          f"({n_audio_frames * al.__dict__.get('FRAME_SIZE', 17)} bytes), "
          f"{len(srcs)} source(s) x {args.blocks} blocks, codec=pcm-diag")
    if not args.no_sidecar:
        for sid, raw in pcm.items():
            side = f"{args.out}.src{sid}.u8"
            with open(side, "wb") as f:
                f.write(raw)
            print(f"  ground truth: {side} ({len(raw)} bytes u8 @ 6000 Hz)")


if __name__ == "__main__":
    main()
