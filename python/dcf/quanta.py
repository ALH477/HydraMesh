# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""quanta subprocess bridge for the DCF-Snake record plane.

The DeMoD **quanta** codec (Gabor-atom matching pursuit + QSS streaming) is **GPL-3.0-only OR
DeMoD Commercial**. Like the DCF-JANUS transport shells out to janus-c, DCF-Snake invokes
``quanta-stream`` (encode) and ``quanta-stream-decode`` (decode) as **subprocesses** — never
linked — so the GPL code stays out of the LGPL library's closure (mere aggregation). Install it
standalone: ``nix build .#quanta`` (see Documentation/DCF_SNAKE_SPEC.md, LICENSING.md).

The quanta CLIs are **file-based** (``quanta-stream in.wav -o out.qss``; ``quanta-stream-decode
out.qss --wav out.wav``), so the wrappers here use temp files (the ``_DirMedium`` pattern), not
pipes. Binaries are discovered via ``$QUANTA_STREAM`` / ``$QUANTA_STREAM_DECODE`` then PATH; when
absent, ``quanta_available()`` is False and callers skip gracefully (CI stays green).

This module also provides a **pure, version-independent QSS packet splitter** — the bridge
between a quanta QSS byte stream and DCF-Snake messages. A QSS stream is a 40-byte header
followed by self-delimiting packets, each ``0xA55A`` sync + body + a CRC-16/CCITT (the same CRC
the wire certifies). ``split_qss_packets`` delimits packets by scanning sync words and validating
each candidate's trailing CRC (resyncing past a body byte that merely looks like a sync word), so
it needs **no** quanta-internal knowledge (band_count, atom size) and is unit-testable without the
binary. Each QSS packet maps to one DCF-Snake message (``snakelab_core.packetize``).
"""
import os
import shutil
import subprocess
import sys
import tempfile

for _mcp in (os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP"),
             os.path.join(os.path.dirname(os.path.abspath(__file__)), "MCP")):
    if os.path.isdir(_mcp):
        sys.path.insert(0, _mcp)
from wirelab_core import crc16_ccitt  # noqa: E402  (== quanta's qss_crc16, DCF convention)

# QSS container constants (quanta/include/qss.h).
QSS_HDR_BYTES = 40
QSS_MAGIC = 0x51535331     # 'QSS1' uncompressed (legacy/debug)
QSS2_MAGIC = 0x51535332    # 'QSS2' coded (default)
QSS_SYNC = 0xA55A          # packet re-anchor word

# quanta streaming modes → DCF-Snake mode_id (snakelab_core.MODE_*).
MODES = ("live", "near", "relaxed")


def quanta_stream_bin():
    return os.environ.get("QUANTA_STREAM") or shutil.which("quanta-stream")


def quanta_decode_bin():
    return os.environ.get("QUANTA_STREAM_DECODE") or shutil.which("quanta-stream-decode")


def quanta_available():
    """True iff both quanta streaming binaries are reachable (PATH or env vars)."""
    return bool(quanta_stream_bin() and quanta_decode_bin())


def _require():
    if not quanta_available():
        raise RuntimeError(
            "quanta not found: need quanta-stream/quanta-stream-decode on PATH or "
            "$QUANTA_STREAM/$QUANTA_STREAM_DECODE. Install the GPL-3.0 codec "
            "(`nix build .#quanta`). See Documentation/DCF_SNAKE_SPEC.md")


# ── pure QSS container splitting (the quanta ⇄ DCF-Snake bridge) ──────────────
def _packet_crc_ok(pkt):
    """A QSS packet is [sync u16][hop_index..body][crc16]; the CRC covers everything between
    the sync word and the trailing CRC (quanta qss_write_packet)."""
    if len(pkt) < 4 or pkt[0] != 0xA5 or pkt[1] != 0x5A:
        return False
    return crc16_ccitt(pkt[2:-2]) == int.from_bytes(pkt[-2:], "big")


def parse_qss_header(stream):
    """Return the validated 40-byte header, or raise ValueError. Checks magic + header CRC
    (CRC-16/CCITT over the first 32 bytes, stored big-endian at bytes 32..33)."""
    if len(stream) < QSS_HDR_BYTES:
        raise ValueError("short QSS stream")
    hdr = bytes(stream[:QSS_HDR_BYTES])
    magic = int.from_bytes(hdr[:4], "big")
    if magic not in (QSS_MAGIC, QSS2_MAGIC):
        raise ValueError(f"bad QSS magic 0x{magic:08x}")
    if crc16_ccitt(hdr[:32]) != int.from_bytes(hdr[32:34], "big"):
        raise ValueError("QSS header CRC mismatch")
    return hdr


def split_qss_packets(stream):
    """Split a QSS byte stream into (header, [packet_bytes]).

    Delimits packets by scanning ``0xA55A`` sync words and validating each candidate packet's
    trailing CRC — resyncing past a body byte-pair that merely looks like a sync word. Version-
    independent (no atom/band knowledge). Trailing bytes that never form a valid packet (a
    truncated final packet) are dropped.
    """
    hdr = parse_qss_header(stream)
    body = bytes(stream[QSS_HDR_BYTES:])
    n = len(body)
    starts = [p for p in range(n - 1) if body[p] == 0xA5 and body[p + 1] == 0x5A]
    starts.append(n)  # EOF sentinel
    packets = []
    si = 0
    while si < len(starts) - 1:
        start = starts[si]
        emitted = False
        for ej in range(si + 1, len(starts)):
            pkt = body[start:starts[ej]]
            if _packet_crc_ok(pkt):
                packets.append(pkt)
                si = ej
                emitted = True
                break
        if not emitted:
            break  # truncated / trailing garbage
    return hdr, packets


def join_qss_packets(header, packets):
    """Reassemble a QSS byte stream from a header + packets (inverse of split_qss_packets)."""
    return bytes(header) + b"".join(bytes(p) for p in packets)


# ── subprocess encode/decode (file-based, like the _DirMedium modems) ─────────
def encode_wav(wav_path, mode="live", seed=None, timeout=120):
    """Run quanta-stream on a WAV file; return the raw QSS stream bytes."""
    _require()
    if mode not in MODES:
        raise ValueError(f"mode must be one of {MODES}")
    with tempfile.TemporaryDirectory() as d:
        qss = os.path.join(d, "out.qss")
        cmd = [quanta_stream_bin(), wav_path, "-o", qss, "--mode", mode]
        if seed is not None:
            cmd += ["--seed", str(seed)]
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL,
                       stderr=subprocess.DEVNULL, timeout=timeout)
        with open(qss, "rb") as fh:
            return fh.read()


def decode_wav(qss_bytes, timeout=120):
    """Run quanta-stream-decode on a QSS stream; return the decoded WAV file bytes."""
    _require()
    with tempfile.TemporaryDirectory() as d:
        qss = os.path.join(d, "in.qss")
        wav = os.path.join(d, "out.wav")
        with open(qss, "wb") as fh:
            fh.write(bytes(qss_bytes))
        subprocess.run([quanta_decode_bin(), qss, "--wav", wav], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=timeout)
        with open(wav, "rb") as fh:
            return fh.read()
