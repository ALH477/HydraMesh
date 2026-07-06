# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-SSTV demo — send a still image over a DCF transport and render it progressively.

An image is fragmented into DeModFrame DATA frames by the certified DCF-SSTV L2 framing
(``python/MCP/sstvlab_core.py``) and pushed through any DCF ``Transport``.  The receiver
reassembles with ``SstvReassembler(progress=True)`` so it paints the image top-down as
fragments trickle in — the "slow-scan" experience.

  # hardware-free (in-process loopback), byte-exact round trip:
  python3 python/dcf/sstv_demo.py --image photo.jpg --transport loopback

  # over the real HydraModem acoustic link (needs frame_tx/frame_rx on PATH):
  python3 python/dcf/sstv_demo.py --image photo.jpg --transport hydra --fec conv

  # no image handy?  --make-sample writes a tiny gradient JPEG (needs Pillow) or a raw blob.
  python3 python/dcf/sstv_demo.py --make-sample /tmp/sample.jpg --image /tmp/sample.jpg

The image bytes are opaque to L2; ``--format`` is only a descriptor hint.  A single image_id
carries <= 8188 B (a low-res thumbnail); larger images chain across image_ids with FLAG_MORE.
"""
import argparse
import os
import sys
import threading
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP"))
import sstvlab_core as sstv  # noqa: E402

from .transport import LoopbackTransport, LoopbackMedium, HydraTransport  # noqa: E402

FORMATS = {"raw": sstv.FMT_RAW, "jpeg": sstv.FMT_JPEG, "png": sstv.FMT_PNG,
           "webp": sstv.FMT_WEBP, "rgb565": sstv.FMT_RGB565}


def make_sample(path):
    """Write a small sample image: a gradient JPEG if Pillow is present, else a raw blob."""
    try:
        from PIL import Image  # optional; only for a nice sample
        w, h = 64, 48
        img = Image.new("RGB", (w, h))
        img.putdata([((x * 4) % 256, (y * 5) % 256, ((x + y) * 3) % 256)
                     for y in range(h) for x in range(w)])
        img.save(path, "JPEG", quality=40)
        print(f"make-sample: wrote {path} ({os.path.getsize(path)} B JPEG {w}x{h})")
    except ImportError:
        blob = bytes((i * 73 + 11) & 0xFF for i in range(1200))
        with open(path, "wb") as fh:
            fh.write(blob)
        print(f"make-sample: Pillow not installed; wrote {path} ({len(blob)} B raw blob)")


def build_transports(kind, fec):
    """Return (tx, rx) transports.  loopback = in-process; hydra = real acoustic (file WAVs)."""
    if kind == "loopback":
        medium = LoopbackMedium()
        # pace the loopback at a slow rate so the progressive render is visible
        tx = LoopbackTransport("sstv-tx", medium, rate_bps=2000, pace=True)
        rx = LoopbackTransport("sstv-rx", medium)
        return tx, rx
    if kind == "hydra":
        # two nodes sharing a dir pair (A.out == B.in); HydraTransport shells to frame_tx/rx
        base = os.path.join(os.getcwd(), "sstv_link")
        a2b = os.path.join(base, "a2b")
        os.makedirs(a2b, exist_ok=True)
        tx = HydraTransport("sstv-tx", fec=fec, out_dir=a2b, in_dir=os.path.join(base, "b2a"))
        rx = HydraTransport("sstv-rx", fec=fec, out_dir=os.path.join(base, "b2a"), in_dir=a2b)
        return tx, rx
    raise SystemExit(f"unknown transport {kind!r}")


def main(argv=None):
    ap = argparse.ArgumentParser(description="Send a still image over DCF-SSTV.")
    ap.add_argument("--image", help="image file to send (opaque bytes, <= 8188 B)")
    ap.add_argument("--out", help="write the recovered image here (default: <image>.recovered)")
    ap.add_argument("--transport", choices=["loopback", "hydra"], default="loopback")
    ap.add_argument("--format", choices=list(FORMATS), default="jpeg")
    ap.add_argument("--fec", choices=["none", "rep3", "conv"], default="conv",
                    help="HydraModem FEC (hydra transport only)")
    ap.add_argument("--image-id", type=int, default=0)
    ap.add_argument("--channel", default="sstv", help="rendezvous channel name (-> dst)")
    ap.add_argument("--make-sample", metavar="PATH",
                    help="write a sample image to PATH and exit (or use with --image)")
    ap.add_argument("--timeout", type=float, default=120.0)
    args = ap.parse_args(argv)

    if args.make_sample:
        make_sample(args.make_sample)
        if not args.image:
            return 0

    if not args.image:
        ap.error("--image is required (or pass --make-sample PATH --image PATH)")
    with open(args.image, "rb") as fh:
        payload = fh.read()
    if len(payload) > sstv.MAX_PAYLOAD:
        ap.error(f"image {len(payload)} B exceeds {sstv.MAX_PAYLOAD} B cap for one image_id")

    dst = sstv.channel_id(args.channel)
    src = 0x00A1
    done = threading.Event()
    recovered = {}
    reasm = sstv.SstvReassembler(accept_dst=dst, progress=True)
    total_frags = (len(payload) + 3) // 4

    def on_frame(frame, meta):
        for ev in reasm.push(frame):
            if ev[0] == "progress":
                _, iid, prefix = ev
                pct = 100.0 * prefix / max(1, len(payload))
                bar = int(pct / 2.5)
                print(f"\r  scan image {iid}: [{'#' * bar:<40}] {prefix}/{len(payload)} B ({pct:4.0f}%)",
                      end="", flush=True)
            elif ev[0] == "image":
                _, iid, ts, s, d, fmt, data, flags = ev
                recovered["data"] = data
                recovered["format_id"] = fmt
                print(f"\n  COMPLETE image {iid}: {len(data)} B, format={fmt}, flags=0x{flags:02X}")
                done.set()

    tx, rx = build_transports(args.transport, args.fec)
    rx.start(on_frame)
    tx.start(lambda *a: None)
    try:
        frames = sstv.packetize(payload, args.image_id, ts_us=int(time.time() * 1e6) & 0xFFFFFF,
                                src=src, dst=dst, format_id=FORMATS[args.format],
                                flags=sstv.FLAG_KEYFRAME)
        print(f"sstv_demo: {args.image} ({len(payload)} B) -> {len(frames)} frames "
              f"({total_frags} data frags) over {args.transport}, channel {args.channel!r} "
              f"(dst 0x{dst:04X})")
        for f in frames:
            tx.send(f)
        if not done.wait(args.timeout):
            print(f"\n  TIMEOUT after {args.timeout}s; image incomplete "
                  f"({reasm.finalize()})")
            return 1
    finally:
        tx.stop()
        rx.stop()

    out_path = args.out or (args.image + ".recovered")
    with open(out_path, "wb") as fh:
        fh.write(recovered["data"])
    exact = recovered["data"] == payload
    print(f"  wrote {out_path} ({len(recovered['data'])} B) — "
          f"{'BYTE-EXACT with the source' if exact else 'MISMATCH!'}")
    return 0 if exact else 1


if __name__ == "__main__":
    raise SystemExit(main())
