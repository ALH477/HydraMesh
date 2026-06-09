"""Executable laws for the wire quantum. Exit 0 iff every law holds."""
import json, os, random, sys
from wirelab_core import crc16_ccitt, encode, decode, syndrome, FRAME_LEN

random.seed(0xD3)
ok = lambda name: print(f"  PASS  {name}")

# Law 0: CRC anchor (CRC-16/CCITT-FALSE check value)
assert crc16_ccitt(b"123456789") == 0x29B1; ok("crc anchor '123456789' -> 0x29B1")

# Law 1: CRC affinity over GF(2): crc(x^y) = crc(x) ^ crc(y) ^ crc(0)
c0 = crc16_ccitt(bytes(15))
for _ in range(500):
    x = bytes(random.randrange(256) for _ in range(15))
    y = bytes(random.randrange(256) for _ in range(15))
    xy = bytes(a ^ b for a, b in zip(x, y))
    assert crc16_ccitt(xy) == crc16_ccitt(x) ^ crc16_ccitt(y) ^ c0
ok(f"crc is affine over GF(2) (500 random pairs); crc(0^15) = 0x{c0:04X}")

# Law 2: section/retraction — decode(encode(phi)) == phi (random + corners)
def rand_fields():
    return dict(frame_type=random.randrange(4), seq=random.randrange(1 << 16),
                src=random.randrange(1 << 16), dst=random.randrange(1 << 16),
                payload=bytes(random.randrange(256) for _ in range(4)),
                ts_us=random.randrange(1 << 24))
cases = [rand_fields() for _ in range(2000)]
cases += [dict(frame_type=t, seq=s, src=s, dst=0xFFFF, payload=p, ts_us=u)
          for t in range(4) for s in (0, 0xFFFF) for p in (bytes(4), b"\xff" * 4)
          for u in (0, 0xFFFFFF)]
for f in cases:
    w = encode(**f)
    d = decode(w)
    assert (d["frame_type"], d["seq"], d["src"], d["dst"], bytes.fromhex(d["payload"]), d["ts_us"]) == \
           (f["frame_type"], f["seq"], f["src"], f["dst"], f["payload"], f["ts_us"])
ok(f"decode∘encode = id on {len(cases)} frames (random + corner)")

# Law 3: image — encode(decode(w)) == w for every valid w we can produce
for f in cases[:500]:
    w = encode(**f); d = decode(w)
    assert encode(d["frame_type"], d["seq"], d["src"], d["dst"], bytes.fromhex(d["payload"]), d["ts_us"]) == w
ok("encode∘decode = id on Valid (500 frames)")

# Law 4: quanta separation — every single-bit flip of a valid frame is rejected
w = encode(3, 0x1234, 0x0001, 0xFFFF, bytes.fromhex("DEADBEEF"), 0xAB12CD)
rejected = 0
for bit in range(FRAME_LEN * 8):
    bad = bytearray(w); bad[bit // 8] ^= 1 << (7 - bit % 8)
    try: decode(bytes(bad))
    except ValueError: rejected += 1
assert rejected == FRAME_LEN * 8
ok(f"all {FRAME_LEN*8} single-bit corruptions of exampleFrame rejected")

# Law 5: aligned unique factorization — concat of k frames re-splits uniquely
stream = b"".join(encode(**rand_fields()) for _ in range(64))
parts = [stream[i:i+17] for i in range(0, len(stream), 17)]
assert all(decode(p) for p in parts) and b"".join(parts) == stream
ok("aligned stream of 64 quanta factors uniquely")

# Reference vector for the Haskell exampleFrame body (FrameSpec.hs)
body = bytes.fromhex("D31012340001FFFFDEADBEEFAB12CD")
print(f"  INFO  exampleFrame body CRC-16/CCITT-FALSE = 0x{crc16_ccitt(body):04X}")
print(f"  INFO  full exampleFrame (17B)              = {w.hex().upper()}")

# Emit the finite certificate: encode basis (109 vectors) + syndrome basis (137 vectors)
FIELD_BITS = [("frame_type", 4), ("seq", 16), ("src", 16), ("dst", 16), ("payload", 32), ("ts_us", 24)]
def fields_from_bits(n: int):
    f, shift = {}, 0
    for name, width in FIELD_BITS:
        f[name] = (n >> shift) & ((1 << width) - 1); shift += width
    f["payload"] = f["payload"].to_bytes(4, "big")
    return f
enc_basis = [{"input_bits": "0" * 27, "frame": encode(**fields_from_bits(0)).hex()}]
for i in range(108):
    enc_basis.append({"input_bit": i, "frame": encode(**fields_from_bits(1 << i)).hex()})
syn_basis = [{"word": "00" * 17, "syndrome": syndrome(bytes(17))}]
for i in range(136):
    word = bytearray(17); word[i // 8] = 1 << (7 - i % 8)
    syn_basis.append({"bit": i, "syndrome": syndrome(bytes(word))})
cert = {
    "format": "DCF DeModFrame v1 (17-byte wire quantum)",
    "crc": "CRC-16/CCITT-FALSE poly=0x1021 init=0xFFFF refin=false refout=false xorout=0x0000 over bytes[0..14]",
    "anchors": {
        "crc_123456789": "0x29B1",
        "crc_zero15": f"0x{c0:04X}",
        "exampleFrame_body_crc": f"0x{crc16_ccitt(body):04X}",
        "exampleFrame_full": w.hex(),
    },
    "theorem": ("encode is affine over GF(2)^108 -> GF(2)^136 and the validity syndrome is affine "
                "GF(2)^136 -> GF(2)^16; any implementation that is bit-placement+CRC (hence affine) and "
                "matches encode_basis (109 vectors) and syndrome_basis (137 vectors) equals the reference "
                "on ALL 2^108 frames and classifies ALL 2^136 words identically."),
    "encode_basis": enc_basis,
    "syndrome_basis": syn_basis,
}
out = sys.argv[1] if len(sys.argv) > 1 else "golden_vectors.json"
with open(out, "w") as fh: json.dump(cert, fh, indent=1)
print(f"  INFO  wrote certificate: {out} ({os.path.getsize(out)} bytes, "
      f"{len(enc_basis)} encode vectors, {len(syn_basis)} syndrome vectors)")

# Law 6 (meta): verify the certificate proves what it claims — perturb one bit of the
# reference and confirm the basis catches it (simulated wrong implementation).
def bad_encode(**kw):
    w2 = bytearray(encode(**kw)); w2[9] ^= 0x01  # systematic payload-bit corruption
    return bytes(w2)
caught = any(bad_encode(**fields_from_bits(1 << v.get("input_bit", 0) if "input_bit" in v else 0)).hex() != v["frame"]
             for v in enc_basis)
assert caught
ok("certificate detects a systematically wrong implementation")
print("ALL LAWS HOLD")
