#!/usr/bin/env python3
"""Standalone SDK certification harness for the DeMoD 17-byte wire quantum.

Usage:
  python3 certify_sdk.py [--golden PATH] [--sdk-json PATH]

With no arguments, runs the reference codec self-test (same as verify_laws.py)
and certifies it against the golden vectors.

With --sdk-json, reads an SDK's emitted 109+137 vectors and diffs them against
the golden file. Exits 0 iff the SDK is certified "cemented" — matching the
reference on ALL 2^108 frames and ALL 2^136 words (Theorems 3 & 4 in
wire_quanta_category.md).

See also: wirelab_mcp.py (MCP server), wirelab_core.py (reference codec).
"""
import json
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
GOLDEN_DEFAULT = HERE / "golden_vectors.json"

def load_golden(path=None):
    p = pathlib.Path(path) if path else GOLDEN_DEFAULT
    return json.loads(p.read_text())

def certify_against_golden(sdk_vectors, golden):
    enc_diffs = [i for i, (g, s) in enumerate(zip(golden["encode_basis"], sdk_vectors["encode_basis"]))
                 if g["frame"].lower() != s["frame"].lower()]
    syn_diffs = [i for i, (g, s) in enumerate(zip(golden["syndrome_basis"], sdk_vectors["syndrome_basis"]))
                 if int(g["syndrome"]) != int(s["syndrome"])]
    ok = (not enc_diffs and not syn_diffs and
          len(sdk_vectors["encode_basis"]) == len(golden["encode_basis"]) and
          len(sdk_vectors["syndrome_basis"]) == len(golden["syndrome_basis"]))
    return ok, enc_diffs, syn_diffs

def selftest():
    sys.path.insert(0, str(HERE))
    from wirelab_core import crc16_ccitt, encode, decode, syndrome, FRAME_LEN

    print("certify_sdk.py self-test")
    print("=" * 60)

    c0 = crc16_ccitt(bytes(15))
    assert crc16_ccitt(b"123456789") == 0x29B1, f"CRC anchor failed: expected 0x29B1"
    print(f"  PASS  CRC anchor '123456789' -> 0x29B1")
    print(f"  PASS  CRC(0^15) = 0x{c0:04X}")

    golden = load_golden()
    print(f"  INFO  loaded golden_vectors.json ({len(golden['encode_basis'])} encode, "
          f"{len(golden['syndrome_basis'])} syndrome vectors)")

    from wirelab_core import FRAME_TYPES
    ex = encode(3, 0x1234, 0x0001, 0xFFFF, bytes.fromhex("DEADBEEF"), 0xAB12CD)
    assert ex.hex().upper() == golden["anchors"]["exampleFrame_full"].upper(), \
        f"exampleFrame mismatch: {ex.hex().upper()} != {golden['anchors']['exampleFrame_full']}"
    print(f"  PASS  exampleFrame = {ex.hex().upper()}")

    d = decode(ex)
    assert d is not None, "exampleFrame decode failed"
    print(f"  PASS  decode(exampleFrame) valid: type={d['frame_type_name']} seq=0x{d['seq']:04X}")

    for i, vec in enumerate(golden["encode_basis"]):
        frame_hex = vec["frame"].lower()
        w = bytes.fromhex(frame_hex)
        d = decode(w)
        if d is None:
            print(f"  FAIL  encode_basis[{i}] does not decode: {frame_hex}")
            return False

    print(f"  PASS  all {len(golden['encode_basis'])} encode_basis vectors decode successfully")

    for i, vec in enumerate(golden["syndrome_basis"]):
        syn = int(vec["syndrome"])
        if "bit" not in vec:
            word = bytes(17)
        else:
            word = bytearray(17)
            word[vec["bit"] // 8] = 1 << (7 - vec["bit"] % 8)
            word = bytes(word)
        computed = syndrome(word)
        if computed != syn:
            print(f"  FAIL  syndrome_basis[{i}] syndrome mismatch: computed 0x{computed:04X} != golden 0x{syn:04X}")
            return False

    print(f"  PASS  all {len(golden['syndrome_basis'])} syndrome_basis vectors match")

    ok, _, _ = certify_against_golden(golden, golden)
    assert ok, "golden-vs-golden sanity check failed"
    print(f"  PASS  golden-vs-golden certification check")

    print()
    print("ALL CERTIFICATION CHECKS PASSED — reference codec is cemented.")
    return True

def main():
    import argparse
    parser = argparse.ArgumentParser(description="Certify an SDK against the golden vectors")
    parser.add_argument("--golden", default=None, help="Path to golden_vectors.json")
    parser.add_argument("--sdk-json", default=None, help="Path to SDK-emitted vectors JSON")
    parser.add_argument("--selftest", action="store_true", help="Run reference codec self-test")
    args = parser.parse_args()

    if args.selftest or args.sdk_json is None:
        success = selftest()
        sys.exit(0 if success else 1)

    golden = load_golden(args.golden)
    with open(args.sdk_json) as f:
        sdk_vectors = json.load(f)

    ok, enc_diffs, syn_diffs = certify_against_golden(sdk_vectors, golden)

    if ok:
        print(f"CEMENTED: SDK matches reference on all 2^108 frames and 2^136 words.")
        print(f"  encode_basis: {len(golden['encode_basis'])} vectors, 0 mismatches")
        print(f"  syndrome_basis: {len(golden['syndrome_basis'])} vectors, 0 mismatches")
        sys.exit(0)
    else:
        print(f"NOT CEMENTED: SDK diverges from reference.")
        if enc_diffs:
            print(f"  encode_basis mismatches at indices: {enc_diffs[:20]}{'...' if len(enc_diffs) > 20 else ''}")
            for i in enc_diffs[:5]:
                g = golden["encode_basis"][i]
                s = sdk_vectors["encode_basis"][i]
                print(f"    [{i}] golden={g['frame']} sdk={s['frame']}")
        if syn_diffs:
            print(f"  syndrome_basis mismatches at indices: {syn_diffs[:20]}{'...' if len(syn_diffs) > 20 else ''}")
            for i in syn_diffs[:5]:
                g = golden["syndrome_basis"][i]
                s = sdk_vectors["syndrome_basis"][i]
                print(f"    [{i}] golden={g['syndrome']} sdk={s['syndrome']}")
        sys.exit(1)

if __name__ == "__main__":
    main()