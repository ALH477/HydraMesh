# DCF Wire Quantum — Canonical Specification

**Version 1** · DeMoD LLC · This document is normative.

The DCF wire protocol has exactly **one** wire format: the 17-byte `DeModFrame`
(version nibble = 1). All other formats (the proto-message UDP header, the C
SDK's `DCFMessageHeader`) are **adapters** over this quantum and must be
explicitly documented as such.

## Wire Layout

```
 Byte  Field        Width   Description
 ────  ──────────  ─────   ─────────────────────────────────────────
  0    sync         1 B     Fixed 0xD3 (first validity gate)
  1    flags        1 B     [7:4] version (currently 1) | [3:0] type (0–3)
  2–3  seq          2 B     Big-endian u16, rolling sequence counter
  4–5  src_id       2 B     Big-endian u16, source node ID
  6–7  dst_id       2 B     Big-endian u16, destination (0xFFFF = broadcast)
  8–11 payload      4 B     Application data
 12–14 timestamp    3 B     24-bit µs offset, big-endian, wraps ~16.7 s
 15–16 crc16        2 B     CRC-16/CCITT-FALSE over bytes [0..14]
```

**Total: 17 bytes = 136 bits.** The free field data is 108 bits (4+16+16+16+32+24).

## Frame Types

| Value | Name   | Purpose                     |
|-------|--------|-----------------------------|
| 0     | DATA   | Application payload        |
| 1     | ACK    | Acknowledgement              |
| 2     | BEACON | Clock sync / broadcast      |
| 3     | CTRL   | Control / fragmented audio   |

## Validity

A frame is valid iff:
1. `frame[0] == 0xD3` (sync byte)
2. `(frame[1] >> 4) == 1` (version nibble)
3. `CRC-16/CCITT-FALSE(frame[0..14]) == (frame[15] << 8) | frame[16]` (CRC)

For a random byte stream, the probability of a 17-byte window passing all three
checks is 2⁻²⁸ (Theorem 2.1, `wire_quanta_category.md`).

## CRC-16/CCITT-FALSE

- Polynomial: `0x1021`
- Init: `0xFFFF`
- RefIn/RefOut: `false`
- XorOut: `0x0000`
- Check value: `CRC("123456789") = 0x29B1`

## Anchors

| Quantity | Value | Source |
|----------|-------|--------|
| CRC("123456789") | `0x29B1` | Reference check value |
| CRC(0¹⁵) | `0x4EC3` | Affine offset c |
| CRC(exampleFrame body) | `0xA963` | Cross-language anchor |
| exampleFrame | `D31312340001FFFFDEADBEEFAB12CD24C0` | type=CTRL, seq=0x1234, src=1, dst=0xFFFF, payload=DEADBEEF, ts=AB12CD |

## Formal Properties

The frame is a **cemented retract** (Theorem 1, `wire_quanta_category.md`):
- `encode` is injective with a left inverse `decode` (decode∘encode = id)
- `encode` is a bijection onto the set of valid words
- `decode` is the unique decoder satisfying the spec

The 246-vector finite certificate (Theorem 4) proves that any implementation
matching the 109 encode-basis + 137 syndrome-basis vectors equals the reference
on all 2¹⁰⁸ frames and classifies all 2¹³⁶ words identically. See
`Documentation/golden_vectors.json`.

## Reference Implementations

| Language | File | Functions |
|----------|------|-----------|
| C | `codec/demod_frame.h` | `dcf_frame_encode`, `dcf_frame_decode`, `dcf_frame_crc` |
| Rust | `codec/frame.rs` | `Frame::encode`, `Frame::decode` |
| Python | `python/MCP/wirelab_core.py` | `encode`, `decode`, `syndrome`, `crc16_ccitt` |
| Lua | `GUI/wirelab.lua` | `encode`, `decode`, `crc16` |

## Certification

To certify a new SDK against the reference:

```sh
python3 python/MCP/certify_sdk.py --selftest          # reference self-test
python3 python/MCP/certify_sdk.py --sdk-json SDK.json # certify an external SDK
```

All CI gates run `python/MCP/verify_laws.py` and `python/MCP/certify_sdk.py --selftest`.
Under Theorem 4, passing the 246-vector test is equivalent to agreement on the
entire 2¹⁰⁸ × 2¹³⁶ input space.

## Other Wire Formats ARE Adapters

| Format | Status | Relationship to DeModFrame |
|--------|--------|----------------------------|
| Proto-message UDP header (C SDK) | Adapter | Must marshal to/from DeModFrame for wire |
| `DCFMessageHeader` (C SDK) | Internal | In-process struct; must not appear on the wire |
| Haskell `FrameSpec` | Implemented | `haskell/src/DCF/Transport/FrameSpec.hs` — encodeFrame / decodeFrame |

Any new transport MUST emit and consume DeModFrame v1 on the wire. Internal
representations are free to differ but MUST be convertible to/from DeModFrame
without loss.