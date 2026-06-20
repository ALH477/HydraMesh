# DCF-FEC — forward error correction (Reed-Solomon)

DCF frames carry a CRC-16 that **detects** corruption. For lossy media (RF/SDR,
acoustic) that isn't enough — a single bit error drops the frame. DCF-FEC adds
**correction**: a systematic Reed-Solomon code, plus a block interleaver for RF
bursts. It is an *adapter around* the 17-byte `DeModFrame` — the frame and its
246-vector certificate are untouched. FEC wraps the frame, the medium corrupts it,
RS recovers it, then the frame CRC validates.

This is a **certified** adapter: encode + the correction law are byte-deterministic
and golden-vectored across **all thirteen wire-codec languages** (like SuperPack).

## Code

- Field **GF(2⁸)**, primitive polynomial **0x11D**, generator **α = 2**, first
  consecutive root **fcr = 0** (the QR/DVB field).
- **Systematic**: a codeword is `message ++ parity` — an undamaged frame passes
  straight through; parity is pure overhead.
- Default parity **2t = 16** bytes → corrects up to **t = 8 byte-errors** per
  codeword (a 17-byte frame → a 33-byte codeword). `nparity` is configurable
  (message + parity ≤ 255).
- Decode = syndromes → Berlekamp–Massey → Chien search → Forney.
- **Block interleaver** (depth D): D codewords written as rows, transmitted
  column-major, so a burst of ≤ D consecutive bytes hits ≤ 1 byte per codeword —
  burst errors become correctable random errors.

## Multi-codeword messages (any length)

A single codeword covers ≤ 239 data bytes. For longer payloads, `encode_message`
splits the message into N equal blocks, RS-codes each, and **interleaves** the N
codewords so an RF burst of up to `N·t` bytes is corrected (it spreads to ≤ t per
codeword). A fixed-parity, self-protecting **header codeword** (`[len u32 | parity
u8]`, 21 bytes) carries the metadata so the receiver needs nothing out-of-band:

```
blob = RS_hdr( len | nparity )  ++  interleave( RS(block_0) … RS(block_{N-1}) )
```

`dcfnode send-modem --fec` uses this for arbitrary payloads (no 239-byte limit);
the burst tolerance scales with the message length.

## References (byte-identical across all 13 wire-codec languages)

| Lang | File | Entry points | Cert |
|------|------|------|------|
| Python | `python/MCP/feclab_core.py` | `rs_encode/decode`, `encode_message`, `decode_message` | `python/tests/test_fec.py` |
| C | `codec/demod_fec.h` | `dcf_fec_encode/decode`, `dcf_fec_encode_message`, `dcf_fec_decode_message` | `C_SDK/tests/test_fec_certify.c` |
| Rust | `codec/src/fec.rs` | `rs_encode/decode`, `encode_message`, `decode_message` | `codec/tests/certify_fec.rs` |
| Go | `go/dcf/fec.go` | `RSEncode/RSDecode`, `EncodeMessage`, `DecodeMessage` | `go/dcf/fec_certify_test.go` |
| Node | `JS/nodejs/src/fec.js` | `rsEncode/rsDecode`, `encodeMessage`, `decodeMessage` | `JS/nodejs/test/certify_fec.js` |
| C++ | `cpp/include/dcf/fec.hpp` | `dcf::fec::rs_encode/rs_decode`, `encode_message`, `decode_message` | `cpp/tests/certify_fec.cpp` |
| Perl | `perl/lib/DCF/FEC.pm` | `rs_encode/rs_decode`, `encode_message`, `decode_message` | `perl/t/fec.t` |
| Lua | `lua/dcf_fec.lua` | `rs_encode/rs_decode`, `encode_message`, `decode_message` | self-cert on load |
| Lisp | `lisp/src/fec.lisp` | `rs-encode/rs-decode`, `encode-message`, `decode-message` | self-cert on load |
| Haskell | `haskell/src/DCF/Transport/FEC.hs` | `rsEncode/rsDecode`, `encodeMessage`, `decodeMessage` | `haskell/test/Certify.hs` |
| Java | `java/com/demod/dcf/FEC.java` | `rsEncode/rsDecode`, `encodeMessage`, `decodeMessage` | `java/com/demod/dcf/FECCertify.java` |
| Kotlin | `kotlin/src/main/kotlin/dcf/FEC.kt` | `FEC.rsEncode/rsDecode`, `encodeMessage`, `decodeMessage` | `kotlin/.../Certify.kt` |
| Swift | `swift/Sources/DCFWire/FEC.swift` | `rsEncode/rsDecode`, `encodeMessage`, `decodeMessage` | `swift/Tests/.../FECCertifyTests.swift` |

Every cert diffs against `Documentation/fec_vectors.json` (encode, multi-codeword
message blobs, and the correction law); all are wired into `wire-certify.yml`.

## Certification

`Documentation/fec_vectors.json` (+ identical `python/MCP/` copy) and
`codec/fec_vectors.gen.h` pin: the systematic encode (byte-exact), deterministic
correction of `≤ t` injected errors, and the generator polynomial. Anchors:
`gen_poly(2t=16)` and the zero-frame parity.

```sh
python3 python/MCP/gen_fec_vectors.py /tmp/fec.json      # regen + verify laws
python3 -m unittest python/tests/test_fec.py             # correction + interleaver
cd codec && cargo test --test certify_fec                # Rust
gcc -std=c11 -I codec C_SDK/tests/test_fec_certify.c -o /tmp/fc && /tmp/fc   # C
```

CI (`.github/workflows/wire-certify.yml`) regenerates the vectors, diffs the
committed copies, and runs the Python/C/Rust certs.

## Usage

FEC is the reliability layer beneath the IQ/SDR modem (`DCF_SDR_SPEC.md`):
`frame → rs_encode → symbols → IQ → radio → IQ → symbols → rs_decode → frame`. The
AWGN/burst errors a real radio injects are corrected, not just detected — which is
what makes the DCF wire legitimately usable over FM/AM/SDR.
