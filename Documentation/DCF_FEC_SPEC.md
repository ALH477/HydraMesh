# DCF-FEC — forward error correction (Reed-Solomon)

DCF frames carry a CRC-16 that **detects** corruption. For lossy media (RF/SDR,
acoustic) that isn't enough — a single bit error drops the frame. DCF-FEC adds
**correction**: a systematic Reed-Solomon code, plus a block interleaver for RF
bursts. It is an *adapter around* the 17-byte `DeModFrame` — the frame and its
246-vector certificate are untouched. FEC wraps the frame, the medium corrupts it,
RS recovers it, then the frame CRC validates.

This is a **certified** adapter: encode + the correction law are byte-deterministic
and golden-vectored across Python/C/Rust (like SuperPack).

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

## References (byte-identical)

| Lang | File | Entry points |
|------|------|------|
| Python | `python/MCP/feclab_core.py` | `rs_encode`, `rs_decode`, `interleave`, `deinterleave` |
| C | `codec/demod_fec.h` | `dcf_fec_encode`, `dcf_fec_decode`, `dcf_fec_interleave` |
| Rust | `codec/src/fec.rs` | `rs_encode`, `rs_decode`, `interleave` |

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
