# DCF SuperPack — 32-byte container for two DeModFrame quanta

Status: reference implementation (`python/MCP/superpack.py`), Python-certified via
self-test + `matrix-bridge/tests/test_bridge_wire.py`. Cross-language ports
(C/Rust/Lua/Haskell/Lisp) are follow-up work; see *Certification* below.

## Motivation

The 17-byte `DeModFrame` (see `WIRE_QUANTUM_SPEC.md`) is the one wire invariant.
When frames are emitted in **pairs** — a descriptor plus its first data fragment,
or two fragments of one message — the second header is largely recoverable from
context. SuperPack removes that redundancy without ever changing the quantum:
the two frames it carries are reconstructed **bit-exact** on unpack.

Two raw frames cost `2 × 17 = 34` bytes. SuperPack is **32** — a 2-byte (5.9%)
saving per pair — and replaces the two independent inner CRCs with a single CRC
covering **both** quanta jointly (strictly stronger integrity).

## Where the bytes go

Per frame, three of the 17 bytes are recoverable, so a pair has 6 redundant bytes:

| Redundant in a pair | Bytes | Why recoverable |
|---|---|---|
| inner sync ×2 | 2 | always the constant `0xD3` |
| inner CRC-16 ×2 | 4 | each is a pure function of its frame's other 15 bytes |

SuperPack drops all 6 and spends 4 back: a single outer sync, a 1-byte
type/version tag, and one joint CRC-16. Net `6 − 4 = 2` bytes saved.

## Layout (32 bytes, big-endian)

```
 [0]      sync   = 0xD3
 [1]      sflags = version[7:4]=1 | type[3:0]=SUPER (0x5)   => 0x15
 [2..15]  frame A core  = A's bytes [1..14]  (flags, seq, src, dst, payload[4], ts24)
 [16..29] frame B core  = B's bytes [1..14]
 [30..31] CRC-16/CCITT-FALSE over bytes [0..29]
```

A *core* is a full frame minus its sync byte (`[0]`) and its CRC (`[15..16]`).

## Operations

**pack(A, B):** validate each input is a 17-byte frame with good sync, version
nibble = 1, and a correct inner CRC (never pack a corrupt frame); concatenate
`0xD3 | sflags | core(A) | core(B)`; append CRC-16 over `[0..29]`.

**unpack(S):** require `len == 32`, `S[0] == 0xD3`, version nibble = 1,
`type == SUPER`, and CRC over `[0..29]` matches. For each core, rebuild the frame
as `0xD3 | core | CRC16(0xD3 | core)`; both reconstructed frames must `decode()`
cleanly. Returns `(A, B)`, each a valid `DeModFrame`.

`is_superpack(buf)`: `len == 32 and buf[0] == 0xD3 and buf[1] == 0x15`.

## Invariants

- **Lossless:** `unpack(pack(A, B)) == (A, B)` for any two valid frames.
- **Quantum-preserving:** each output frame independently satisfies sync +
  version + CRC, so the existing 246-vector wire certificate is unaffected —
  SuperPack is a *container adapter*, not a codec change.
- **Smaller:** `len(SuperPack) = 32 < 34 = 2 × 17`.
- **Tamper-evident:** any single-bit change to the 32 bytes fails the joint CRC.

## Anchors

- Inner-frame anchors are unchanged: `CRC("123456789") = 0x29B1`, `CRC(0^15) = 0x4EC3`.
- SuperPack of two all-zero-core frames (`encode(0,0,0,0,b"\x00\x00\x00\x00",0)`)
  has **joint CRC = `0x5B75`** (pinned in `superpack._selftest`).

## Certification

`python3 python/MCP/superpack.py` prints `superpack selftest: CERTIFIED`, and
`pytest matrix-bridge/tests/test_bridge_wire.py` pins size, losslessness, the
golden joint CRC, tamper detection, and rejection of corrupt inputs. To promote
SuperPack into the certified multi-language core, port `pack`/`unpack` to the
other reference codecs and add SuperPack vectors alongside `golden_vectors.json`
(the inner frames reuse the existing CRC machinery, so only the container framing
is new).
