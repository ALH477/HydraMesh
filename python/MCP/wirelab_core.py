# SPDX-License-Identifier: LGPL-3.0-only
"""DCF WireLab core — reference codec for the 17-byte DeModFrame (wire quantum).
Layout (big-endian):
  [0]    sync = 0xD3
  [1]    flags: version[7:4]=1 | frame_type[3:0]
  [2-3]  seq    u16
  [4-5]  src    u16
  [6-7]  dst    u16
  [8-11] payload 4 bytes
  [12-14] ts_us  u24
  [15-16] CRC-16/CCITT-FALSE over bytes [0..14]
"""
SYNC, VERSION, FRAME_LEN = 0xD3, 1, 17
FRAME_TYPES = {0: "FData", 1: "FAck", 2: "FBeacon", 3: "FCtrl"}

def crc16_ccitt(data: bytes) -> int:
    crc = 0xFFFF
    for b in data:
        crc ^= b << 8
        for _ in range(8):
            crc = ((crc << 1) ^ 0x1021) & 0xFFFF if crc & 0x8000 else (crc << 1) & 0xFFFF
    return crc

def encode(frame_type: int, seq: int, src: int, dst: int, payload: bytes, ts_us: int) -> bytes:
    if not (0 <= frame_type < 16): raise ValueError("frame_type must be a nibble")
    if len(payload) != 4: raise ValueError("payload must be exactly 4 bytes")
    if not (0 <= seq < 1 << 16 and 0 <= src < 1 << 16 and 0 <= dst < 1 << 16):
        raise ValueError("seq/src/dst must be u16")
    b = bytearray(FRAME_LEN)
    b[0] = SYNC
    b[1] = (VERSION << 4) | frame_type
    b[2:4] = seq.to_bytes(2, "big"); b[4:6] = src.to_bytes(2, "big"); b[6:8] = dst.to_bytes(2, "big")
    b[8:12] = payload
    b[12:15] = (ts_us & 0xFFFFFF).to_bytes(3, "big")
    b[15:17] = crc16_ccitt(bytes(b[:15])).to_bytes(2, "big")
    return bytes(b)

def syndrome(word: bytes) -> int:
    """Affine validity map L: B^17 -> B^16; word valid w.r.t. CRC iff syndrome==0."""
    if len(word) != FRAME_LEN: raise ValueError("need 17 bytes")
    return crc16_ccitt(word[:15]) ^ int.from_bytes(word[15:17], "big")

def decode(word: bytes):
    """Returns (fields dict) or raises ValueError with reason."""
    if len(word) != FRAME_LEN: raise ValueError(f"length {len(word)} != 17")
    if word[0] != SYNC: raise ValueError("bad sync byte")
    if word[1] >> 4 != VERSION: raise ValueError("bad version nibble")
    if syndrome(word) != 0: raise ValueError("CRC mismatch")
    return {
        "frame_type": word[1] & 0x0F,
        "frame_type_name": FRAME_TYPES.get(word[1] & 0x0F, f"0x{word[1] & 0x0F:X}"),
        "seq": int.from_bytes(word[2:4], "big"),
        "src": int.from_bytes(word[4:6], "big"),
        "dst": int.from_bytes(word[6:8], "big"),
        "payload": word[8:12].hex(),
        "ts_us": int.from_bytes(word[12:15], "big"),
        "crc": int.from_bytes(word[15:17], "big"),
    }
