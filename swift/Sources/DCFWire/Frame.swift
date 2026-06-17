// SPDX-License-Identifier: LGPL-3.0-only

// The DeMoD 17-byte DeModFrame wire quantum (version 1), byte-identical to the
// reference codec in python/MCP/wirelab_core.py and certified against
// Documentation/golden_vectors.json (the cross-language contract).
//
// Wire layout (big-endian):
//   [0]     sync = 0xD3
//   [1]     flags: version[7:4]=1 | frame_type[3:0]
//   [2:4]   seq      u16
//   [4:6]   src      u16
//   [6:8]   dst      u16
//   [8:12]  payload  4 bytes
//   [12:15] ts_us    u24
//   [15:17] CRC-16/CCITT-FALSE over bytes [0..14]

public let SYNC: UInt8 = 0xD3
public let VERSION: UInt8 = 1
public let FRAME_SIZE: Int = 17
public let CRC_COVER: Int = 15
public let BROADCAST: UInt16 = 0xFFFF

/// CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF, no reflection, no xorout).
public func crc16(_ data: [UInt8], _ len: Int) -> UInt16 {
    var crc = 0xFFFF
    for k in 0..<len {
        crc ^= Int(data[k]) << 8
        crc &= 0xFFFF
        for _ in 0..<8 {
            if crc & 0x8000 != 0 {
                crc = ((crc << 1) ^ 0x1021) & 0xFFFF
            } else {
                crc = (crc << 1) & 0xFFFF
            }
        }
    }
    return UInt16(crc & 0xFFFF)
}

public enum FrameError: Error {
    case badLength, badSync, badVersion, badCrc
}

public struct Frame {
    public var version: UInt8 = 1     // 4-bit
    public var type: UInt8 = 0        // 4-bit (0=Data,1=Ack,2=Beacon,3=Ctrl)
    public var seq: UInt16 = 0
    public var src: UInt16 = 0
    public var dst: UInt16 = 0
    public var payload: [UInt8] = [0, 0, 0, 0]
    public var tsUs: UInt32 = 0       // u24

    public init() {}

    /// Serialise into exactly 17 bytes, computing and appending the CRC.
    public func encode() -> [UInt8] {
        var b = [UInt8](repeating: 0, count: FRAME_SIZE)
        b[0] = SYNC
        b[1] = UInt8(((Int(version) & 0x0F) << 4) | (Int(type) & 0x0F))
        b[2] = UInt8(seq >> 8); b[3] = UInt8(seq & 0x00FF)
        b[4] = UInt8(src >> 8); b[5] = UInt8(src & 0x00FF)
        b[6] = UInt8(dst >> 8); b[7] = UInt8(dst & 0x00FF)
        b[8] = payload[0]; b[9] = payload[1]; b[10] = payload[2]; b[11] = payload[3]
        b[12] = UInt8((tsUs >> 16) & 0xFF)
        b[13] = UInt8((tsUs >> 8) & 0xFF)
        b[14] = UInt8(tsUs & 0xFF)
        let crc = crc16(b, CRC_COVER)
        b[15] = UInt8(crc >> 8); b[16] = UInt8(crc & 0x00FF)
        return b
    }
}

/// Affine validity syndrome of a 17-byte word: CRC-valid iff this returns 0.
public func syndrome(_ w: [UInt8]) -> UInt16 {
    precondition(w.count == FRAME_SIZE, "need 17 bytes")
    let stored = (UInt16(w[15]) << 8) | UInt16(w[16])
    return crc16(w, CRC_COVER) ^ stored
}

/// Parse a 17-byte buffer, validating sync, version nibble, and CRC.
public func decode(_ w: [UInt8]) throws -> Frame {
    guard w.count == FRAME_SIZE else { throw FrameError.badLength }
    guard w[0] == SYNC else { throw FrameError.badSync }
    guard (w[1] >> 4) == VERSION else { throw FrameError.badVersion }
    guard syndrome(w) == 0 else { throw FrameError.badCrc }
    var f = Frame()
    f.version = w[1] >> 4
    f.type = w[1] & 0x0F
    f.seq = (UInt16(w[2]) << 8) | UInt16(w[3])
    f.src = (UInt16(w[4]) << 8) | UInt16(w[5])
    f.dst = (UInt16(w[6]) << 8) | UInt16(w[7])
    f.payload = [w[8], w[9], w[10], w[11]]
    f.tsUs = (UInt32(w[12]) << 16) | (UInt32(w[13]) << 8) | UInt32(w[14])
    return f
}

/// Verify the codec against the CRC + example-frame anchors; throws on divergence.
/// Swift libraries have no load hook, so call this once at startup; the cert test
/// invokes it.
public func selfCert() throws {
    guard crc16(Array("123456789".utf8), 9) == 0x29B1 else { throw FrameError.badCrc }
    guard crc16([UInt8](repeating: 0, count: 15), 15) == 0x4EC3 else { throw FrameError.badCrc }
    var ex = Frame()
    ex.version = 1; ex.type = 3; ex.seq = 0x1234; ex.src = 1; ex.dst = BROADCAST
    ex.payload = [0xDE, 0xAD, 0xBE, 0xEF]; ex.tsUs = 0xAB12CD
    let want: [UInt8] = [0xD3, 0x13, 0x12, 0x34, 0x00, 0x01, 0xFF, 0xFF,
                         0xDE, 0xAD, 0xBE, 0xEF, 0xAB, 0x12, 0xCD, 0x24, 0xC0]
    guard ex.encode() == want else { throw FrameError.badCrc }
}
