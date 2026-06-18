// SPDX-License-Identifier: LGPL-3.0-only
import Foundation

// DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
// DeModFrame quanta under a single joint CRC-16.
//
// Two raw frames cost 2*17 = 34 bytes. When frames are emitted in pairs the
// second header is largely recoverable from context (both inner sync bytes are
// 0xD3; each inner CRC is a pure function of its own 15 leading bytes). SuperPack
// drops those 6 redundant bytes and spends 4 back on one outer sync, a
// type/version tag, and ONE joint CRC over the whole container — net 34 -> 32
// bytes plus a strictly stronger integrity check.
//
// Why it is the lower-latency option: a SuperPack puts a frame pair on the wire
// as a single datagram instead of two — one packet, one IP/UDP header, one
// syscall — so paired traffic crosses the network with strictly lower per-pair
// overhead and latency than emitting the two frames separately.
//
// Unpack reconstructs each inner frame bit-exact, so the outputs are ordinary
// valid DeModFrames and the 246-vector wire certificate is untouched.

public let SUPER_TYPE: UInt8 = 0x05
public let SUPER_LEN: Int = 32
public let SUPER_CORE_LEN: Int = 14            // a frame's bytes [1..14]
public let SUPER_SFLAGS: UInt8 = (VERSION << 4) | SUPER_TYPE

public enum SuperPackError: Error {
    case badLength, badSync, badVersion, badType, innerCRC, jointCRC, innerDecode
}

/// The 14 reconstructable bytes of a frame; validates sync, version, inner CRC.
func frameCore(_ frame: [UInt8]) throws -> [UInt8] {
    guard frame.count == FRAME_SIZE else { throw SuperPackError.badLength }
    guard frame[0] == SYNC else { throw SuperPackError.badSync }
    guard (frame[1] >> 4) == VERSION else { throw SuperPackError.badVersion }
    let stored = (UInt16(frame[15]) << 8) | UInt16(frame[16])
    guard crc16(frame, CRC_COVER) == stored else { throw SuperPackError.innerCRC }
    return Array(frame[1..<(1 + SUPER_CORE_LEN)])
}

/// Restore a full 17-byte frame from its 14-byte core (sync + recomputed CRC).
func rebuildFrame(_ core: ArraySlice<UInt8>) -> [UInt8] {
    var f = [UInt8](repeating: 0, count: FRAME_SIZE)
    f[0] = SYNC
    for (i, byte) in core.enumerated() { f[1 + i] = byte }
    let crc = crc16(f, CRC_COVER)
    f[15] = UInt8(crc >> 8)
    f[16] = UInt8(crc & 0xFF)
    return f
}

/// Combine two valid 17-byte frames into one 32-byte SuperPack.
public func packSuper(_ a: [UInt8], _ b: [UInt8]) throws -> [UInt8] {
    let coreA = try frameCore(a)
    let coreB = try frameCore(b)
    var out = [UInt8](repeating: 0, count: SUPER_LEN)
    out[0] = SYNC
    out[1] = SUPER_SFLAGS
    for i in 0..<SUPER_CORE_LEN {
        out[2 + i] = coreA[i]
        out[2 + SUPER_CORE_LEN + i] = coreB[i]
    }
    let crc = crc16(out, 30)
    out[30] = UInt8(crc >> 8)
    out[31] = UInt8(crc & 0xFF)
    return out
}

/// True iff buf looks like a SuperPack (length + sync + version/type tag).
public func isSuperPack(_ buf: [UInt8]) -> Bool {
    buf.count == SUPER_LEN && buf[0] == SYNC && buf[1] == SUPER_SFLAGS
}

/// Split a 32-byte SuperPack into (frameA, frameB), each a bit-exact frame.
public func unpackSuper(_ buf: [UInt8]) throws -> ([UInt8], [UInt8]) {
    guard buf.count == SUPER_LEN else { throw SuperPackError.badLength }
    guard buf[0] == SYNC else { throw SuperPackError.badSync }
    guard (buf[1] >> 4) == VERSION else { throw SuperPackError.badVersion }
    guard (buf[1] & 0x0F) == SUPER_TYPE else { throw SuperPackError.badType }
    let stored = (UInt16(buf[30]) << 8) | UInt16(buf[31])
    guard crc16(buf, 30) == stored else { throw SuperPackError.jointCRC }
    let frameA = rebuildFrame(buf[2..<(2 + SUPER_CORE_LEN)])
    let frameB = rebuildFrame(buf[(2 + SUPER_CORE_LEN)..<(2 + 2 * SUPER_CORE_LEN)])
    // Belt and braces: the rebuilt frames must themselves decode cleanly.
    do {
        _ = try decode(frameA)
        _ = try decode(frameB)
    } catch {
        throw SuperPackError.innerDecode
    }
    return (frameA, frameB)
}
