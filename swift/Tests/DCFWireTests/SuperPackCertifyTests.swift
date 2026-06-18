// SPDX-License-Identifier: LGPL-3.0-only
import XCTest
import Foundation
@testable import DCFWire

/// Certifies the Swift SuperPack container byte-for-byte against the cross-language
/// golden vectors in Documentation/superpack_vectors.json.
final class SuperPackCertifyTests: XCTestCase {

    private func superJSON() throws -> [String: Any] {
        var root = URL(fileURLWithPath: #filePath)
        for _ in 0..<4 { root.deleteLastPathComponent() }
        let candidates = [
            root.appendingPathComponent("Documentation/superpack_vectors.json"),
            root.appendingPathComponent("python/MCP/superpack_vectors.json"),
            URL(fileURLWithPath: "../Documentation/superpack_vectors.json"),
            URL(fileURLWithPath: "Documentation/superpack_vectors.json"),
        ]
        for c in candidates where FileManager.default.fileExists(atPath: c.path) {
            let data = try Data(contentsOf: c)
            return try JSONSerialization.jsonObject(with: data) as! [String: Any]
        }
        XCTFail("superpack_vectors.json not found (run gen_superpack_vectors.py)")
        return [:]
    }

    private func hex(_ b: [UInt8]) -> String {
        b.map { String(format: "%02x", Int($0)) }.joined()
    }
    private func unhex(_ s: String) -> [UInt8] {
        let chars = Array(s)
        var out = [UInt8](); out.reserveCapacity(chars.count / 2)
        var i = 0
        while i + 1 < chars.count { out.append(UInt8(String(chars[i...i + 1]), radix: 16)!); i += 2 }
        return out
    }

    private func cases() throws -> [[String: Any]] {
        (try superJSON()["cases"] as? [[String: Any]]) ?? []
    }

    func testPackMatchesGolden() throws {
        for (i, c) in try cases().enumerated() {
            let a = unhex(c["a"] as! String)
            let b = unhex(c["b"] as! String)
            let packed = try packSuper(a, b)
            XCTAssertEqual(packed.count, SUPER_LEN, "case \(i) length")
            XCTAssertTrue(isSuperPack(packed), "case \(i) recognised")
            XCTAssertEqual(hex(packed), (c["super"] as! String).lowercased(), "case \(i) pack bytes")
        }
    }

    func testUnpackLossless() throws {
        for (i, c) in try cases().enumerated() {
            let (a, b) = try unpackSuper(unhex(c["super"] as! String))
            XCTAssertEqual(hex(a), (c["a"] as! String).lowercased(), "case \(i) frame A")
            XCTAssertEqual(hex(b), (c["b"] as! String).lowercased(), "case \(i) frame B")
        }
    }

    func testTamperEvidentAndAnchor() throws {
        let sp = unhex((try cases())[0]["super"] as! String)
        for i in 0..<sp.count {
            var bad = sp
            bad[i] ^= 0x01
            XCTAssertThrowsError(try unpackSuper(bad), "tamper at byte \(i) not detected")
        }
        var zero = Frame()
        zero.version = 1
        zero.type = 0
        let zf = zero.encode()
        let spz = try packSuper(zf, zf)
        let joint = (UInt16(spz[30]) << 8) | UInt16(spz[31])
        XCTAssertEqual(joint, 0x5B75, "zero-core joint CRC anchor")
    }
}
