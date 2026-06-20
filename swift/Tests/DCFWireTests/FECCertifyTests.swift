// SPDX-License-Identifier: LGPL-3.0-only
import XCTest
import Foundation
@testable import DCFWire

/// Certifies the Swift FEC adapter byte-for-byte against the cross-language golden
/// vectors in Documentation/fec_vectors.json (encode, the multi-codeword message
/// format, and the correction law).
final class FECCertifyTests: XCTestCase {

    private func fecJSON() throws -> [String: Any] {
        var root = URL(fileURLWithPath: #filePath)
        for _ in 0..<4 { root.deleteLastPathComponent() }
        let candidates = [
            root.appendingPathComponent("Documentation/fec_vectors.json"),
            root.appendingPathComponent("python/MCP/fec_vectors.json"),
            URL(fileURLWithPath: "../Documentation/fec_vectors.json"),
            URL(fileURLWithPath: "Documentation/fec_vectors.json"),
        ]
        for c in candidates where FileManager.default.fileExists(atPath: c.path) {
            let data = try Data(contentsOf: c)
            return try JSONSerialization.jsonObject(with: data) as! [String: Any]
        }
        XCTFail("fec_vectors.json not found (run gen_fec_vectors.py)")
        return [:]
    }

    private func hex(_ b: [UInt8]) -> String { b.map { String(format: "%02x", Int($0)) }.joined() }
    private func unhex(_ s: String) -> [UInt8] {
        let chars = Array(s)
        var out = [UInt8](); out.reserveCapacity(chars.count / 2)
        var i = 0
        while i + 1 < chars.count { out.append(UInt8(String(chars[i...i + 1]), radix: 16)!); i += 2 }
        return out
    }

    func testEncodeMatchesGolden() throws {
        let root = try fecJSON()
        let np = root["nparity"] as! Int
        for (i, c) in (root["cases"] as! [[String: Any]]).enumerated() {
            XCTAssertEqual(hex(rsEncode(unhex(c["msg"] as! String), np)),
                           (c["code"] as! String).lowercased(), "encode \(i)")
        }
    }

    func testDecodeCorrectsGolden() throws {
        let root = try fecJSON()
        let np = root["nparity"] as! Int
        for (i, c) in (root["correct"] as! [[String: Any]]).enumerated() {
            let r = try rsDecode(unhex(c["corrupt"] as! String), np, 17)
            XCTAssertEqual(hex(r.msg), (c["msg"] as! String).lowercased(), "decode \(i)")
            XCTAssertEqual(r.corrected, c["nerr"] as! Int, "count \(i)")
        }
    }

    func testMultiCodewordMessages() throws {
        let root = try fecJSON()
        let np = root["nparity"] as! Int
        for (i, c) in (root["messages"] as! [[String: Any]]).enumerated() {
            let msg = unhex(c["msg"] as! String)
            let blob = encodeMessage(msg, np)
            XCTAssertEqual(hex(blob), (c["blob"] as! String).lowercased(), "message \(i) blob")
            XCTAssertEqual(try decodeMessage(blob).msg, msg, "message \(i) round-trip")
        }
    }

    func testInterleavedBurst() throws {
        let root = try fecJSON()
        let b = root["message_burst"] as! [String: Any]
        let got = try decodeMessage(unhex(b["corrupt"] as! String)).msg
        XCTAssertEqual(hex(got), (b["msg"] as! String).lowercased())
    }
}
