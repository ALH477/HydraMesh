// SPDX-License-Identifier: LGPL-3.0-only
import XCTest
import Foundation
@testable import DCFWire

/// Certifies the Swift DeModFrame codec byte-for-byte against the cross-language
/// golden vectors in Documentation/golden_vectors.json.
final class FrameCertifyTests: XCTestCase {

    private func goldenJSON() throws -> [String: Any] {
        // <repo>/swift/Tests/DCFWireTests/FrameCertifyTests.swift -> <repo>
        var root = URL(fileURLWithPath: #filePath)
        for _ in 0..<4 { root.deleteLastPathComponent() }
        let candidates = [
            root.appendingPathComponent("Documentation/golden_vectors.json"),
            root.appendingPathComponent("python/MCP/golden_vectors.json"),
            URL(fileURLWithPath: "../Documentation/golden_vectors.json"),
            URL(fileURLWithPath: "Documentation/golden_vectors.json"),
        ]
        for c in candidates where FileManager.default.fileExists(atPath: c.path) {
            let data = try Data(contentsOf: c)
            return try JSONSerialization.jsonObject(with: data) as! [String: Any]
        }
        XCTFail("golden_vectors.json not found")
        return [:]
    }

    private func hex(_ b: [UInt8]) -> String {
        b.map { String(format: "%02x", Int($0)) }.joined()
    }

    private func unhex(_ s: String) -> [UInt8] {
        let chars = Array(s)
        var out = [UInt8]()
        out.reserveCapacity(chars.count / 2)
        var i = 0
        while i + 1 < chars.count {
            out.append(UInt8(String(chars[i...i + 1]), radix: 16)!)
            i += 2
        }
        return out
    }

    private func rawValid(_ b: [UInt8]) -> Bool {
        guard b.count == FRAME_SIZE, b[0] == SYNC else { return false }
        let stored = (UInt16(b[15]) << 8) | UInt16(b[16])
        return crc16(b, CRC_COVER) == stored
    }

    func testSelfCert() throws {
        try selfCert()
    }

    func testCRCAnchors() {
        XCTAssertEqual(crc16(Array("123456789".utf8), 9), 0x29B1)
        XCTAssertEqual(crc16([UInt8](repeating: 0, count: 15), 15), 0x4EC3)
    }

    func testExampleFrame() throws {
        let obj = try goldenJSON()
        let anchors = obj["anchors"] as! [String: Any]
        let want = (anchors["exampleFrame_full"] as! String).lowercased()
        var ex = Frame()
        ex.version = 1; ex.type = 3; ex.seq = 0x1234; ex.src = 1; ex.dst = BROADCAST
        ex.payload = [0xDE, 0xAD, 0xBE, 0xEF]; ex.tsUs = 0xAB12CD
        XCTAssertEqual(hex(ex.encode()), want)
    }

    func testEncodeBasis() throws {
        let obj = try goldenJSON()
        let basis = obj["encode_basis"] as! [[String: Any]]
        XCTAssertFalse(basis.isEmpty)
        for (i, v) in basis.enumerated() {
            let raw = unhex(v["frame"] as! String)
            XCTAssertTrue(rawValid(raw), "encode_basis[\(i)] raw CRC invalid")
            if Int(raw[1] & 0x0F) <= 3 {
                let f = try decode(raw)
                XCTAssertEqual(f.encode(), raw, "encode_basis[\(i)] roundtrip")
            }
        }
    }

    func testSyndromeBasis() throws {
        let obj = try goldenJSON()
        let basis = obj["syndrome_basis"] as! [[String: Any]]
        XCTAssertFalse(basis.isEmpty)
        for (i, v) in basis.enumerated() {
            var word = [UInt8](repeating: 0, count: FRAME_SIZE)
            if let bit = v["bit"] as? Int {
                word[bit / 8] = UInt8(1 << (7 - bit % 8))
            } else if let w = v["word"] as? String {
                word = unhex(w)
            }
            let expected = UInt16(v["syndrome"] as! Int)
            XCTAssertEqual(syndrome(word), expected, "syndrome_basis[\(i)]")
        }
    }
}
