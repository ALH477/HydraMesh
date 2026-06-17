// SPDX-License-Identifier: LGPL-3.0-only
#ifndef DCF_FRAME_HPP
#define DCF_FRAME_HPP

// The DeMoD 17-byte DeModFrame wire quantum (version 1), byte-identical to the
// reference codec in python/MCP/wirelab_core.py and certified against
// Documentation/golden_vectors.json (the cross-language contract). Header-only.
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

#include <array>
#include <cstddef>
#include <cstdint>
#include <stdexcept>

namespace dcf {

constexpr std::uint8_t  SYNC = 0xD3;
constexpr std::uint8_t  VERSION = 1;
constexpr std::size_t   FRAME_SIZE = 17;
constexpr std::size_t   CRC_COVER = 15;
constexpr std::uint16_t BROADCAST = 0xFFFF;

using Bytes17 = std::array<std::uint8_t, FRAME_SIZE>;

/// CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF, no reflection, no xorout).
inline std::uint16_t crc16(const std::uint8_t* data, std::size_t len) {
    std::uint16_t crc = 0xFFFF;
    for (std::size_t k = 0; k < len; ++k) {
        crc = static_cast<std::uint16_t>(crc ^ (static_cast<std::uint16_t>(data[k]) << 8));
        for (int i = 0; i < 8; ++i) {
            crc = (crc & 0x8000) ? static_cast<std::uint16_t>((crc << 1) ^ 0x1021)
                                 : static_cast<std::uint16_t>(crc << 1);
        }
    }
    return crc;
}

struct Frame {
    std::uint8_t  version = 1;     // 4-bit
    std::uint8_t  type = 0;        // 4-bit (0=Data,1=Ack,2=Beacon,3=Ctrl)
    std::uint16_t seq = 0;
    std::uint16_t src = 0;
    std::uint16_t dst = 0;
    std::array<std::uint8_t, 4> payload{};
    std::uint32_t ts_us = 0;       // u24

    /// Serialise into exactly 17 bytes, computing and appending the CRC.
    Bytes17 encode() const {
        Bytes17 b{};
        b[0] = SYNC;
        b[1] = static_cast<std::uint8_t>(((version & 0x0F) << 4) | (type & 0x0F));
        b[2] = static_cast<std::uint8_t>(seq >> 8);  b[3] = static_cast<std::uint8_t>(seq);
        b[4] = static_cast<std::uint8_t>(src >> 8);  b[5] = static_cast<std::uint8_t>(src);
        b[6] = static_cast<std::uint8_t>(dst >> 8);  b[7] = static_cast<std::uint8_t>(dst);
        b[8] = payload[0]; b[9] = payload[1]; b[10] = payload[2]; b[11] = payload[3];
        b[12] = static_cast<std::uint8_t>((ts_us >> 16) & 0xFF);
        b[13] = static_cast<std::uint8_t>((ts_us >> 8) & 0xFF);
        b[14] = static_cast<std::uint8_t>(ts_us & 0xFF);
        const std::uint16_t crc = crc16(b.data(), CRC_COVER);
        b[15] = static_cast<std::uint8_t>(crc >> 8);
        b[16] = static_cast<std::uint8_t>(crc);
        return b;
    }
};

/// Affine validity syndrome of a 17-byte word: CRC-valid iff this returns 0.
inline std::uint16_t syndrome(const std::uint8_t* w, std::size_t len) {
    if (len != FRAME_SIZE) throw std::invalid_argument("need 17 bytes");
    const std::uint16_t stored = static_cast<std::uint16_t>((w[15] << 8) | w[16]);
    return static_cast<std::uint16_t>(crc16(w, CRC_COVER) ^ stored);
}

/// Parse a 17-byte buffer, validating sync, version nibble, and CRC.
inline Frame decode(const std::uint8_t* w, std::size_t len) {
    if (len != FRAME_SIZE) throw std::invalid_argument("length != 17");
    if (w[0] != SYNC) throw std::invalid_argument("bad sync byte");
    if ((w[1] >> 4) != VERSION) throw std::invalid_argument("bad version nibble");
    if (syndrome(w, len) != 0) throw std::invalid_argument("CRC mismatch");
    Frame f;
    f.version = static_cast<std::uint8_t>(w[1] >> 4);
    f.type = static_cast<std::uint8_t>(w[1] & 0x0F);
    f.seq = static_cast<std::uint16_t>((w[2] << 8) | w[3]);
    f.src = static_cast<std::uint16_t>((w[4] << 8) | w[5]);
    f.dst = static_cast<std::uint16_t>((w[6] << 8) | w[7]);
    f.payload = {w[8], w[9], w[10], w[11]};
    f.ts_us = (static_cast<std::uint32_t>(w[12]) << 16) |
              (static_cast<std::uint32_t>(w[13]) << 8) | w[14];
    return f;
}

/// Verify the codec against the CRC + example-frame anchors; throws on divergence.
inline void self_cert() {
    const char* s = "123456789";
    if (crc16(reinterpret_cast<const std::uint8_t*>(s), 9) != 0x29B1)
        throw std::runtime_error("dcf: CRC anchor diverged (123456789)");
    const std::uint8_t zeros[15] = {0};
    if (crc16(zeros, 15) != 0x4EC3)
        throw std::runtime_error("dcf: CRC anchor diverged (0^15)");
    Frame ex;
    ex.version = 1; ex.type = 3; ex.seq = 0x1234; ex.src = 1; ex.dst = BROADCAST;
    ex.payload = {0xDE, 0xAD, 0xBE, 0xEF}; ex.ts_us = 0xAB12CD;
    const Bytes17 got = ex.encode();
    // exampleFrame_full = d31312340001ffffdeadbeefab12cd24c0
    static const std::uint8_t want[17] = {
        0xD3, 0x13, 0x12, 0x34, 0x00, 0x01, 0xFF, 0xFF,
        0xDE, 0xAD, 0xBE, 0xEF, 0xAB, 0x12, 0xCD, 0x24, 0xC0,
    };
    for (std::size_t i = 0; i < FRAME_SIZE; ++i)
        if (got[i] != want[i]) throw std::runtime_error("dcf: exampleFrame anchor diverged");
}

namespace detail {
// Self-certify at static-init time (the header-only analogue of "self-cert on load"):
// an anchor divergence escapes as an exception during init -> std::terminate.
[[maybe_unused]] inline const bool kSelfCertified = (self_cert(), true);
}  // namespace detail

}  // namespace dcf

#endif  // DCF_FRAME_HPP
