// SPDX-License-Identifier: LGPL-3.0-only
#ifndef DCF_SUPERPACK_HPP
#define DCF_SUPERPACK_HPP

// DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
// DeModFrame quanta under a single joint CRC-16. Header-only.
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

#include <array>
#include <cstddef>
#include <cstdint>
#include <stdexcept>

#include "dcf/frame.hpp"

namespace dcf {

constexpr std::uint8_t  SUPER_TYPE = 0x05;
constexpr std::size_t   SUPER_LEN = 32;
constexpr std::size_t   SUPER_CORE_LEN = 14;  // a frame's bytes [1..14]
constexpr std::uint8_t  SUPER_SFLAGS = static_cast<std::uint8_t>((VERSION << 4) | SUPER_TYPE);

using Bytes32 = std::array<std::uint8_t, SUPER_LEN>;

namespace detail_sp {
// The 14 reconstructable bytes of a frame, validating sync, version, inner CRC.
inline std::array<std::uint8_t, SUPER_CORE_LEN> frame_core(const Bytes17& f) {
    if (f[0] != SYNC) throw std::invalid_argument("superpack: bad sync byte");
    if ((f[1] >> 4) != VERSION) throw std::invalid_argument("superpack: bad version nibble");
    const std::uint16_t stored = static_cast<std::uint16_t>((f[15] << 8) | f[16]);
    if (crc16(f.data(), CRC_COVER) != stored) throw std::invalid_argument("superpack: inner CRC");
    std::array<std::uint8_t, SUPER_CORE_LEN> core{};
    for (std::size_t i = 0; i < SUPER_CORE_LEN; ++i) core[i] = f[i + 1];
    return core;
}

// Rebuild a full 17-byte frame from its 14-byte core (sync + recomputed CRC).
inline Bytes17 rebuild(const std::uint8_t* core) {
    Bytes17 out{};
    out[0] = SYNC;
    for (std::size_t i = 0; i < SUPER_CORE_LEN; ++i) out[i + 1] = core[i];
    const std::uint16_t crc = crc16(out.data(), CRC_COVER);
    out[15] = static_cast<std::uint8_t>(crc >> 8);
    out[16] = static_cast<std::uint8_t>(crc);
    return out;
}
}  // namespace detail_sp

/// Combine two valid 17-byte frames into one 32-byte SuperPack.
inline Bytes32 pack(const Bytes17& a, const Bytes17& b) {
    const auto core_a = detail_sp::frame_core(a);
    const auto core_b = detail_sp::frame_core(b);
    Bytes32 out{};
    out[0] = SYNC;
    out[1] = SUPER_SFLAGS;
    for (std::size_t i = 0; i < SUPER_CORE_LEN; ++i) {
        out[2 + i] = core_a[i];
        out[2 + SUPER_CORE_LEN + i] = core_b[i];
    }
    const std::uint16_t crc = crc16(out.data(), 30);
    out[30] = static_cast<std::uint8_t>(crc >> 8);
    out[31] = static_cast<std::uint8_t>(crc);
    return out;
}

/// True iff buf looks like a SuperPack (length + sync + version/type tag).
inline bool is_superpack(const std::uint8_t* buf, std::size_t len) {
    return len == SUPER_LEN && buf[0] == SYNC && buf[1] == SUPER_SFLAGS;
}

/// Split a 32-byte SuperPack into two bit-exact 17-byte frames. Throws on failure.
inline std::pair<Bytes17, Bytes17> unpack(const std::uint8_t* in, std::size_t len) {
    if (len != SUPER_LEN) throw std::invalid_argument("superpack: length != 32");
    if (in[0] != SYNC) throw std::invalid_argument("superpack: bad sync byte");
    if ((in[1] >> 4) != VERSION) throw std::invalid_argument("superpack: bad version nibble");
    if ((in[1] & 0x0F) != SUPER_TYPE) throw std::invalid_argument("superpack: not a SuperPack");
    const std::uint16_t stored = static_cast<std::uint16_t>((in[30] << 8) | in[31]);
    if (crc16(in, 30) != stored) throw std::invalid_argument("superpack: joint CRC mismatch");
    Bytes17 a = detail_sp::rebuild(in + 2);
    Bytes17 b = detail_sp::rebuild(in + 2 + SUPER_CORE_LEN);
    // Belt and braces: the rebuilt frames must themselves validate.
    decode(a.data(), a.size());
    decode(b.data(), b.size());
    return {a, b};
}

}  // namespace dcf

#endif  // DCF_SUPERPACK_HPP
