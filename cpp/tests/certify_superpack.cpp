// SPDX-License-Identifier: LGPL-3.0-only
//
// Certifies dcf::pack/unpack (SuperPack) byte-for-byte against the cross-language
// golden vectors in Documentation/superpack_vectors.json. Dependency-free.
//   g++ -std=c++17 -I cpp/include cpp/tests/certify_superpack.cpp -o cert_sp && ./cert_sp

#include "dcf/superpack.hpp"

#include <cstdint>
#include <cstdio>
#include <fstream>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

static std::string read_file(const std::string& path) {
    std::ifstream f(path, std::ios::binary);
    if (!f) return std::string();
    std::ostringstream ss;
    ss << f.rdbuf();
    return ss.str();
}

static std::string load_vectors() {
    const char* candidates[] = {
        "Documentation/superpack_vectors.json",
        "../Documentation/superpack_vectors.json",
        "../../Documentation/superpack_vectors.json",
        "python/MCP/superpack_vectors.json",
    };
    for (const char* c : candidates) {
        std::string s = read_file(c);
        if (!s.empty()) return s;
    }
    return std::string();
}

static std::vector<std::uint8_t> from_hex(const std::string& h) {
    std::vector<std::uint8_t> out;
    for (std::size_t i = 0; i + 1 < h.size(); i += 2)
        out.push_back(static_cast<std::uint8_t>(std::stoul(h.substr(i, 2), nullptr, 16)));
    return out;
}
static std::string to_hex(const std::uint8_t* b, std::size_t n) {
    static const char* d = "0123456789abcdef";
    std::string s;
    for (std::size_t i = 0; i < n; ++i) { s += d[b[i] >> 4]; s += d[b[i] & 0xF]; }
    return s;
}

int main() {
    const std::string js = load_vectors();
    if (js.empty()) { std::fprintf(stderr, "superpack_vectors.json not found\n"); return 2; }

    const std::regex re(
        "\"a\"\\s*:\\s*\"([0-9a-fA-F]+)\"[\\s\\S]*?\"b\"\\s*:\\s*\"([0-9a-fA-F]+)\""
        "[\\s\\S]*?\"super\"\\s*:\\s*\"([0-9a-fA-F]+)\"");
    auto begin = std::sregex_iterator(js.begin(), js.end(), re);
    auto end = std::sregex_iterator();

    int n = 0, failures = 0;
    for (auto it = begin; it != end; ++it, ++n) {
        const std::smatch& m = *it;
        const auto av = from_hex(m[1].str());
        const auto bv = from_hex(m[2].str());
        const std::string sp_hex = m[3].str();
        dcf::Bytes17 a{}, b{};
        std::copy(av.begin(), av.end(), a.begin());
        std::copy(bv.begin(), bv.end(), b.begin());

        // pack matches the golden container
        const dcf::Bytes32 packed = dcf::pack(a, b);
        if (!dcf::is_superpack(packed.data(), packed.size())) {
            std::fprintf(stderr, "FAIL: case %d not recognised as SuperPack\n", n); ++failures;
        }
        if (to_hex(packed.data(), packed.size()) != sp_hex) {
            std::fprintf(stderr, "FAIL: case %d pack bytes mismatch\n", n); ++failures;
        }
        // unpack is lossless
        const auto sp = from_hex(sp_hex);
        const auto pr = dcf::unpack(sp.data(), sp.size());
        if (to_hex(pr.first.data(), 17) != m[1].str() || to_hex(pr.second.data(), 17) != m[2].str()) {
            std::fprintf(stderr, "FAIL: case %d unpack mismatch\n", n); ++failures;
        }
    }
    if (n == 0) { std::fprintf(stderr, "no cases parsed\n"); return 2; }
    std::printf("PASS: %d SuperPack pairs pack + unpack byte-identically\n", n);

    // tamper detection on case 0
    {
        std::smatch m; std::regex_search(js, m, re);
        auto sp = from_hex(m[3].str());
        for (std::size_t i = 0; i < sp.size(); ++i) {
            auto bad = sp; bad[i] ^= 0x01;
            bool rejected = false;
            try { dcf::unpack(bad.data(), bad.size()); } catch (...) { rejected = true; }
            if (!rejected) { std::fprintf(stderr, "FAIL: tamper at %zu not detected\n", i); ++failures; }
        }
    }
    // zero-core anchor
    {
        dcf::Frame zero; zero.version = 1; zero.type = 0;
        const dcf::Bytes17 zf = zero.encode();
        const dcf::Bytes32 spz = dcf::pack(zf, zf);
        const std::uint16_t joint = static_cast<std::uint16_t>((spz[30] << 8) | spz[31]);
        if (joint != 0x5B75) { std::fprintf(stderr, "FAIL: zero-core anchor 0x%04X\n", joint); ++failures; }
    }
    if (failures) { std::fprintf(stderr, "\n%d FAILURE(S)\n", failures); return 1; }
    std::printf("PASS: joint CRC tamper-evident; zero-core anchor = 0x5B75\n");
    std::printf("\nALL SUPERPACK VECTORS HOLD — C++ DCF SuperPack is cemented.\n");
    return 0;
}
