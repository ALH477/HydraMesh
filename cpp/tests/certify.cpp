// SPDX-License-Identifier: LGPL-3.0-only
//
// Certifies dcf::Frame byte-for-byte against the cross-language golden vectors in
// Documentation/golden_vectors.json. Dependency-free (std::regex extraction).
//   g++ -std=c++17 -I cpp/include cpp/tests/certify.cpp -o cert && ./cert
//   (or via CMake: cmake . && make && ctest)

#include "dcf/frame.hpp"

#include <algorithm>
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

static std::string load_golden() {
    const char* candidates[] = {
        "Documentation/golden_vectors.json",
        "../Documentation/golden_vectors.json",
        "../../Documentation/golden_vectors.json",
        "python/MCP/golden_vectors.json",
    };
    for (const char* c : candidates) {
        std::string s = read_file(c);
        if (!s.empty()) return s;
    }
    return std::string();
}

static std::vector<std::uint8_t> unhex(const std::string& s) {
    std::vector<std::uint8_t> out(s.size() / 2);
    for (std::size_t i = 0; i < out.size(); ++i)
        out[i] = static_cast<std::uint8_t>(std::stoul(s.substr(2 * i, 2), nullptr, 16));
    return out;
}

int main(int argc, char** argv) {
    const std::string json = (argc > 1) ? read_file(argv[1]) : load_golden();
    if (json.empty()) {
        std::fprintf(stderr, "golden_vectors.json not found\n");
        return 2;
    }

    int fails = 0;
    auto check = [&](bool cond, const char* label) {
        std::printf("  %s  %s\n", cond ? "PASS" : "FAIL", label);
        if (!cond) ++fails;
    };

    // 1. CRC anchors
    check(dcf::crc16(reinterpret_cast<const std::uint8_t*>("123456789"), 9) == 0x29B1,
          "CRC(\"123456789\") = 0x29B1");
    {
        const std::uint8_t z[15] = {0};
        check(dcf::crc16(z, 15) == 0x4EC3, "CRC(0^15) = 0x4EC3");
    }

    // 2. example frame anchor
    {
        std::smatch m;
        std::string exHex;
        if (std::regex_search(json, m, std::regex("\"exampleFrame_full\"\\s*:\\s*\"([0-9a-fA-F]+)\"")))
            exHex = m[1].str();
        dcf::Frame ex;
        ex.version = 1; ex.type = 3; ex.seq = 0x1234; ex.src = 1; ex.dst = dcf::BROADCAST;
        ex.payload = {0xDE, 0xAD, 0xBE, 0xEF}; ex.ts_us = 0xAB12CD;
        const dcf::Bytes17 b = ex.encode();
        char buf[2 * dcf::FRAME_SIZE + 1];
        for (std::size_t i = 0; i < dcf::FRAME_SIZE; ++i)
            std::snprintf(buf + 2 * i, 3, "%02x", b[i]);
        check(std::string(buf) == exHex, "exampleFrame_full matches");
    }

    // 3. encode_basis: raw-CRC-valid + (known types) decode/roundtrip
    {
        const std::regex re("\"frame\"\\s*:\\s*\"([0-9a-fA-F]+)\"");
        int count = 0, encFails = 0;
        for (auto it = std::sregex_iterator(json.begin(), json.end(), re);
             it != std::sregex_iterator(); ++it) {
            const std::vector<std::uint8_t> raw = unhex((*it)[1].str());
            const int idx = count++;
            const bool valid = raw.size() == dcf::FRAME_SIZE && raw[0] == dcf::SYNC &&
                dcf::crc16(raw.data(), dcf::CRC_COVER) ==
                    static_cast<std::uint16_t>((raw[15] << 8) | raw[16]);
            if (!valid) {
                ++encFails;
                std::printf("  FAIL  encode_basis[%d]: raw CRC invalid\n", idx);
                continue;
            }
            if ((raw[1] & 0x0F) <= 3) {
                try {
                    const dcf::Frame f = dcf::decode(raw.data(), raw.size());
                    const dcf::Bytes17 re2 = f.encode();
                    if (!std::equal(re2.begin(), re2.end(), raw.begin())) {
                        ++encFails;
                        std::printf("  FAIL  encode_basis[%d]: roundtrip mismatch\n", idx);
                    }
                } catch (const std::exception& e) {
                    ++encFails;
                    std::printf("  FAIL  encode_basis[%d]: %s\n", idx, e.what());
                }
            }
        }
        check(count > 0, "encode_basis vectors present");
        fails += encFails;
        if (encFails == 0)
            std::printf("  PASS  %d encode_basis vectors (decode + roundtrip)\n", count);
    }

    // 4. syndrome_basis: reproduce each basis word and check its syndrome
    {
        const std::size_t pos = json.find("\"syndrome_basis\"");
        const std::string section = (pos == std::string::npos) ? json : json.substr(pos);
        const std::regex objre("\\{([^}]*)\\}");
        const std::regex synre("\"syndrome\"\\s*:\\s*([0-9]+)");
        const std::regex bitre("\"bit\"\\s*:\\s*([0-9]+)");
        int count = 0, synFails = 0;
        for (auto it = std::sregex_iterator(section.begin(), section.end(), objre);
             it != std::sregex_iterator(); ++it) {
            const std::string obj = (*it)[1].str();
            std::smatch sm;
            if (!std::regex_search(obj, sm, synre)) continue;
            const std::uint16_t expected = static_cast<std::uint16_t>(std::stoul(sm[1].str()));
            std::uint8_t word[dcf::FRAME_SIZE] = {0};
            std::smatch bm;
            if (std::regex_search(obj, bm, bitre)) {
                const int bit = std::stoi(bm[1].str());
                word[bit / 8] = static_cast<std::uint8_t>(1 << (7 - bit % 8));
            }
            const std::uint16_t got = dcf::syndrome(word, dcf::FRAME_SIZE);
            if (got != expected) {
                ++synFails;
                std::printf("  FAIL  syndrome_basis[%d]: got 0x%04X want 0x%04X\n", count, got, expected);
            }
            ++count;
        }
        check(count > 0, "syndrome_basis vectors present");
        fails += synFails;
        if (synFails == 0)
            std::printf("  PASS  %d syndrome_basis vectors\n", count);
    }

    std::printf("\n");
    if (fails == 0) {
        std::printf("ALL CHECKS PASSED — C++ codec cemented.\n");
        return 0;
    }
    std::printf("%d certification check(s) FAILED\n", fails);
    return 1;
}
