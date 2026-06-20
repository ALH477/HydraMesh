// SPDX-License-Identifier: LGPL-3.0-only
//
// Certifies dcf::fec (Reed-Solomon) byte-for-byte against the cross-language golden
// vectors in Documentation/fec_vectors.json. Dependency-free.
//   g++ -std=c++17 -I cpp/include cpp/tests/certify_fec.cpp -o cert_fec && ./cert_fec

#include "dcf/fec.hpp"

#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

static std::string read_file(const std::string& p) {
    std::ifstream f(p, std::ios::binary);
    if (!f) return {};
    std::ostringstream ss;
    ss << f.rdbuf();
    return ss.str();
}
static std::string load() {
    for (const char* c : {"Documentation/fec_vectors.json", "../Documentation/fec_vectors.json",
                          "../../Documentation/fec_vectors.json", "python/MCP/fec_vectors.json"}) {
        std::string s = read_file(c);
        if (!s.empty()) return s;
    }
    return {};
}
static dcf::fec::Bytes from_hex(const std::string& h) {
    dcf::fec::Bytes out;
    for (std::size_t i = 0; i + 1 < h.size(); i += 2)
        out.push_back(static_cast<std::uint8_t>(std::stoi(h.substr(i, 2), nullptr, 16)));
    return out;
}
// All hex values for "key":"..." within [a,b) of j, in order.
static std::vector<std::string> field(const std::string& j, std::size_t a, std::size_t b,
                                      const std::string& key) {
    std::vector<std::string> out;
    std::string pat = "\"" + key + "\": \"";
    std::size_t p = a;
    while ((p = j.find(pat, p)) != std::string::npos && p < b) {
        std::size_t s = p + pat.size();
        std::size_t e = j.find('"', s);
        out.push_back(j.substr(s, e - s));
        p = e;
    }
    return out;
}
static std::size_t at(const std::string& j, const std::string& key) { return j.find("\"" + key + "\""); }

static int fails = 0;
static void check(bool c, const char* m) { std::printf("  %s  %s\n", c ? "PASS" : "FAIL", m); if (!c) fails++; }

int main() {
    std::string j = load();
    if (j.empty()) { std::printf("fec_vectors.json not found\n"); return 1; }
    std::size_t cCases = at(j, "cases"), cCorr = at(j, "correct"), cMsgs = at(j, "messages"),
                cBurst = at(j, "message_burst");

    // 1. systematic encode byte-identical
    auto msgs = field(j, cCases, cCorr, "msg");
    auto codes = field(j, cCases, cCorr, "code");
    bool ok = msgs.size() == codes.size() && !msgs.empty();
    for (std::size_t i = 0; ok && i < msgs.size(); i++)
        if (dcf::fec::rs_encode(from_hex(msgs[i]), 16) != from_hex(codes[i])) ok = false;
    check(ok, "RS encode vectors byte-identical");

    // 2. decode corrects the pinned corrupted codewords
    auto corrupt = field(j, cCorr, cMsgs, "corrupt");
    auto cmsgs = field(j, cCorr, cMsgs, "msg");
    ok = corrupt.size() == cmsgs.size() && !corrupt.empty();
    for (std::size_t i = 0; ok && i < corrupt.size(); i++) {
        auto r = dcf::fec::rs_decode(from_hex(corrupt[i]), 16, 17);
        if (r.first != from_hex(cmsgs[i])) ok = false;
    }
    check(ok, "corrupted codewords corrected to the original frame");

    // 3. multi-codeword messages: golden blob byte-identical + round-trip
    auto mmsg = field(j, cMsgs, cBurst, "msg");
    auto mblob = field(j, cMsgs, cBurst, "blob");
    ok = mmsg.size() == mblob.size() && !mmsg.empty();
    for (std::size_t i = 0; ok && i < mmsg.size(); i++) {
        auto msg = from_hex(mmsg[i]);
        auto blob = dcf::fec::encode_message(msg, 16);
        if (blob != from_hex(mblob[i])) ok = false;
        if (dcf::fec::decode_message(blob).first != msg) ok = false;
    }
    check(ok, "multi-codeword messages byte-identical + round-trip");

    // 4. interleaved burst across codewords corrected
    auto bmsg = field(j, cBurst, j.size(), "msg");
    auto bcor = field(j, cBurst, j.size(), "corrupt");
    ok = false;
    if (!bmsg.empty() && !bcor.empty()) {
        try {
            ok = dcf::fec::decode_message(from_hex(bcor[0])).first == from_hex(bmsg[0]);
        } catch (...) { ok = false; }
    }
    check(ok, "interleaved burst across codewords corrected");

    if (fails == 0) std::printf("ALL FEC VECTORS MATCH\n");
    return fails ? 1 : 0;
}
