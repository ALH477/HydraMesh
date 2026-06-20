// SPDX-License-Identifier: LGPL-3.0-only
#ifndef DCF_FEC_HPP
#define DCF_FEC_HPP

// DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
// byte-identical to python/MCP/feclab_core.py, codec/demod_fec.h, codec/src/fec.rs,
// go/dcf/fec.go, and JS/nodejs/src/fec.js (pinned by Documentation/fec_vectors.json).
// FEC wraps a 17-byte DeModFrame so a lossy medium (RF/SDR, acoustic) can CORRECT it,
// not just detect damage. Header-only, dependency-free.
//
// Field GF(2^8), prim 0x11D, generator a=2, fcr=0. Systematic: codeword =
// message ++ parity. Default 2t=16 parity -> corrects 8 byte-errors. The
// multi-codeword message layer chunks + interleaves any-length payloads behind a
// self-protecting header.

#include <cstdint>
#include <stdexcept>
#include <utility>
#include <vector>

namespace dcf {
namespace fec {

using Bytes = std::vector<std::uint8_t>;

constexpr int kPrim = 0x11D;
constexpr int kGen = 2;
constexpr int kFcr = 0;
constexpr int kDefaultParity = 16;
constexpr int kHdrParity = 16;
constexpr std::size_t kHdrLen = 5 + kHdrParity; // 21

namespace detail {

struct Gf {
    std::uint8_t exp[512];
    std::uint8_t log[256];
};
inline const Gf& gf() {
    static const Gf g = [] {
        Gf t{};
        int x = 1;
        for (int i = 0; i < 255; i++) {
            t.exp[i] = static_cast<std::uint8_t>(x);
            t.log[x] = static_cast<std::uint8_t>(i);
            x <<= 1;
            if (x & 0x100) x ^= kPrim;
        }
        for (int i = 255; i < 512; i++) t.exp[i] = t.exp[i - 255];
        return t;
    }();
    return g;
}
inline std::uint8_t mul(std::uint8_t a, std::uint8_t b) {
    if (a == 0 || b == 0) return 0;
    const Gf& g = gf();
    return g.exp[g.log[a] + g.log[b]];
}
inline std::uint8_t div(std::uint8_t a, std::uint8_t b) {
    if (a == 0) return 0;
    const Gf& g = gf();
    return g.exp[(g.log[a] + 255 - g.log[b]) % 255];
}
inline std::uint8_t pow(std::uint8_t a, int p) {
    const Gf& g = gf();
    return g.exp[(((g.log[a] * p) % 255) + 255) % 255];
}
inline std::uint8_t inv(std::uint8_t a) {
    const Gf& g = gf();
    return g.exp[255 - g.log[a]];
}

inline Bytes pmul(const Bytes& p, const Bytes& q) {
    Bytes r(p.size() + q.size() - 1, 0);
    for (std::size_t j = 0; j < q.size(); j++)
        for (std::size_t i = 0; i < p.size(); i++) r[i + j] ^= mul(p[i], q[j]);
    return r;
}
inline Bytes padd(const Bytes& p, const Bytes& q) {
    std::size_t n = p.size() > q.size() ? p.size() : q.size();
    Bytes r(n, 0);
    for (std::size_t i = 0; i < p.size(); i++) r[i + n - p.size()] = p[i];
    for (std::size_t i = 0; i < q.size(); i++) r[i + n - q.size()] ^= q[i];
    return r;
}
inline Bytes pscale(const Bytes& p, std::uint8_t s) {
    Bytes r(p.size());
    for (std::size_t i = 0; i < p.size(); i++) r[i] = mul(p[i], s);
    return r;
}
inline std::uint8_t peval(const Bytes& p, std::uint8_t x) {
    std::uint8_t y = p[0];
    for (std::size_t i = 1; i < p.size(); i++) y = mul(y, x) ^ p[i];
    return y;
}

inline Bytes generator(int nparity) {
    Bytes g{1};
    for (int i = 0; i < nparity; i++) g = pmul(g, {1, pow(kGen, kFcr + i)});
    return g;
}

inline Bytes syndromes(const Bytes& cw, int nparity) {
    Bytes s(static_cast<std::size_t>(nparity) + 1, 0); // leading 0
    for (int i = 0; i < nparity; i++) s[i + 1] = peval(cw, pow(kGen, kFcr + i));
    return s;
}
inline Bytes errLocator(const Bytes& synd, int nparity) {
    Bytes errLoc{1}, oldLoc{1};
    for (int i = 0; i < nparity; i++) {
        std::uint8_t delta = synd[i + 1];
        for (std::size_t j = 1; j < errLoc.size(); j++)
            delta ^= mul(errLoc[errLoc.size() - 1 - j], synd[i + 1 - j]);
        oldLoc.push_back(0);
        if (delta != 0) {
            if (oldLoc.size() > errLoc.size()) {
                Bytes newLoc = pscale(oldLoc, delta);
                oldLoc = pscale(errLoc, inv(delta));
                errLoc = newLoc;
            }
            errLoc = padd(errLoc, pscale(oldLoc, delta));
        }
    }
    std::size_t s = 0;
    while (s < errLoc.size() && errLoc[s] == 0) s++;
    return Bytes(errLoc.begin() + s, errLoc.end());
}

}  // namespace detail

inline Bytes rs_encode(const Bytes& msg, int nparity) {
    Bytes gen = detail::generator(nparity);
    Bytes out(msg.size() + nparity, 0);
    for (std::size_t i = 0; i < msg.size(); i++) out[i] = msg[i];
    for (std::size_t i = 0; i < msg.size(); i++) {
        std::uint8_t coef = out[i];
        if (coef)
            for (std::size_t j = 1; j < gen.size(); j++) out[i + j] ^= detail::mul(gen[j], coef);
    }
    Bytes code(msg.begin(), msg.end());
    code.insert(code.end(), out.begin() + msg.size(), out.end());
    return code;
}

// Returns (message, corrected). Throws std::runtime_error if uncorrectable.
inline std::pair<Bytes, int> rs_decode(const Bytes& codeword, int nparity, int msglen = -1) {
    Bytes cw = codeword;
    if (msglen < 0) msglen = static_cast<int>(cw.size()) - nparity;
    Bytes synd = detail::syndromes(cw, nparity);
    bool zero = true;
    for (auto s : synd)
        if (s) { zero = false; break; }
    if (zero) return {Bytes(cw.begin(), cw.begin() + msglen), 0};
    Bytes errLoc = detail::errLocator(synd, nparity);
    if (static_cast<int>(errLoc.size()) - 1 > nparity / 2)
        throw std::runtime_error("fec: too many errors");
    Bytes rev(errLoc.rbegin(), errLoc.rend());
    int errs = static_cast<int>(rev.size()) - 1;
    std::vector<int> pos;
    for (std::size_t i = 0; i < cw.size(); i++)
        if (detail::peval(rev, detail::pow(kGen, static_cast<int>(i))) == 0)
            pos.push_back(static_cast<int>(cw.size()) - 1 - static_cast<int>(i));
    if (static_cast<int>(pos.size()) != errs) throw std::runtime_error("fec: error location failed");
    // Forney
    std::vector<int> coefPos;
    Bytes eLoc{1}, X;
    for (int p : pos) {
        int cp = static_cast<int>(cw.size()) - 1 - p;
        coefPos.push_back(cp);
        eLoc = detail::pmul(eLoc, {detail::pow(kGen, cp), 1});
        X.push_back(detail::pow(kGen, cp));
    }
    Bytes syndRev(synd.rbegin(), synd.rend());
    Bytes prod = detail::pmul(syndRev, eLoc);
    Bytes rem(prod.end() - eLoc.size(), prod.end());
    for (std::size_t i = 0; i < X.size(); i++) {
        std::uint8_t Xi = X[i], XiInv = detail::inv(Xi);
        std::uint8_t denom = 1;
        for (std::size_t j = 0; j < X.size(); j++)
            if (j != i) denom = detail::mul(denom, static_cast<std::uint8_t>(1 ^ detail::mul(XiInv, X[j])));
        std::uint8_t numer = detail::peval(rem, XiInv);
        numer = detail::mul(numer, detail::pow(Xi, 1 - kFcr));
        if (denom == 0) throw std::runtime_error("fec: forney denominator zero");
        cw[pos[i]] ^= detail::div(numer, denom);
    }
    for (auto s : detail::syndromes(cw, nparity))
        if (s) throw std::runtime_error("fec: residual syndrome");
    return {Bytes(cw.begin(), cw.begin() + msglen), static_cast<int>(pos.size())};
}

inline Bytes interleave(const std::vector<Bytes>& cws) {
    if (cws.empty()) return {};
    std::size_t d = cws.size(), n = cws[0].size();
    Bytes out(d * n, 0);
    for (std::size_t r = 0; r < d; r++)
        for (std::size_t c = 0; c < n; c++) out[c * d + r] = cws[r][c];
    return out;
}
inline std::vector<Bytes> deinterleave(const Bytes& s, std::size_t depth, std::size_t n) {
    std::vector<Bytes> cws(depth, Bytes(n, 0));
    for (std::size_t c = 0; c < n; c++)
        for (std::size_t r = 0; r < depth; r++) cws[r][c] = s[c * depth + r];
    return cws;
}

inline std::pair<std::size_t, std::size_t> chunking(std::size_t l, int np) {
    std::size_t maxk = 255 - np;
    std::size_t nchunks = (l == 0) ? 1 : (l + maxk - 1) / maxk;
    std::size_t k = (l == 0) ? 1 : (l + nchunks - 1) / nchunks;
    return {nchunks, k};
}

inline Bytes encode_message(const Bytes& msg, int nparity) {
    std::size_t l = msg.size();
    auto [nchunks, k] = chunking(l, nparity);
    Bytes hdr{static_cast<std::uint8_t>(l >> 24), static_cast<std::uint8_t>(l >> 16),
              static_cast<std::uint8_t>(l >> 8), static_cast<std::uint8_t>(l),
              static_cast<std::uint8_t>(nparity)};
    Bytes out = rs_encode(hdr, kHdrParity);
    std::vector<Bytes> cws;
    for (std::size_t c = 0; c < nchunks; c++) {
        Bytes block(k, 0);
        for (std::size_t i = 0; i < k; i++)
            if (c * k + i < l) block[i] = msg[c * k + i];
        cws.push_back(rs_encode(block, nparity));
    }
    Bytes body = interleave(cws);
    out.insert(out.end(), body.begin(), body.end());
    return out;
}

inline std::pair<Bytes, int> decode_message(const Bytes& blob) {
    if (blob.size() < kHdrLen) throw std::runtime_error("fec: short blob");
    auto [hdr, _] = rs_decode(Bytes(blob.begin(), blob.begin() + kHdrLen), kHdrParity, 5);
    std::size_t l = (static_cast<std::size_t>(hdr[0]) << 24) | (static_cast<std::size_t>(hdr[1]) << 16) |
                    (static_cast<std::size_t>(hdr[2]) << 8) | hdr[3];
    int np = hdr[4];
    auto [nchunks, k] = chunking(l, np);
    std::size_t cwlen = k + np;
    Bytes body(blob.begin() + kHdrLen, blob.end());
    if (body.size() != nchunks * cwlen) throw std::runtime_error("fec: blob length mismatch");
    Bytes out;
    int total = 0;
    for (auto& cw : deinterleave(body, nchunks, cwlen)) {
        auto [block, n] = rs_decode(cw, np, static_cast<int>(k));
        total += n;
        out.insert(out.end(), block.begin(), block.end());
    }
    if (l < out.size()) out.resize(l);
    return {out, total};
}

}  // namespace fec
}  // namespace dcf

#endif  // DCF_FEC_HPP
