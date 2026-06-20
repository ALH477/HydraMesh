// SPDX-License-Identifier: LGPL-3.0-only

/// DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
/// byte-identical to the Python/C/Rust/Go/Node/C++/Perl/Lua/Lisp/Haskell/Java/Kotlin
/// references (pinned by Documentation/fec_vectors.json). FEC wraps a 17-byte
/// DeModFrame so a lossy medium (RF/SDR, acoustic) can CORRECT it, not just detect.
///
/// Field GF(2^8), prim 0x11D, generator 2, fcr 0. Systematic: codeword =
/// message ++ parity. Default 2t=16 parity -> corrects 8 byte-errors. The message
/// layer chunks + interleaves any-length payloads behind a self-protecting header.

private let GF_PRIM = 0x11D
private let GF_GEN = 2
private let FCR = 0
public let RS_DEFAULT_PARITY = 16
private let HDR_PARITY = 16
private let HDR_LEN = 21

public enum FECError: Error {
    case tooManyErrors, locationFailed, forneyFailed, residual, shortBlob, lengthMismatch
}

public struct FECResult {
    public let msg: [UInt8]
    public let corrected: Int
}

private let EXP: [Int] = {
    var e = [Int](repeating: 0, count: 512)
    var x = 1
    for i in 0..<255 { e[i] = x; x <<= 1; if x & 0x100 != 0 { x ^= GF_PRIM } }
    for i in 255..<512 { e[i] = e[i - 255] }
    return e
}()
private let LOG: [Int] = {
    var l = [Int](repeating: 0, count: 256)
    var x = 1
    for i in 0..<255 { l[x] = i; x <<= 1; if x & 0x100 != 0 { x ^= GF_PRIM } }
    return l
}()

private func gmul(_ a: Int, _ b: Int) -> Int { (a == 0 || b == 0) ? 0 : EXP[LOG[a] + LOG[b]] }
private func gdiv(_ a: Int, _ b: Int) -> Int { a == 0 ? 0 : EXP[(LOG[a] + 255 - LOG[b]) % 255] }
private func gpow(_ a: Int, _ p: Int) -> Int { EXP[((LOG[a] * p) % 255 + 255) % 255] }
private func ginv(_ a: Int) -> Int { EXP[255 - LOG[a]] }

// Polynomials as [Int], high-order coefficient first.
private func pmul(_ p: [Int], _ q: [Int]) -> [Int] {
    var r = [Int](repeating: 0, count: p.count + q.count - 1)
    for j in 0..<q.count { for i in 0..<p.count { r[i + j] ^= gmul(p[i], q[j]) } }
    return r
}
private func padd(_ p: [Int], _ q: [Int]) -> [Int] {
    let n = max(p.count, q.count)
    var r = [Int](repeating: 0, count: n)
    for i in 0..<p.count { r[i + n - p.count] = p[i] }
    for i in 0..<q.count { r[i + n - q.count] ^= q[i] }
    return r
}
private func pscale(_ p: [Int], _ s: Int) -> [Int] { p.map { gmul($0, s) } }
private func peval(_ p: [Int], _ x: Int) -> Int {
    var y = p[0]
    for i in 1..<p.count { y = gmul(y, x) ^ p[i] }
    return y
}
private func genpoly(_ np: Int) -> [Int] {
    var g = [1]
    for i in 0..<np { g = pmul(g, [1, gpow(GF_GEN, FCR + i)]) }
    return g
}

/// message bytes -> codeword bytes (message ++ nparity parity), systematic.
public func rsEncode(_ msg: [UInt8], _ np: Int) -> [UInt8] {
    let m = msg.count
    let gen = genpoly(np)
    var out = [Int](repeating: 0, count: m + np)
    for i in 0..<m { out[i] = Int(msg[i]) }
    for i in 0..<m {
        let coef = out[i]
        if coef != 0 { for j in 1..<gen.count { out[i + j] ^= gmul(gen[j], coef) } }
    }
    var code = msg
    code.append(contentsOf: (m..<m + np).map { UInt8(out[$0]) })
    return code
}

private func syndromes(_ cw: [Int], _ np: Int) -> [Int] {
    var s = [Int](repeating: 0, count: np + 1)
    for i in 0..<np { s[i + 1] = peval(cw, gpow(GF_GEN, FCR + i)) }
    return s
}
private func errLocator(_ synd: [Int], _ np: Int) -> [Int] {
    var el = [1]
    var ol = [1]
    for i in 0..<np {
        var delta = synd[i + 1]
        for j in 1..<el.count { delta ^= gmul(el[el.count - 1 - j], synd[i + 1 - j]) }
        ol.append(0)
        if delta != 0 {
            if ol.count > el.count {
                let nl = pscale(ol, delta)
                ol = pscale(el, ginv(delta))
                el = nl
            }
            el = padd(el, pscale(ol, delta))
        }
    }
    var s = 0
    while s < el.count && el[s] == 0 { s += 1 }
    return Array(el.dropFirst(s))
}

/// codeword bytes -> FECResult. msglen < 0 means codeword.count - nparity.
public func rsDecode(_ codeword: [UInt8], _ np: Int, _ msglen: Int) throws -> FECResult {
    let n = codeword.count
    var cw = codeword.map { Int($0) }
    let ml = msglen < 0 ? n - np : msglen
    let synd = syndromes(cw, np)
    if synd.allSatisfy({ $0 == 0 }) { return FECResult(msg: cw[0..<ml].map { UInt8($0) }, corrected: 0) }
    let el = errLocator(synd, np)
    if el.count - 1 > np / 2 { throw FECError.tooManyErrors }
    let rev = Array(el.reversed())
    let errs = rev.count - 1
    var pos = [Int]()
    for i in 0..<n where peval(rev, gpow(GF_GEN, i)) == 0 { pos.append(n - 1 - i) }
    if pos.count != errs { throw FECError.locationFailed }
    var eloc = [1]
    var xs = [Int]()
    for p in pos {
        let cp = n - 1 - p
        eloc = pmul(eloc, [gpow(GF_GEN, cp), 1])
        xs.append(gpow(GF_GEN, cp))
    }
    let prod = pmul(Array(synd.reversed()), eloc)
    let rem = Array(prod[(prod.count - eloc.count)...])
    for i in 0..<xs.count {
        let xi = xs[i]
        let xiInv = ginv(xi)
        var denom = 1
        for j in 0..<xs.count where j != i { denom = gmul(denom, 1 ^ gmul(xiInv, xs[j])) }
        let numer = gmul(peval(rem, xiInv), gpow(xi, 1 - FCR))
        if denom == 0 { throw FECError.forneyFailed }
        cw[pos[i]] ^= gdiv(numer, denom)
    }
    if !syndromes(cw, np).allSatisfy({ $0 == 0 }) { throw FECError.residual }
    return FECResult(msg: cw[0..<ml].map { UInt8($0) }, corrected: pos.count)
}

public func interleave(_ cws: [[UInt8]]) -> [UInt8] {
    if cws.isEmpty { return [] }
    let d = cws.count
    let n = cws[0].count
    var out = [UInt8](repeating: 0, count: d * n)
    for r in 0..<d { for c in 0..<n { out[c * d + r] = cws[r][c] } }
    return out
}
public func deinterleave(_ stream: [UInt8], _ depth: Int, _ n: Int) -> [[UInt8]] {
    var cws = [[UInt8]](repeating: [UInt8](repeating: 0, count: n), count: depth)
    for c in 0..<n { for r in 0..<depth { cws[r][c] = stream[c * depth + r] } }
    return cws
}

private func chunking(_ l: Int, _ np: Int) -> (Int, Int) {
    let maxk = 255 - np
    let nchunks = l == 0 ? 1 : (l + maxk - 1) / maxk
    let k = l == 0 ? 1 : (l + nchunks - 1) / nchunks
    return (nchunks, k)
}

public func encodeMessage(_ msg: [UInt8], _ np: Int) -> [UInt8] {
    let l = msg.count
    let (nchunks, k) = chunking(l, np)
    let hdr: [UInt8] = [UInt8((l >> 24) & 255), UInt8((l >> 16) & 255), UInt8((l >> 8) & 255), UInt8(l & 255), UInt8(np)]
    var out = rsEncode(hdr, HDR_PARITY)
    var cws = [[UInt8]]()
    for c in 0..<nchunks {
        var block = [UInt8](repeating: 0, count: k)
        for i in 0..<k {
            let o = c * k + i
            if o < l { block[i] = msg[o] }
        }
        cws.append(rsEncode(block, np))
    }
    out.append(contentsOf: interleave(cws))
    return out
}

public func decodeMessage(_ blob: [UInt8]) throws -> FECResult {
    if blob.count < HDR_LEN { throw FECError.shortBlob }
    let h = try rsDecode(Array(blob[0..<HDR_LEN]), HDR_PARITY, 5).msg
    let l = (Int(h[0]) << 24) | (Int(h[1]) << 16) | (Int(h[2]) << 8) | Int(h[3])
    let np = Int(h[4])
    let (nchunks, k) = chunking(l, np)
    let cwlen = k + np
    let body = Array(blob[HDR_LEN...])
    if body.count != nchunks * cwlen { throw FECError.lengthMismatch }
    let cws = deinterleave(body, nchunks, cwlen)
    var out = [UInt8]()
    var total = 0
    for cw in cws {
        let r = try rsDecode(cw, np, k)
        total += r.corrected
        out.append(contentsOf: r.msg)
    }
    if l < out.count { out = Array(out[0..<l]) }
    return FECResult(msg: out, corrected: total)
}
