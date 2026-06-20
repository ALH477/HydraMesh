// SPDX-License-Identifier: LGPL-3.0-only
package dcf

/**
 * DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
 * byte-identical to the Python/C/Rust/Go/Node/C++/Perl/Lua/Lisp/Haskell/Java
 * references (pinned by Documentation/fec_vectors.json). FEC wraps a 17-byte
 * DeModFrame so a lossy medium (RF/SDR, acoustic) can CORRECT it, not just detect.
 *
 * Field GF(2^8), prim 0x11D, generator 2, fcr 0. Systematic: codeword =
 * message ++ parity. Default 2t=16 parity -> corrects 8 byte-errors. The message
 * layer chunks + interleaves any-length payloads behind a self-protecting header.
 */
object FEC {
    const val GF_PRIM = 0x11D
    const val GF_GEN = 2
    const val FCR = 0
    const val RS_DEFAULT_PARITY = 16
    const val HDR_PARITY = 16
    const val HDR_LEN = 21

    private val EXP = IntArray(512)
    private val LOG = IntArray(256)
    init {
        var x = 1
        for (i in 0..254) { EXP[i] = x; LOG[x] = i; x = x shl 1; if (x and 0x100 != 0) x = x xor GF_PRIM }
        for (i in 255 until 512) EXP[i] = EXP[i - 255]
    }

    private fun gmul(a: Int, b: Int) = if (a == 0 || b == 0) 0 else EXP[LOG[a] + LOG[b]]
    private fun gdiv(a: Int, b: Int) = if (a == 0) 0 else EXP[(LOG[a] + 255 - LOG[b]) % 255]
    private fun gpow(a: Int, p: Int) = EXP[((LOG[a] * p) % 255 + 255) % 255]
    private fun ginv(a: Int) = EXP[255 - LOG[a]]

    private fun pmul(p: IntArray, q: IntArray): IntArray {
        val r = IntArray(p.size + q.size - 1)
        for (j in q.indices) for (i in p.indices) r[i + j] = r[i + j] xor gmul(p[i], q[j])
        return r
    }
    private fun padd(p: IntArray, q: IntArray): IntArray {
        val n = maxOf(p.size, q.size)
        val r = IntArray(n)
        for (i in p.indices) r[i + n - p.size] = p[i]
        for (i in q.indices) r[i + n - q.size] = r[i + n - q.size] xor q[i]
        return r
    }
    private fun pscale(p: IntArray, s: Int) = IntArray(p.size) { gmul(p[it], s) }
    private fun peval(p: IntArray, x: Int): Int {
        var y = p[0]
        for (i in 1 until p.size) y = gmul(y, x) xor p[i]
        return y
    }
    private fun genpoly(np: Int): IntArray {
        var g = intArrayOf(1)
        for (i in 0 until np) g = pmul(g, intArrayOf(1, gpow(GF_GEN, FCR + i)))
        return g
    }

    fun rsEncode(msg: ByteArray, np: Int): ByteArray {
        val m = msg.size
        val gen = genpoly(np)
        val out = IntArray(m + np)
        for (i in 0 until m) out[i] = msg[i].toInt() and 0xFF
        for (i in 0 until m) {
            val coef = out[i]
            if (coef != 0) for (j in 1 until gen.size) out[i + j] = out[i + j] xor gmul(gen[j], coef)
        }
        val code = ByteArray(m + np)
        for (i in 0 until m) code[i] = msg[i]
        for (i in 0 until np) code[m + i] = out[m + i].toByte()
        return code
    }

    data class Result(val msg: ByteArray, val corrected: Int)

    private fun syndromes(cw: IntArray, np: Int): IntArray {
        val s = IntArray(np + 1)
        for (i in 0 until np) s[i + 1] = peval(cw, gpow(GF_GEN, FCR + i))
        return s
    }
    private fun errLocator(synd: IntArray, np: Int): IntArray {
        var el = intArrayOf(1)
        var ol = intArrayOf(1)
        for (i in 0 until np) {
            var delta = synd[i + 1]
            for (j in 1 until el.size) delta = delta xor gmul(el[el.size - 1 - j], synd[i + 1 - j])
            ol = ol + 0
            if (delta != 0) {
                if (ol.size > el.size) {
                    val nl = pscale(ol, delta)
                    ol = pscale(el, ginv(delta))
                    el = nl
                }
                el = padd(el, pscale(ol, delta))
            }
        }
        var s = 0
        while (s < el.size && el[s] == 0) s++
        return el.copyOfRange(s, el.size)
    }
    private fun reverse(a: IntArray) = IntArray(a.size) { a[a.size - 1 - it] }

    fun rsDecode(codeword: ByteArray, np: Int, msglen: Int): Result {
        val n = codeword.size
        val cw = IntArray(n) { codeword[it].toInt() and 0xFF }
        val ml = if (msglen < 0) n - np else msglen
        val synd = syndromes(cw, np)
        if (synd.all { it == 0 }) return Result(slice(cw, ml), 0)
        val el = errLocator(synd, np)
        if (el.size - 1 > np / 2) throw RuntimeException("fec: too many errors")
        val rev = reverse(el)
        val errs = rev.size - 1
        val pos = ArrayList<Int>()
        for (i in 0 until n) if (peval(rev, gpow(GF_GEN, i)) == 0) pos.add(n - 1 - i)
        if (pos.size != errs) throw RuntimeException("fec: error location failed")
        var eloc = intArrayOf(1)
        val xs = IntArray(pos.size)
        for (i in pos.indices) {
            val cp = n - 1 - pos[i]
            eloc = pmul(eloc, intArrayOf(gpow(GF_GEN, cp), 1))
            xs[i] = gpow(GF_GEN, cp)
        }
        val prod = pmul(reverse(synd), eloc)
        val rem = prod.copyOfRange(prod.size - eloc.size, prod.size)
        for (i in xs.indices) {
            val xi = xs[i]
            val xiInv = ginv(xi)
            var denom = 1
            for (j in xs.indices) if (j != i) denom = gmul(denom, 1 xor gmul(xiInv, xs[j]))
            val numer = gmul(peval(rem, xiInv), gpow(xi, 1 - FCR))
            if (denom == 0) throw RuntimeException("fec: forney denom zero")
            cw[pos[i]] = cw[pos[i]] xor gdiv(numer, denom)
        }
        if (!syndromes(cw, np).all { it == 0 }) throw RuntimeException("fec: residual syndrome")
        return Result(slice(cw, ml), pos.size)
    }
    private fun slice(cw: IntArray, len: Int) = ByteArray(len) { cw[it].toByte() }

    fun interleave(cws: Array<ByteArray>): ByteArray {
        if (cws.isEmpty()) return ByteArray(0)
        val d = cws.size
        val n = cws[0].size
        val out = ByteArray(d * n)
        for (r in 0 until d) for (c in 0 until n) out[c * d + r] = cws[r][c]
        return out
    }
    fun deinterleave(stream: ByteArray, depth: Int, n: Int): Array<ByteArray> {
        val cws = Array(depth) { ByteArray(n) }
        for (c in 0 until n) for (r in 0 until depth) cws[r][c] = stream[c * depth + r]
        return cws
    }
    private fun chunking(l: Int, np: Int): Pair<Int, Int> {
        val maxk = 255 - np
        val nchunks = if (l == 0) 1 else (l + maxk - 1) / maxk
        val k = if (l == 0) 1 else (l + nchunks - 1) / nchunks
        return Pair(nchunks, k)
    }

    fun encodeMessage(msg: ByteArray, np: Int): ByteArray {
        val l = msg.size
        val (nchunks, k) = chunking(l, np)
        val hdr = byteArrayOf((l shr 24).toByte(), (l shr 16).toByte(), (l shr 8).toByte(), l.toByte(), np.toByte())
        val out = rsEncode(hdr, HDR_PARITY)
        val cws = Array(nchunks) { c ->
            val block = ByteArray(k)
            for (i in 0 until k) { val o = c * k + i; if (o < l) block[i] = msg[o] }
            rsEncode(block, np)
        }
        return out + interleave(cws)
    }
    fun decodeMessage(blob: ByteArray): Result {
        if (blob.size < HDR_LEN) throw RuntimeException("fec: short blob")
        val h = rsDecode(blob.copyOfRange(0, HDR_LEN), HDR_PARITY, 5).msg
        val l = ((h[0].toInt() and 0xFF) shl 24) or ((h[1].toInt() and 0xFF) shl 16) or
            ((h[2].toInt() and 0xFF) shl 8) or (h[3].toInt() and 0xFF)
        val np = h[4].toInt() and 0xFF
        val (nchunks, k) = chunking(l, np)
        val cwlen = k + np
        val body = blob.copyOfRange(HDR_LEN, blob.size)
        if (body.size != nchunks * cwlen) throw RuntimeException("fec: blob length mismatch")
        val cws = deinterleave(body, nchunks, cwlen)
        val out = ByteArray(nchunks * k)
        var total = 0
        for (c in 0 until nchunks) {
            val r = rsDecode(cws[c], np, k)
            total += r.corrected
            r.msg.copyInto(out, c * k)
        }
        return Result(out.copyOfRange(0, l), total)
    }
}
