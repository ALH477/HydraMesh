// SPDX-License-Identifier: LGPL-3.0-only
package com.demod.dcf;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
 * byte-identical to feclab_core.py, codec/demod_fec.h, codec/src/fec.rs, go/dcf/fec.go,
 * JS/nodejs/src/fec.js, cpp/include/dcf/fec.hpp, perl/lib/DCF/FEC.pm, lua/dcf_fec.lua,
 * lisp/src/fec.lisp, and DCF.Transport.FEC (pinned by Documentation/fec_vectors.json).
 * FEC wraps a 17-byte DeModFrame so a lossy medium can CORRECT it, not just detect.
 *
 * <p>Field GF(2^8), prim 0x11D, generator 2, fcr 0. Systematic: codeword =
 * message ++ parity. Default 2t=16 parity -&gt; corrects 8 byte-errors. The message
 * layer chunks + interleaves any-length payloads behind a self-protecting header.
 */
public final class FEC {

    public static final int GF_PRIM = 0x11D;
    public static final int GF_GEN = 2;
    public static final int FCR = 0;
    public static final int RS_DEFAULT_PARITY = 16;
    public static final int HDR_PARITY = 16;
    public static final int HDR_LEN = 21;

    private static final int[] EXP = new int[512];
    private static final int[] LOG = new int[256];
    static {
        int x = 1;
        for (int i = 0; i < 255; i++) {
            EXP[i] = x;
            LOG[x] = i;
            x <<= 1;
            if ((x & 0x100) != 0) {
                x ^= GF_PRIM;
            }
        }
        for (int i = 255; i < 512; i++) {
            EXP[i] = EXP[i - 255];
        }
    }

    private FEC() {}

    static int gmul(int a, int b) { return (a == 0 || b == 0) ? 0 : EXP[LOG[a] + LOG[b]]; }
    static int gdiv(int a, int b) { return a == 0 ? 0 : EXP[(LOG[a] + 255 - LOG[b]) % 255]; }
    static int gpow(int a, int p) { return EXP[((LOG[a] * p) % 255 + 255) % 255]; }
    static int ginv(int a) { return EXP[255 - LOG[a]]; }

    static int[] pmul(int[] p, int[] q) {
        int[] r = new int[p.length + q.length - 1];
        for (int j = 0; j < q.length; j++) {
            for (int i = 0; i < p.length; i++) {
                r[i + j] ^= gmul(p[i], q[j]);
            }
        }
        return r;
    }
    static int[] padd(int[] p, int[] q) {
        int n = Math.max(p.length, q.length);
        int[] r = new int[n];
        for (int i = 0; i < p.length; i++) { r[i + n - p.length] = p[i]; }
        for (int i = 0; i < q.length; i++) { r[i + n - q.length] ^= q[i]; }
        return r;
    }
    static int[] pscale(int[] p, int s) {
        int[] r = new int[p.length];
        for (int i = 0; i < p.length; i++) { r[i] = gmul(p[i], s); }
        return r;
    }
    static int peval(int[] p, int x) {
        int y = p[0];
        for (int i = 1; i < p.length; i++) { y = gmul(y, x) ^ p[i]; }
        return y;
    }
    static int[] genpoly(int np) {
        int[] g = {1};
        for (int i = 0; i < np; i++) { g = pmul(g, new int[] {1, gpow(GF_GEN, FCR + i)}); }
        return g;
    }

    /** message bytes -&gt; codeword bytes (message ++ nparity parity), systematic. */
    public static byte[] rsEncode(byte[] msg, int np) {
        int m = msg.length;
        int[] gen = genpoly(np);
        int[] out = new int[m + np];
        for (int i = 0; i < m; i++) { out[i] = msg[i] & 0xFF; }
        for (int i = 0; i < m; i++) {
            int coef = out[i];
            if (coef != 0) {
                for (int j = 1; j < gen.length; j++) { out[i + j] ^= gmul(gen[j], coef); }
            }
        }
        byte[] code = new byte[m + np];
        for (int i = 0; i < m; i++) { code[i] = msg[i]; }
        for (int i = 0; i < np; i++) { code[m + i] = (byte) out[m + i]; }
        return code;
    }

    /** Decode result: the recovered message and the number of corrected bytes. */
    public static final class Result {
        public final byte[] msg;
        public final int corrected;
        Result(byte[] m, int c) { msg = m; corrected = c; }
    }

    static int[] syndromes(int[] cw, int np) {
        int[] s = new int[np + 1];
        for (int i = 0; i < np; i++) { s[i + 1] = peval(cw, gpow(GF_GEN, FCR + i)); }
        return s;
    }
    static int[] errLocator(int[] synd, int np) {
        int[] el = {1};
        int[] ol = {1};
        for (int i = 0; i < np; i++) {
            int delta = synd[i + 1];
            for (int j = 1; j < el.length; j++) { delta ^= gmul(el[el.length - 1 - j], synd[i + 1 - j]); }
            ol = Arrays.copyOf(ol, ol.length + 1);
            if (delta != 0) {
                if (ol.length > el.length) {
                    int[] nl = pscale(ol, delta);
                    ol = pscale(el, ginv(delta));
                    el = nl;
                }
                el = padd(el, pscale(ol, delta));
            }
        }
        int s = 0;
        while (s < el.length && el[s] == 0) { s++; }
        return Arrays.copyOfRange(el, s, el.length);
    }
    static int[] reverse(int[] a) {
        int[] r = new int[a.length];
        for (int i = 0; i < a.length; i++) { r[i] = a[a.length - 1 - i]; }
        return r;
    }

    /** codeword bytes -&gt; Result. msglen &lt; 0 means codeword.length - nparity. */
    public static Result rsDecode(byte[] codeword, int np, int msglen) {
        int n = codeword.length;
        int[] cw = new int[n];
        for (int i = 0; i < n; i++) { cw[i] = codeword[i] & 0xFF; }
        if (msglen < 0) { msglen = n - np; }
        int[] synd = syndromes(cw, np);
        boolean zero = true;
        for (int s : synd) { if (s != 0) { zero = false; break; } }
        if (zero) { return new Result(slice(cw, msglen), 0); }
        int[] el = errLocator(synd, np);
        if (el.length - 1 > np / 2) { throw new RuntimeException("fec: too many errors"); }
        int[] rev = reverse(el);
        int errs = rev.length - 1;
        List<Integer> pos = new ArrayList<>();
        for (int i = 0; i < n; i++) { if (peval(rev, gpow(GF_GEN, i)) == 0) { pos.add(n - 1 - i); } }
        if (pos.size() != errs) { throw new RuntimeException("fec: error location failed"); }
        int[] eloc = {1};
        int[] xs = new int[pos.size()];
        for (int i = 0; i < pos.size(); i++) {
            int cp = n - 1 - pos.get(i);
            eloc = pmul(eloc, new int[] {gpow(GF_GEN, cp), 1});
            xs[i] = gpow(GF_GEN, cp);
        }
        int[] prod = pmul(reverse(synd), eloc);
        int[] rem = Arrays.copyOfRange(prod, prod.length - eloc.length, prod.length);
        for (int i = 0; i < xs.length; i++) {
            int xi = xs[i];
            int xiInv = ginv(xi);
            int denom = 1;
            for (int j = 0; j < xs.length; j++) { if (j != i) { denom = gmul(denom, 1 ^ gmul(xiInv, xs[j])); } }
            int numer = gmul(peval(rem, xiInv), gpow(xi, 1 - FCR));
            if (denom == 0) { throw new RuntimeException("fec: forney denom zero"); }
            cw[pos.get(i)] ^= gdiv(numer, denom);
        }
        for (int s : syndromes(cw, np)) { if (s != 0) { throw new RuntimeException("fec: residual syndrome"); } }
        return new Result(slice(cw, msglen), pos.size());
    }
    static byte[] slice(int[] cw, int len) {
        byte[] b = new byte[len];
        for (int i = 0; i < len; i++) { b[i] = (byte) cw[i]; }
        return b;
    }

    public static byte[] interleave(byte[][] cws) {
        if (cws.length == 0) { return new byte[0]; }
        int d = cws.length;
        int n = cws[0].length;
        byte[] out = new byte[d * n];
        for (int r = 0; r < d; r++) { for (int c = 0; c < n; c++) { out[c * d + r] = cws[r][c]; } }
        return out;
    }
    public static byte[][] deinterleave(byte[] stream, int depth, int n) {
        byte[][] cws = new byte[depth][n];
        for (int c = 0; c < n; c++) { for (int r = 0; r < depth; r++) { cws[r][c] = stream[c * depth + r]; } }
        return cws;
    }
    static int[] chunking(int l, int np) {
        int maxk = 255 - np;
        int nchunks = l == 0 ? 1 : (l + maxk - 1) / maxk;
        int k = l == 0 ? 1 : (l + nchunks - 1) / nchunks;
        return new int[] {nchunks, k};
    }

    public static byte[] encodeMessage(byte[] msg, int np) {
        int l = msg.length;
        int[] ck = chunking(l, np);
        int nchunks = ck[0];
        int k = ck[1];
        byte[] hdr = {(byte) (l >> 24), (byte) (l >> 16), (byte) (l >> 8), (byte) l, (byte) np};
        byte[] out = rsEncode(hdr, HDR_PARITY);
        byte[][] cws = new byte[nchunks][];
        for (int c = 0; c < nchunks; c++) {
            byte[] block = new byte[k];
            for (int i = 0; i < k; i++) {
                int o = c * k + i;
                if (o < l) { block[i] = msg[o]; }
            }
            cws[c] = rsEncode(block, np);
        }
        byte[] body = interleave(cws);
        byte[] res = new byte[out.length + body.length];
        System.arraycopy(out, 0, res, 0, out.length);
        System.arraycopy(body, 0, res, out.length, body.length);
        return res;
    }

    public static Result decodeMessage(byte[] blob) {
        if (blob.length < HDR_LEN) { throw new RuntimeException("fec: short blob"); }
        byte[] h = rsDecode(Arrays.copyOfRange(blob, 0, HDR_LEN), HDR_PARITY, 5).msg;
        int l = ((h[0] & 0xFF) << 24) | ((h[1] & 0xFF) << 16) | ((h[2] & 0xFF) << 8) | (h[3] & 0xFF);
        int np = h[4] & 0xFF;
        int[] ck = chunking(l, np);
        int nchunks = ck[0];
        int k = ck[1];
        int cwlen = k + np;
        byte[] body = Arrays.copyOfRange(blob, HDR_LEN, blob.length);
        if (body.length != nchunks * cwlen) { throw new RuntimeException("fec: blob length mismatch"); }
        byte[][] cws = deinterleave(body, nchunks, cwlen);
        byte[] out = new byte[nchunks * k];
        int total = 0;
        for (int c = 0; c < nchunks; c++) {
            Result r = rsDecode(cws[c], np, k);
            total += r.corrected;
            System.arraycopy(r.msg, 0, out, c * k, k);
        }
        return new Result(Arrays.copyOfRange(out, 0, l), total);
    }
}
