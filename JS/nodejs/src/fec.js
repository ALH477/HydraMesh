// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
// byte-identical to python/MCP/feclab_core.py, codec/demod_fec.h, codec/src/fec.rs,
// and go/dcf/fec.go, pinned by Documentation/fec_vectors.json. FEC wraps a 17-byte
// DeModFrame so a lossy medium (RF/SDR, acoustic) can CORRECT it, not just detect
// damage — the frame and its 246-vector certificate are untouched.
//
// Field GF(2^8), prim 0x11D, generator a=2, fcr=0. Systematic: codeword =
// message ++ parity. Default 2t=16 parity bytes -> corrects 8 byte-errors. The
// multi-codeword message layer chunks + interleaves any-length payloads behind a
// self-protecting header.

const GF_PRIM = 0x11d;
const GF_GEN = 2;
const FCR = 0;
const RS_DEFAULT_NPARITY = 16;
const HDR_PARITY = 16;
const HDR_LEN = 5 + HDR_PARITY; // 21-byte self-protecting header

const EXP = new Uint8Array(512);
const LOG = new Uint8Array(256);
(function buildTables() {
  let x = 1;
  for (let i = 0; i < 255; i++) {
    EXP[i] = x;
    LOG[x] = i;
    x <<= 1;
    if (x & 0x100) x ^= GF_PRIM;
  }
  for (let i = 255; i < 512; i++) EXP[i] = EXP[i - 255];
})();

const gmul = (a, b) => (a === 0 || b === 0 ? 0 : EXP[LOG[a] + LOG[b]]);
const gdiv = (a, b) => (a === 0 ? 0 : EXP[(LOG[a] + 255 - LOG[b]) % 255]);
const gpow = (a, p) => EXP[((((LOG[a] * p) % 255) + 255) % 255)];
const ginv = (a) => EXP[255 - LOG[a]];

// GF polynomials (high-order coefficient first), as plain number arrays.
function pmul(p, q) {
  const r = new Array(p.length + q.length - 1).fill(0);
  for (let j = 0; j < q.length; j++)
    for (let i = 0; i < p.length; i++) r[i + j] ^= gmul(p[i], q[j]);
  return r;
}
function padd(p, q) {
  const n = Math.max(p.length, q.length);
  const r = new Array(n).fill(0);
  for (let i = 0; i < p.length; i++) r[i + n - p.length] = p[i];
  for (let i = 0; i < q.length; i++) r[i + n - q.length] ^= q[i];
  return r;
}
const pscale = (p, s) => p.map((c) => gmul(c, s));
function peval(p, x) {
  let y = p[0];
  for (let i = 1; i < p.length; i++) y = gmul(y, x) ^ p[i];
  return y;
}

function generatorPoly(nparity) {
  let g = [1];
  for (let i = 0; i < nparity; i++) g = pmul(g, [1, gpow(GF_GEN, FCR + i)]);
  return g;
}

/** message Buffer -> codeword Buffer (message ++ nparity parity bytes), systematic. */
function rsEncode(msg, nparity) {
  const m = Array.from(msg);
  const gen = generatorPoly(nparity);
  const out = m.concat(new Array(nparity).fill(0));
  for (let i = 0; i < m.length; i++) {
    const coef = out[i];
    if (coef !== 0) for (let j = 1; j < gen.length; j++) out[i + j] ^= gmul(gen[j], coef);
  }
  return Buffer.from(m.concat(out.slice(m.length)));
}

function syndromes(cw, nparity) {
  const s = [0];
  for (let i = 0; i < nparity; i++) s.push(peval(cw, gpow(GF_GEN, FCR + i)));
  return s;
}
function errLocator(synd, nparity) {
  let errLoc = [1];
  let oldLoc = [1];
  for (let i = 0; i < nparity; i++) {
    let delta = synd[i + 1];
    for (let j = 1; j < errLoc.length; j++) delta ^= gmul(errLoc[errLoc.length - 1 - j], synd[i + 1 - j]);
    oldLoc = oldLoc.concat([0]);
    if (delta !== 0) {
      if (oldLoc.length > errLoc.length) {
        const newLoc = pscale(oldLoc, delta);
        oldLoc = pscale(errLoc, ginv(delta));
        errLoc = newLoc;
      }
      errLoc = padd(errLoc, pscale(oldLoc, delta));
    }
  }
  while (errLoc.length && errLoc[0] === 0) errLoc.shift();
  return errLoc;
}
function errPositions(errLocRev, nmess) {
  const errs = errLocRev.length - 1;
  const pos = [];
  for (let i = 0; i < nmess; i++) if (peval(errLocRev, gpow(GF_GEN, i)) === 0) pos.push(nmess - 1 - i);
  return pos.length === errs ? pos : null;
}
function correct(cw, synd, errPos) {
  const n = cw.length;
  const coefPos = errPos.map((p) => n - 1 - p);
  let eLoc = [1];
  const X = [];
  for (const p of coefPos) {
    eLoc = pmul(eLoc, [gpow(GF_GEN, p), 1]);
    X.push(gpow(GF_GEN, p));
  }
  const prod = pmul(synd.slice().reverse(), eLoc);
  const rem = prod.slice(prod.length - eLoc.length);
  for (let i = 0; i < X.length; i++) {
    const Xi = X[i];
    const XiInv = ginv(Xi);
    let denom = 1;
    for (let j = 0; j < X.length; j++) if (j !== i) denom = gmul(denom, 1 ^ gmul(XiInv, X[j]));
    let numer = peval(rem, XiInv);
    numer = gmul(numer, gpow(Xi, 1 - FCR));
    if (denom === 0) return false;
    cw[errPos[i]] ^= gdiv(numer, denom);
  }
  return true;
}

/** codeword Buffer -> { msg: Buffer, corrected: number }. Throws if uncorrectable. */
function rsDecode(codeword, nparity, msglen) {
  const cw = Array.from(codeword);
  if (msglen == null) msglen = cw.length - nparity;
  const synd = syndromes(cw, nparity);
  if (synd.every((s) => s === 0)) return { msg: Buffer.from(cw.slice(0, msglen)), corrected: 0 };
  const errLoc = errLocator(synd, nparity);
  if (errLoc.length - 1 > nparity >> 1) throw new Error('fec: too many errors');
  const pos = errPositions(errLoc.slice().reverse(), cw.length);
  if (!pos) throw new Error('fec: error location failed');
  if (!correct(cw, synd, pos)) throw new Error('fec: forney failed');
  if (!syndromes(cw, nparity).every((s) => s === 0)) throw new Error('fec: residual syndrome');
  return { msg: Buffer.from(cw.slice(0, msglen)), corrected: pos.length };
}

function interleave(codewords) {
  if (codewords.length === 0) return Buffer.alloc(0);
  const d = codewords.length;
  const n = codewords[0].length;
  const out = Buffer.alloc(d * n);
  for (let r = 0; r < d; r++) for (let c = 0; c < n; c++) out[c * d + r] = codewords[r][c];
  return out;
}
function deinterleave(stream, depth, n) {
  const cws = [];
  for (let r = 0; r < depth; r++) cws.push(Buffer.alloc(n));
  for (let c = 0; c < n; c++) for (let r = 0; r < depth; r++) cws[r][c] = stream[c * depth + r];
  return cws;
}

function chunking(l, np) {
  const maxk = 255 - np;
  const nchunks = l === 0 ? 1 : Math.ceil(l / maxk);
  const k = l === 0 ? 1 : Math.ceil(l / nchunks);
  return [nchunks, k];
}

/** any-length message Buffer -> self-describing RS+interleave blob Buffer. */
function encodeMessage(msg, nparity) {
  msg = Buffer.from(msg);
  const l = msg.length;
  const [nchunks, k] = chunking(l, nparity);
  const hdr = Buffer.from([(l >>> 24) & 255, (l >>> 16) & 255, (l >>> 8) & 255, l & 255, nparity]);
  const out = rsEncode(hdr, HDR_PARITY);
  const cws = [];
  for (let c = 0; c < nchunks; c++) {
    const block = Buffer.alloc(k);
    for (let i = 0; i < k; i++) {
      const o = c * k + i;
      if (o < l) block[i] = msg[o];
    }
    cws.push(rsEncode(block, nparity));
  }
  return Buffer.concat([out, interleave(cws)]);
}

/** blob Buffer -> { msg: Buffer, corrected: number }. Throws if uncorrectable. */
function decodeMessage(blob) {
  blob = Buffer.from(blob);
  if (blob.length < HDR_LEN) throw new Error('fec: short blob');
  const { msg: hdr } = rsDecode(blob.slice(0, HDR_LEN), HDR_PARITY, 5);
  const l = ((hdr[0] << 24) | (hdr[1] << 16) | (hdr[2] << 8) | hdr[3]) >>> 0;
  const np = hdr[4];
  const [nchunks, k] = chunking(l, np);
  const cwlen = k + np;
  const body = blob.slice(HDR_LEN);
  if (body.length !== nchunks * cwlen) throw new Error('fec: blob length mismatch');
  const parts = [];
  let total = 0;
  for (const cw of deinterleave(body, nchunks, cwlen)) {
    const { msg: block, corrected } = rsDecode(cw, np, k);
    total += corrected;
    parts.push(block);
  }
  let out = Buffer.concat(parts);
  if (l < out.length) out = out.slice(0, l);
  return { msg: out, corrected: total };
}

module.exports = {
  rsEncode, rsDecode, interleave, deinterleave, encodeMessage, decodeMessage,
  generatorPoly, RS_DEFAULT_NPARITY, HDR_PARITY, HDR_LEN,
};
