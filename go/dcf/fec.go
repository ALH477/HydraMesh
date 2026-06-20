// SPDX-License-Identifier: LGPL-3.0-only

package dcf

// Reed-Solomon forward-error-correction adapter — byte-identical to
// python/MCP/feclab_core.py, codec/demod_fec.h, and codec/src/fec.rs, pinned by
// Documentation/fec_vectors.json. FEC wraps a 17-byte DeModFrame so a lossy medium
// (RF/SDR, acoustic) can CORRECT it, not just detect damage — the frame and its
// 246-vector certificate are untouched. The certified half (this); the IQ/audio
// waveform that carries the bytes is analog/loopback-tested.
//
// Field GF(2^8), prim 0x11D, generator a=2, fcr=0. Systematic: codeword =
// message ++ parity. Default 2t=16 parity bytes -> corrects 8 byte-errors. The
// multi-codeword message layer chunks + interleaves any-length payloads behind a
// self-protecting header.

import "errors"

const (
	gfPrim          = 0x11D
	gfGen           = 2
	fcr             = 0
	RSDefaultParity = 16
	HdrParity       = 16
	hdrLen          = 5 + HdrParity // 21-byte self-protecting header
)

var gfExp [512]byte
var gfLog [256]byte

func init() {
	x := 1
	for i := 0; i < 255; i++ {
		gfExp[i] = byte(x)
		gfLog[x] = byte(i)
		x <<= 1
		if x&0x100 != 0 {
			x ^= gfPrim
		}
	}
	for i := 255; i < 512; i++ {
		gfExp[i] = gfExp[i-255]
	}
}

func gmul(a, b byte) byte {
	if a == 0 || b == 0 {
		return 0
	}
	return gfExp[int(gfLog[a])+int(gfLog[b])]
}
func gdiv(a, b byte) byte {
	if a == 0 {
		return 0
	}
	return gfExp[(int(gfLog[a])+255-int(gfLog[b]))%255]
}
func gpow(a byte, p int) byte { return gfExp[((int(gfLog[a])*p)%255+255)%255] }
func ginv(a byte) byte        { return gfExp[255-int(gfLog[a])] }

// GF polynomials, high-order coefficient first.
func pmul(p, q []byte) []byte {
	r := make([]byte, len(p)+len(q)-1)
	for j := range q {
		for i := range p {
			r[i+j] ^= gmul(p[i], q[j])
		}
	}
	return r
}
func padd(p, q []byte) []byte {
	n := len(p)
	if len(q) > n {
		n = len(q)
	}
	r := make([]byte, n)
	for i := range p {
		r[i+n-len(p)] = p[i]
	}
	for i := range q {
		r[i+n-len(q)] ^= q[i]
	}
	return r
}
func pscale(p []byte, s byte) []byte {
	r := make([]byte, len(p))
	for i := range p {
		r[i] = gmul(p[i], s)
	}
	return r
}
func peval(p []byte, x byte) byte {
	y := p[0]
	for i := 1; i < len(p); i++ {
		y = gmul(y, x) ^ p[i]
	}
	return y
}

// RSGeneratorPoly returns g(x) = prod (x - a^(fcr+i)), i in 0..nparity-1.
func RSGeneratorPoly(nparity int) []byte {
	g := []byte{1}
	for i := 0; i < nparity; i++ {
		g = pmul(g, []byte{1, gpow(gfGen, fcr+i)})
	}
	return g
}

// RSEncode returns the systematic codeword message ++ nparity parity bytes.
func RSEncode(msg []byte, nparity int) []byte {
	gen := RSGeneratorPoly(nparity)
	out := make([]byte, len(msg)+nparity)
	copy(out, msg)
	for i := 0; i < len(msg); i++ {
		coef := out[i]
		if coef != 0 {
			for j := 1; j < len(gen); j++ {
				out[i+j] ^= gmul(gen[j], coef)
			}
		}
	}
	code := make([]byte, len(msg)+nparity)
	copy(code, msg)
	copy(code[len(msg):], out[len(msg):])
	return code
}

func syndromes(cw []byte, nparity int) []byte {
	s := make([]byte, nparity+1) // leading 0 (Forney convention)
	for i := 0; i < nparity; i++ {
		s[i+1] = peval(cw, gpow(gfGen, fcr+i))
	}
	return s
}

func errLocator(synd []byte, nparity int) []byte {
	errLoc := []byte{1}
	oldLoc := []byte{1}
	for i := 0; i < nparity; i++ {
		delta := synd[i+1]
		for j := 1; j < len(errLoc); j++ {
			delta ^= gmul(errLoc[len(errLoc)-1-j], synd[i+1-j])
		}
		oldLoc = append(oldLoc, 0)
		if delta != 0 {
			if len(oldLoc) > len(errLoc) {
				newLoc := pscale(oldLoc, delta)
				oldLoc = pscale(errLoc, ginv(delta))
				errLoc = newLoc
			}
			errLoc = padd(errLoc, pscale(oldLoc, delta))
		}
	}
	for len(errLoc) > 0 && errLoc[0] == 0 {
		errLoc = errLoc[1:]
	}
	return errLoc
}

func errPositions(errLocRev []byte, nmess int) ([]int, bool) {
	errs := len(errLocRev) - 1
	var pos []int
	for i := 0; i < nmess; i++ {
		if peval(errLocRev, gpow(gfGen, i)) == 0 {
			pos = append(pos, nmess-1-i)
		}
	}
	if len(pos) != errs {
		return nil, false
	}
	return pos, true
}

func correctErrata(cw, synd []byte, errPos []int) bool {
	n := len(cw)
	coefPos := make([]int, len(errPos))
	for i, p := range errPos {
		coefPos[i] = n - 1 - p
	}
	eLoc := []byte{1}
	X := make([]byte, 0, len(coefPos))
	for _, p := range coefPos {
		eLoc = pmul(eLoc, []byte{gpow(gfGen, p), 1})
		X = append(X, gpow(gfGen, p))
	}
	syndRev := make([]byte, len(synd))
	for i := range synd {
		syndRev[i] = synd[len(synd)-1-i]
	}
	prod := pmul(syndRev, eLoc)
	rem := prod[len(prod)-len(eLoc):]
	for i := 0; i < len(X); i++ {
		Xi := X[i]
		XiInv := ginv(Xi)
		denom := byte(1)
		for j := 0; j < len(X); j++ {
			if j != i {
				denom = gmul(denom, 1^gmul(XiInv, X[j]))
			}
		}
		numer := peval(rem, XiInv)
		numer = gmul(numer, gpow(Xi, 1-fcr))
		if denom == 0 {
			return false
		}
		cw[errPos[i]] ^= gdiv(numer, denom)
	}
	return true
}

// RSDecode returns the message (first msglen bytes) and the corrected-byte count,
// or an error if the codeword has more errors than the code can correct. msglen < 0
// means len(codeword) - nparity.
func RSDecode(codeword []byte, nparity, msglen int) ([]byte, int, error) {
	cw := make([]byte, len(codeword))
	copy(cw, codeword)
	if msglen < 0 {
		msglen = len(cw) - nparity
	}
	synd := syndromes(cw, nparity)
	allZero := true
	for _, s := range synd {
		if s != 0 {
			allZero = false
			break
		}
	}
	if allZero {
		return append([]byte(nil), cw[:msglen]...), 0, nil
	}
	errLoc := errLocator(synd, nparity)
	if len(errLoc)-1 > nparity/2 {
		return nil, 0, errors.New("fec: too many errors")
	}
	errLocRev := make([]byte, len(errLoc))
	for i := range errLoc {
		errLocRev[i] = errLoc[len(errLoc)-1-i]
	}
	pos, ok := errPositions(errLocRev, len(cw))
	if !ok {
		return nil, 0, errors.New("fec: error location failed")
	}
	if !correctErrata(cw, synd, pos) {
		return nil, 0, errors.New("fec: forney failed")
	}
	for _, s := range syndromes(cw, nparity) {
		if s != 0 {
			return nil, 0, errors.New("fec: residual syndrome")
		}
	}
	return append([]byte(nil), cw[:msglen]...), len(pos), nil
}

// Interleave writes equal-length codewords column-major (burst spreading).
func Interleave(codewords [][]byte) []byte {
	if len(codewords) == 0 {
		return nil
	}
	d, n := len(codewords), len(codewords[0])
	out := make([]byte, d*n)
	for r := 0; r < d; r++ {
		for c := 0; c < n; c++ {
			out[c*d+r] = codewords[r][c]
		}
	}
	return out
}

// Deinterleave is the inverse of Interleave.
func Deinterleave(stream []byte, depth, n int) [][]byte {
	cws := make([][]byte, depth)
	for r := range cws {
		cws[r] = make([]byte, n)
	}
	for c := 0; c < n; c++ {
		for r := 0; r < depth; r++ {
			cws[r][c] = stream[c*depth+r]
		}
	}
	return cws
}

func chunking(l, np int) (int, int) {
	maxk := 255 - np
	nchunks := 1
	k := 1
	if l > 0 {
		nchunks = (l + maxk - 1) / maxk
		k = (l + nchunks - 1) / nchunks
	}
	return nchunks, k
}

// EncodeMessage encodes any-length payload into a self-describing RS+interleave blob.
func EncodeMessage(msg []byte, nparity int) []byte {
	l := len(msg)
	nchunks, k := chunking(l, nparity)
	hdr := []byte{byte(l >> 24), byte(l >> 16), byte(l >> 8), byte(l), byte(nparity)}
	out := RSEncode(hdr, HdrParity)
	cws := make([][]byte, nchunks)
	for c := 0; c < nchunks; c++ {
		block := make([]byte, k)
		for i := 0; i < k; i++ {
			if o := c*k + i; o < l {
				block[i] = msg[o]
			}
		}
		cws[c] = RSEncode(block, nparity)
	}
	return append(out, Interleave(cws)...)
}

// DecodeMessage inverts EncodeMessage, returning the message and total corrected bytes.
func DecodeMessage(blob []byte) ([]byte, int, error) {
	if len(blob) < hdrLen {
		return nil, 0, errors.New("fec: short blob")
	}
	hdr, _, err := RSDecode(blob[:hdrLen], HdrParity, 5)
	if err != nil {
		return nil, 0, err
	}
	l := int(hdr[0])<<24 | int(hdr[1])<<16 | int(hdr[2])<<8 | int(hdr[3])
	np := int(hdr[4])
	nchunks, k := chunking(l, np)
	cwlen := k + np
	body := blob[hdrLen:]
	if len(body) != nchunks*cwlen {
		return nil, 0, errors.New("fec: blob length mismatch")
	}
	out := make([]byte, 0, nchunks*k)
	total := 0
	for _, cw := range Deinterleave(body, nchunks, cwlen) {
		block, n, err := RSDecode(cw, np, k)
		if err != nil {
			return nil, 0, err
		}
		total += n
		out = append(out, block...)
	}
	if l < len(out) {
		out = out[:l]
	}
	return out, total, nil
}
