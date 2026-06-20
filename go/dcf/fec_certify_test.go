// SPDX-License-Identifier: LGPL-3.0-only

package dcf

// Go certification for the DCF FEC adapter — diffs the Go Reed-Solomon against the
// cross-language golden vectors (Documentation/fec_vectors.json). Passing this ==
// byte-agreement with the Python, C, and Rust references (encode + the multi-codeword
// message format) and the correction law.

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"os"
	"testing"
)

type fecVectors struct {
	NParity int `json:"nparity"`
	Cases   []struct {
		Msg  string `json:"msg"`
		Code string `json:"code"`
	} `json:"cases"`
	Correct []struct {
		Corrupt string `json:"corrupt"`
		Msg     string `json:"msg"`
		Nerr    int    `json:"nerr"`
	} `json:"correct"`
	Messages []struct {
		Len  int    `json:"len"`
		Msg  string `json:"msg"`
		Blob string `json:"blob"`
	} `json:"messages"`
	MessageBurst struct {
		Msg     string `json:"msg"`
		Corrupt string `json:"corrupt"`
	} `json:"message_burst"`
}

func loadFec(t *testing.T) fecVectors {
	t.Helper()
	for _, p := range []string{
		"../../Documentation/fec_vectors.json",
		"../../python/MCP/fec_vectors.json",
	} {
		data, err := os.ReadFile(p)
		if err != nil {
			continue
		}
		var v fecVectors
		if err := json.Unmarshal(data, &v); err != nil {
			t.Fatalf("parse %s: %v", p, err)
		}
		return v
	}
	t.Fatal("fec_vectors.json not found (run gen_fec_vectors.py)")
	return fecVectors{}
}

func mh(t *testing.T, s string) []byte {
	t.Helper()
	b, err := hex.DecodeString(s)
	if err != nil {
		t.Fatalf("bad hex: %v", err)
	}
	return b
}

func TestFECEncodeMatchesGolden(t *testing.T) {
	v := loadFec(t)
	for i, c := range v.Cases {
		code := RSEncode(mh(t, c.Msg), v.NParity)
		if got := hex.EncodeToString(code); got != c.Code {
			t.Fatalf("case[%d] encode\n got %s\nwant %s", i, got, c.Code)
		}
	}
	t.Logf("%d RS encode vectors byte-identical", len(v.Cases))
}

func TestFECDecodeCorrectsGolden(t *testing.T) {
	v := loadFec(t)
	for i, c := range v.Correct {
		msg, n, err := RSDecode(mh(t, c.Corrupt), v.NParity, 17)
		if err != nil {
			t.Fatalf("case[%d] decode: %v", i, err)
		}
		if !bytes.Equal(msg, mh(t, c.Msg)) {
			t.Fatalf("case[%d] did not recover message", i)
		}
		if n != c.Nerr {
			t.Fatalf("case[%d] corrected %d != %d", i, n, c.Nerr)
		}
	}
	t.Logf("%d corrupted codewords corrected", len(v.Correct))
}

func TestFECMessageMatchesGolden(t *testing.T) {
	v := loadFec(t)
	for i, m := range v.Messages {
		msg := mh(t, m.Msg)
		blob := EncodeMessage(msg, v.NParity)
		if got := hex.EncodeToString(blob); got != m.Blob {
			t.Fatalf("message[%d] len=%d blob mismatch\n got %s\nwant %s", i, m.Len, got, m.Blob)
		}
		out, _, err := DecodeMessage(blob)
		if err != nil || !bytes.Equal(out, msg) {
			t.Fatalf("message[%d] round-trip failed: %v", i, err)
		}
	}
	t.Logf("%d multi-codeword messages byte-identical + round-trip", len(v.Messages))
}

func TestFECMessageBurst(t *testing.T) {
	v := loadFec(t)
	out, n, err := DecodeMessage(mh(t, v.MessageBurst.Corrupt))
	if err != nil || !bytes.Equal(out, mh(t, v.MessageBurst.Msg)) {
		t.Fatalf("burst not corrected: %v", err)
	}
	if n == 0 {
		t.Fatal("expected corrections")
	}
	t.Logf("interleaved burst corrected (%d bytes)", n)
}

func TestFECInterleaveRoundtrip(t *testing.T) {
	cws := make([][]byte, 5)
	for i := range cws {
		m := bytes.Repeat([]byte{byte(i)}, 17)
		cws[i] = RSEncode(m, RSDefaultParity)
	}
	stream := Interleave(cws)
	back := Deinterleave(stream, 5, 17+RSDefaultParity)
	for i := range cws {
		if !bytes.Equal(cws[i], back[i]) {
			t.Fatalf("interleave roundtrip codeword %d", i)
		}
	}
}
