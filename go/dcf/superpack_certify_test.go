// SPDX-License-Identifier: LGPL-3.0-only

package dcf

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"os"
	"testing"
)

// superVectors mirrors Documentation/superpack_vectors.json.
type superVectors struct {
	SuperLen int `json:"super_len"`
	Anchors  struct {
		ZeroCoreJointCRC int `json:"zero_core_joint_crc"`
	} `json:"anchors"`
	Cases []struct {
		A     string `json:"a"`
		B     string `json:"b"`
		Super string `json:"super"`
	} `json:"cases"`
}

func loadSuper(t *testing.T) superVectors {
	t.Helper()
	paths := []string{
		"../../Documentation/superpack_vectors.json",
		"../../python/MCP/superpack_vectors.json",
	}
	var lastErr error
	for _, p := range paths {
		data, err := os.ReadFile(p)
		if err != nil {
			lastErr = err
			continue
		}
		var sv superVectors
		if err := json.Unmarshal(data, &sv); err != nil {
			t.Fatalf("parse %s: %v", p, err)
		}
		return sv
	}
	t.Fatalf("superpack_vectors.json not found (run gen_superpack_vectors.py): %v", lastErr)
	return superVectors{}
}

func mustHex(t *testing.T, s string) []byte {
	t.Helper()
	b, err := hex.DecodeString(s)
	if err != nil {
		t.Fatalf("bad hex %q: %v", s, err)
	}
	return b
}

func TestSuperPackMatchesGolden(t *testing.T) {
	sv := loadSuper(t)
	for i, c := range sv.Cases {
		a, b := mustHex(t, c.A), mustHex(t, c.B)
		out, err := PackSuper(a, b)
		if err != nil {
			t.Fatalf("case[%d] pack: %v", i, err)
		}
		if len(out) != sv.SuperLen {
			t.Fatalf("case[%d] length %d != %d", i, len(out), sv.SuperLen)
		}
		if !IsSuperPack(out[:]) {
			t.Fatalf("case[%d] not recognised as SuperPack", i)
		}
		if got := hex.EncodeToString(out[:]); got != c.Super {
			t.Fatalf("case[%d] pack bytes\n got %s\nwant %s", i, got, c.Super)
		}
	}
	t.Logf("%d SuperPack pairs pack byte-identically", len(sv.Cases))
}

func TestSuperPackUnpackLossless(t *testing.T) {
	sv := loadSuper(t)
	for i, c := range sv.Cases {
		a, b, err := UnpackSuper(mustHex(t, c.Super))
		if err != nil {
			t.Fatalf("case[%d] unpack: %v", i, err)
		}
		if !bytes.Equal(a[:], mustHex(t, c.A)) {
			t.Fatalf("case[%d] frame A mismatch", i)
		}
		if !bytes.Equal(b[:], mustHex(t, c.B)) {
			t.Fatalf("case[%d] frame B mismatch", i)
		}
	}
	t.Logf("%d SuperPack pairs unpack losslessly", len(sv.Cases))
}

func TestSuperPackTamperEvident(t *testing.T) {
	sv := loadSuper(t)
	sp := mustHex(t, sv.Cases[0].Super)
	for i := range sp {
		bad := append([]byte(nil), sp...)
		bad[i] ^= 0x01
		if _, _, err := UnpackSuper(bad); err == nil {
			t.Fatalf("tamper at byte %d not detected", i)
		}
	}
	// zero-core anchor: SuperPack of two all-zero-core frames has joint CRC 0x5B75.
	zero := Frame{Version: 1, Type: FData}.Encode()
	spz, err := PackSuper(zero[:], zero[:])
	if err != nil {
		t.Fatalf("pack(zero,zero): %v", err)
	}
	joint := int(spz[30])<<8 | int(spz[31])
	if joint != sv.Anchors.ZeroCoreJointCRC || joint != 0x5B75 {
		t.Fatalf("zero-core joint CRC drift: 0x%04X", joint)
	}
	t.Log("joint CRC tamper-evident; zero-core anchor = 0x5B75")
}
