// SPDX-License-Identifier: LGPL-3.0-only

package dcf

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"os"
	"testing"
)

// goldenVectors mirrors the relevant shape of Documentation/golden_vectors.json.
type goldenVectors struct {
	Anchors struct {
		Crc123456789     string `json:"crc_123456789"`
		CrcZero15        string `json:"crc_zero15"`
		ExampleFrameFull string `json:"exampleFrame_full"`
	} `json:"anchors"`
	EncodeBasis []struct {
		Frame string `json:"frame"`
	} `json:"encode_basis"`
	SyndromeBasis []struct {
		Word     *string `json:"word"`
		Bit      *int    `json:"bit"`
		Syndrome uint64  `json:"syndrome"`
	} `json:"syndrome_basis"`
}

func loadGolden(t *testing.T) goldenVectors {
	t.Helper()
	paths := []string{
		"../../Documentation/golden_vectors.json",
		"../../python/MCP/golden_vectors.json",
	}
	var lastErr error
	for _, p := range paths {
		data, err := os.ReadFile(p)
		if err != nil {
			lastErr = err
			continue
		}
		var gv goldenVectors
		if err := json.Unmarshal(data, &gv); err != nil {
			t.Fatalf("parse %s: %v", p, err)
		}
		return gv
	}
	t.Fatalf("golden_vectors.json not found in expected locations: %v", lastErr)
	return goldenVectors{}
}

func TestCRCAnchors(t *testing.T) {
	if c := Crc16CCITT([]byte("123456789")); c != 0x29B1 {
		t.Errorf("CRC(\"123456789\") = 0x%04X, want 0x29B1", c)
	}
	if c := Crc16CCITT(make([]byte, 15)); c != 0x4EC3 {
		t.Errorf("CRC(0^15) = 0x%04X, want 0x4EC3", c)
	}
}

func TestExampleFrameAnchor(t *testing.T) {
	gv := loadGolden(t)
	ex := Frame{Version: 1, Type: FCtrl, Seq: 0x1234, Src: 1, Dst: Broadcast,
		Payload: [4]byte{0xDE, 0xAD, 0xBE, 0xEF}, TsUs: 0xAB12CD}
	got := ex.Encode()
	if h := hex.EncodeToString(got[:]); h != gv.Anchors.ExampleFrameFull {
		t.Errorf("exampleFrame = %s, want %s", h, gv.Anchors.ExampleFrameFull)
	}
}

func rawCRCValid(b []byte) bool {
	if len(b) != FrameSize || b[0] != SyncByte {
		return false
	}
	stored := uint16(b[15])<<8 | uint16(b[16])
	return Crc16CCITT(b[:crcCover]) == stored
}

// TestEncodeBasis: every encode_basis frame is a raw-CRC-valid 17-byte word, and every
// known frame type (0..3) decodes and re-encodes byte-identically. Reserved types (>3)
// are valid wire frames with no decoded form — CRC validity alone is asserted.
func TestEncodeBasis(t *testing.T) {
	gv := loadGolden(t)
	if len(gv.EncodeBasis) == 0 {
		t.Fatal("no encode_basis vectors")
	}
	for i, v := range gv.EncodeBasis {
		raw, err := hex.DecodeString(v.Frame)
		if err != nil {
			t.Fatalf("encode_basis[%d]: bad hex %q: %v", i, v.Frame, err)
		}
		if !rawCRCValid(raw) {
			t.Errorf("encode_basis[%d]: raw CRC invalid", i)
			continue
		}
		if raw[1]&0x0F <= 3 {
			f, err := Decode(raw)
			if err != nil {
				t.Errorf("encode_basis[%d]: decode failed: %v", i, err)
				continue
			}
			re := f.Encode()
			if !bytes.Equal(re[:], raw) {
				t.Errorf("encode_basis[%d]: roundtrip mismatch\n got  %x\n want %x", i, re[:], raw)
			}
		}
	}
}

// TestSyndromeBasis reproduces each basis word (all-zero, or a single set bit) and
// checks its syndrome against the certificate.
func TestSyndromeBasis(t *testing.T) {
	gv := loadGolden(t)
	if len(gv.SyndromeBasis) == 0 {
		t.Fatal("no syndrome_basis vectors")
	}
	for i, v := range gv.SyndromeBasis {
		word := make([]byte, FrameSize)
		switch {
		case v.Bit != nil:
			bit := *v.Bit
			word[bit/8] = 1 << (7 - uint(bit%8))
		case v.Word != nil:
			raw, err := hex.DecodeString(*v.Word)
			if err != nil || len(raw) != FrameSize {
				t.Fatalf("syndrome_basis[%d]: bad word", i)
			}
			word = raw
		}
		got, err := Syndrome(word)
		if err != nil {
			t.Fatalf("syndrome_basis[%d]: %v", i, err)
		}
		if uint64(got) != v.Syndrome {
			t.Errorf("syndrome_basis[%d]: got 0x%04X, want 0x%04X", i, got, v.Syndrome)
		}
	}
}
