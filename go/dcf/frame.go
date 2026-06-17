// SPDX-License-Identifier: LGPL-3.0-only

// Package dcf implements the DeMoD 17-byte DeModFrame wire quantum (version 1),
// byte-identical to the reference codec in python/MCP/wirelab_core.py and certified
// against Documentation/golden_vectors.json (the cross-language contract).
//
// Wire layout (big-endian):
//
//	[0]     sync = 0xD3
//	[1]     flags: version[7:4]=1 | frame_type[3:0]
//	[2:4]   seq      u16
//	[4:6]   src      u16
//	[6:8]   dst      u16
//	[8:12]  payload  4 bytes
//	[12:15] ts_us    u24
//	[15:17] CRC-16/CCITT-FALSE over bytes [0..14]
package dcf

import (
	"encoding/hex"
	"errors"
	"fmt"
)

// Wire constants.
const (
	SyncByte  = 0xD3
	Version   = 1
	FrameSize = 17
	crcCover  = 15
	Broadcast = 0xFFFF
)

// FrameType is the 4-bit frame-type nibble.
type FrameType uint8

const (
	FData   FrameType = 0
	FAck    FrameType = 1
	FBeacon FrameType = 2
	FCtrl   FrameType = 3
)

// Decode errors.
var (
	ErrBadLength  = errors.New("dcf: frame must be 17 bytes")
	ErrBadSync    = errors.New("dcf: bad sync byte")
	ErrBadVersion = errors.New("dcf: bad version nibble")
	ErrBadCRC     = errors.New("dcf: CRC mismatch")
)

// Crc16CCITT computes CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF, no reflection,
// no xorout) over data.
func Crc16CCITT(data []byte) uint16 {
	crc := uint16(0xFFFF)
	for _, b := range data {
		crc ^= uint16(b) << 8
		for i := 0; i < 8; i++ {
			if crc&0x8000 != 0 {
				crc = (crc << 1) ^ 0x1021
			} else {
				crc <<= 1
			}
		}
	}
	return crc
}

// Frame is a decoded DeModFrame with host-order fields.
type Frame struct {
	Version uint8
	Type    FrameType
	Seq     uint16
	Src     uint16
	Dst     uint16
	Payload [4]byte
	TsUs    uint32 // 24-bit; the high byte is masked off on encode
}

// Encode serialises f into exactly 17 bytes, computing and appending the CRC.
func (f Frame) Encode() [FrameSize]byte {
	var b [FrameSize]byte
	b[0] = SyncByte
	b[1] = ((f.Version & 0x0F) << 4) | (byte(f.Type) & 0x0F)
	b[2] = byte(f.Seq >> 8)
	b[3] = byte(f.Seq)
	b[4] = byte(f.Src >> 8)
	b[5] = byte(f.Src)
	b[6] = byte(f.Dst >> 8)
	b[7] = byte(f.Dst)
	copy(b[8:12], f.Payload[:])
	b[12] = byte((f.TsUs >> 16) & 0xFF)
	b[13] = byte((f.TsUs >> 8) & 0xFF)
	b[14] = byte(f.TsUs & 0xFF)
	crc := Crc16CCITT(b[:crcCover])
	b[15] = byte(crc >> 8)
	b[16] = byte(crc)
	return b
}

// Syndrome returns the affine validity syndrome of a 17-byte word: the word's CRC is
// valid iff Syndrome == 0.
func Syndrome(word []byte) (uint16, error) {
	if len(word) != FrameSize {
		return 0, ErrBadLength
	}
	stored := uint16(word[15])<<8 | uint16(word[16])
	return Crc16CCITT(word[:crcCover]) ^ stored, nil
}

// Decode parses a 17-byte wire buffer, validating sync, version nibble, and CRC.
func Decode(word []byte) (Frame, error) {
	if len(word) != FrameSize {
		return Frame{}, ErrBadLength
	}
	if word[0] != SyncByte {
		return Frame{}, ErrBadSync
	}
	if word[1]>>4 != Version {
		return Frame{}, ErrBadVersion
	}
	if syn, _ := Syndrome(word); syn != 0 {
		return Frame{}, ErrBadCRC
	}
	return Frame{
		Version: word[1] >> 4,
		Type:    FrameType(word[1] & 0x0F),
		Seq:     uint16(word[2])<<8 | uint16(word[3]),
		Src:     uint16(word[4])<<8 | uint16(word[5]),
		Dst:     uint16(word[6])<<8 | uint16(word[7]),
		Payload: [4]byte{word[8], word[9], word[10], word[11]},
		TsUs:    uint32(word[12])<<16 | uint32(word[13])<<8 | uint32(word[14]),
	}, nil
}

// exampleFrameFull is the golden anchor frame (Ctrl, seq=0x1234, src=1, dst=broadcast,
// payload=DEADBEEF, ts=0xAB12CD) from Documentation/golden_vectors.json.
const exampleFrameFull = "d31312340001ffffdeadbeefab12cd24c0"

// init self-certifies the codec against the CRC and example-frame anchors on import,
// panicking (refuse to load) if the implementation has diverged from the reference.
func init() {
	if c := Crc16CCITT([]byte("123456789")); c != 0x29B1 {
		panic(fmt.Sprintf("dcf: CRC anchor diverged: CRC(\"123456789\")=0x%04X, want 0x29B1", c))
	}
	if c := Crc16CCITT(make([]byte, 15)); c != 0x4EC3 {
		panic(fmt.Sprintf("dcf: CRC anchor diverged: CRC(0^15)=0x%04X, want 0x4EC3", c))
	}
	ex := Frame{Version: 1, Type: FCtrl, Seq: 0x1234, Src: 1, Dst: Broadcast,
		Payload: [4]byte{0xDE, 0xAD, 0xBE, 0xEF}, TsUs: 0xAB12CD}
	got := ex.Encode()
	if hex.EncodeToString(got[:]) != exampleFrameFull {
		panic(fmt.Sprintf("dcf: exampleFrame anchor diverged: got %s, want %s",
			hex.EncodeToString(got[:]), exampleFrameFull))
	}
}
