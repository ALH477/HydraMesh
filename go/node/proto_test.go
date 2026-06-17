// SPDX-License-Identifier: LGPL-3.0-only

package node

import (
	"bytes"
	"encoding/hex"
	"testing"
)

// TestProtoMessageRoundTrip checks Deserialize(Serialize(m)) == m for a payload and the
// empty-payload edge case.
func TestProtoMessageRoundTrip(t *testing.T) {
	cases := []ProtoMessage{
		{MsgType: MsgGameDCF, Sequence: 7, Timestamp: 123456789, Payload: []byte{0xDE, 0xAD, 0xBE, 0xEF}},
		{MsgType: MsgPong, Sequence: 0, Timestamp: 0, Payload: nil},
		{MsgType: MsgTextDCF, Sequence: 0xFFFFFFFF, Timestamp: 0xFFFFFFFFFFFFFFFF, Payload: []byte("x")},
	}
	for i, m := range cases {
		got, err := Deserialize(m.Serialize())
		if err != nil {
			t.Fatalf("case %d: Deserialize: %v", i, err)
		}
		if got.MsgType != m.MsgType || got.Sequence != m.Sequence || got.Timestamp != m.Timestamp {
			t.Fatalf("case %d: header mismatch: got %+v want %+v", i, got, m)
		}
		if !bytes.Equal(got.Payload, m.Payload) && !(len(got.Payload) == 0 && len(m.Payload) == 0) {
			t.Fatalf("case %d: payload mismatch: got %x want %x", i, got.Payload, m.Payload)
		}
	}
}

// TestProtoMessageGoldenVector pins the big-endian wire layout against a hand-computed vector:
// type(1) | sequence(4) | timestamp(8) | payload_len(4) | payload(N), matching the Rust
// reference's ProtoMessage::serialize byte order exactly.
func TestProtoMessageGoldenVector(t *testing.T) {
	m := ProtoMessage{
		MsgType:   1,
		Sequence:  42,
		Timestamp: 0x0102030405060708,
		Payload:   []byte{1, 2, 3},
	}
	want := "010000002a010203040506070800000003010203"
	got := hex.EncodeToString(m.Serialize())
	if got != want {
		t.Fatalf("serialize golden mismatch:\n got %s\nwant %s", got, want)
	}
}

// TestDeserializeGuards rejects a short header and a payload-length field overrunning the
// buffer.
func TestDeserializeGuards(t *testing.T) {
	if _, err := Deserialize(make([]byte, 16)); err != ErrShortMessage {
		t.Fatalf("short header: got %v want ErrShortMessage", err)
	}
	// Valid 17-byte header claiming a 10-byte payload that isn't present.
	empty := ProtoMessage{MsgType: 1, Sequence: 1, Timestamp: 1, Payload: nil}
	hdr := empty.Serialize()
	hdr[16] = 10 // payload_len low byte = 10
	if _, err := Deserialize(hdr); err != ErrPayloadOverrun {
		t.Fatalf("overrun: got %v want ErrPayloadOverrun", err)
	}
}
