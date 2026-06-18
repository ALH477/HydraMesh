// SPDX-License-Identifier: LGPL-3.0-only

package dcf

// SuperPack is a container adapter over the 17-byte DeModFrame quantum: it carries
// TWO frames in ONE 32-byte message under a single joint CRC-16, and reconstructs
// both inner frames bit-exact on unpack.
//
// Two raw frames cost 2*17 = 34 bytes. When frames are emitted in pairs the second
// header is largely recoverable from context (both inner sync bytes are 0xD3; each
// inner CRC is a pure function of its own 15 leading bytes). SuperPack drops those 6
// redundant bytes and spends 4 back on one outer sync, a type/version tag, and ONE
// joint CRC over the whole container — net 34 -> 32 bytes plus stronger integrity.
//
// Why it is the lower-latency option: a SuperPack puts a frame pair on the wire as a
// single datagram instead of two — one packet, one IP/UDP header, one syscall — so
// paired traffic crosses the network with strictly lower per-pair overhead and
// latency than emitting the two frames separately.
//
// Because the unpacked frames are ordinary valid DeModFrames, the 246-vector wire
// certificate is untouched. Layout (big-endian, 32 bytes):
//
//	[0]      sync   = 0xD3
//	[1]      sflags = version[7:4]=1 | type[3:0]=SUPER (0x5)  => 0x15
//	[2:16]   frame A core = A's bytes [1:15]
//	[16:30]  frame B core = B's bytes [1:15]
//	[30:32]  CRC-16/CCITT-FALSE over bytes [0:30]

import "errors"

// SuperPack constants.
const (
	SuperType    = 0x05
	SuperSize    = 32
	SuperCoreLen = 14 // a frame's bytes [1:15]: everything but sync and CRC
	superSFlags  = (Version << 4) | SuperType
)

// SuperPack errors.
var (
	ErrSuperLength  = errors.New("dcf: superpack must be 32 bytes")
	ErrSuperSync    = errors.New("dcf: superpack bad sync byte")
	ErrSuperVersion = errors.New("dcf: superpack bad version nibble")
	ErrSuperType    = errors.New("dcf: not a superpack type")
	ErrSuperCRC     = errors.New("dcf: superpack CRC mismatch")
	ErrSuperInner   = errors.New("dcf: superpack inner frame invalid")
)

// frameCore returns the 14 reconstructable bytes of a 17-byte frame, validating
// sync, version nibble, and the inner CRC so a corrupt frame is never packed.
func frameCore(frame []byte) ([]byte, error) {
	if len(frame) != FrameSize {
		return nil, ErrBadLength
	}
	if frame[0] != SyncByte {
		return nil, ErrSuperSync
	}
	if frame[1]>>4 != Version {
		return nil, ErrSuperVersion
	}
	stored := uint16(frame[15])<<8 | uint16(frame[16])
	if Crc16CCITT(frame[:crcCover]) != stored {
		return nil, ErrSuperInner
	}
	core := make([]byte, SuperCoreLen)
	copy(core, frame[1:15])
	return core, nil
}

// rebuildFrame restores a full 17-byte DeModFrame from its 14-byte core.
func rebuildFrame(core []byte) [FrameSize]byte {
	var f [FrameSize]byte
	f[0] = SyncByte
	copy(f[1:15], core)
	crc := Crc16CCITT(f[:crcCover])
	f[15] = byte(crc >> 8)
	f[16] = byte(crc)
	return f
}

// PackSuper combines two valid 17-byte frames into one 32-byte SuperPack.
func PackSuper(a, b []byte) ([SuperSize]byte, error) {
	var out [SuperSize]byte
	coreA, err := frameCore(a)
	if err != nil {
		return out, err
	}
	coreB, err := frameCore(b)
	if err != nil {
		return out, err
	}
	out[0] = SyncByte
	out[1] = superSFlags
	copy(out[2:2+SuperCoreLen], coreA)
	copy(out[2+SuperCoreLen:2+2*SuperCoreLen], coreB)
	crc := Crc16CCITT(out[:30])
	out[30] = byte(crc >> 8)
	out[31] = byte(crc)
	return out, nil
}

// IsSuperPack reports whether buf looks like a SuperPack (length + sync + tag).
func IsSuperPack(buf []byte) bool {
	return len(buf) == SuperSize && buf[0] == SyncByte && buf[1] == superSFlags
}

// UnpackSuper splits a 32-byte SuperPack back into two bit-exact 17-byte frames.
func UnpackSuper(buf []byte) (a, b [FrameSize]byte, err error) {
	if len(buf) != SuperSize {
		return a, b, ErrSuperLength
	}
	if buf[0] != SyncByte {
		return a, b, ErrSuperSync
	}
	if buf[1]>>4 != Version {
		return a, b, ErrSuperVersion
	}
	if buf[1]&0x0F != SuperType {
		return a, b, ErrSuperType
	}
	stored := uint16(buf[30])<<8 | uint16(buf[31])
	if Crc16CCITT(buf[:30]) != stored {
		return a, b, ErrSuperCRC
	}
	a = rebuildFrame(buf[2 : 2+SuperCoreLen])
	b = rebuildFrame(buf[2+SuperCoreLen : 2+2*SuperCoreLen])
	// Belt and braces: the rebuilt frames must themselves decode cleanly.
	if _, e := Decode(a[:]); e != nil {
		return a, b, ErrSuperInner
	}
	if _, e := Decode(b[:]); e != nil {
		return a, b, ErrSuperInner
	}
	return a, b, nil
}
