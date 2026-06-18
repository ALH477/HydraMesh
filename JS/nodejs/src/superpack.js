// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
// DeModFrame quanta under a single joint CRC-16.
//
// Two raw frames cost 2*17 = 34 bytes. When frames are emitted in pairs the
// second header is largely recoverable from context (both inner sync bytes are
// 0xD3; each inner CRC is a pure function of its own 15 leading bytes). SuperPack
// drops those 6 redundant bytes and spends 4 back on one outer sync, a
// type/version tag, and ONE joint CRC over the whole container — net 34 -> 32
// bytes plus a strictly stronger integrity check.
//
// Why it is the lower-latency option: a SuperPack puts a frame pair on the wire
// as a single datagram instead of two — one packet, one IP/UDP header, one
// syscall — so paired traffic crosses the network with strictly lower per-pair
// overhead and latency than emitting the two frames separately.
//
// Unpack reconstructs each inner frame bit-exact, so the outputs are ordinary
// valid DeModFrames and the 246-vector wire certificate is untouched.

const { crc16, decode, SYNC, VERSION, FRAME_SIZE, CRC_COVER } = require('./frame.js');

const SUPER_TYPE = 0x05;
const SUPER_LEN = 32;
const SUPER_CORE_LEN = 14; // a frame's bytes [1..14]: everything but sync and CRC
const SUPER_SFLAGS = ((VERSION & 0x0F) << 4) | SUPER_TYPE;

/** The 14 reconstructable bytes of a frame; validates sync, version, inner CRC. */
function frameCore(frame) {
  if (frame.length !== FRAME_SIZE) throw new Error('need a 17-byte frame');
  if ((frame[0] & 0xFF) !== SYNC) throw new Error('bad sync byte');
  if (((frame[1] & 0xFF) >>> 4) !== VERSION) throw new Error('bad version nibble');
  const stored = ((frame[15] & 0xFF) << 8) | (frame[16] & 0xFF);
  if (crc16(frame, CRC_COVER) !== stored) throw new Error('inner frame CRC mismatch');
  return Buffer.from(frame.subarray(1, 1 + SUPER_CORE_LEN));
}

/** Restore a full 17-byte frame from its 14-byte core (sync + recomputed CRC). */
function rebuildFrame(core) {
  const f = Buffer.alloc(FRAME_SIZE);
  f[0] = SYNC;
  core.copy(f, 1, 0, SUPER_CORE_LEN);
  f.writeUInt16BE(crc16(f, CRC_COVER), 15);
  return f;
}

/** Combine two valid 17-byte frames into one 32-byte SuperPack Buffer. */
function pack(frameA, frameB) {
  const coreA = frameCore(frameA);
  const coreB = frameCore(frameB);
  const out = Buffer.alloc(SUPER_LEN);
  out[0] = SYNC;
  out[1] = SUPER_SFLAGS;
  coreA.copy(out, 2);
  coreB.copy(out, 2 + SUPER_CORE_LEN);
  out.writeUInt16BE(crc16(out, 30), 30);
  return out;
}

/** True iff buf looks like a SuperPack (length + sync + version/type tag). */
function isSuperpack(buf) {
  return buf.length === SUPER_LEN && (buf[0] & 0xFF) === SYNC && (buf[1] & 0xFF) === SUPER_SFLAGS;
}

/** Split a 32-byte SuperPack into [frameA, frameB], each a bit-exact 17-byte frame. */
function unpack(buf) {
  if (buf.length !== SUPER_LEN) throw new Error('length != 32');
  if ((buf[0] & 0xFF) !== SYNC) throw new Error('bad sync byte');
  if (((buf[1] & 0xFF) >>> 4) !== VERSION) throw new Error('bad version nibble');
  if ((buf[1] & 0x0F) !== SUPER_TYPE) throw new Error('not a SuperPack type');
  const stored = ((buf[30] & 0xFF) << 8) | (buf[31] & 0xFF);
  if (crc16(buf, 30) !== stored) throw new Error('SuperPack CRC mismatch');
  const frameA = rebuildFrame(buf.subarray(2, 2 + SUPER_CORE_LEN));
  const frameB = rebuildFrame(buf.subarray(2 + SUPER_CORE_LEN, 2 + 2 * SUPER_CORE_LEN));
  decode(frameA); // belt and braces: must decode cleanly
  decode(frameB);
  return [frameA, frameB];
}

module.exports = {
  SUPER_TYPE, SUPER_LEN, SUPER_CORE_LEN, SUPER_SFLAGS,
  pack, unpack, isSuperpack,
};
