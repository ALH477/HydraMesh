// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// The DeMoD 17-byte DeModFrame wire quantum (version 1), byte-identical to the
// reference codec in python/MCP/wirelab_core.py and certified against
// Documentation/golden_vectors.json (the cross-language contract).
//
// Wire layout (big-endian):
//   [0]     sync = 0xD3
//   [1]     flags: version[7:4]=1 | frame_type[3:0]
//   [2:4]   seq      u16
//   [4:6]   src      u16
//   [6:8]   dst      u16
//   [8:12]  payload  4 bytes
//   [12:15] ts_us    u24
//   [15:17] CRC-16/CCITT-FALSE over bytes [0..14]

const SYNC = 0xD3;
const VERSION = 1;
const FRAME_SIZE = 17;
const CRC_COVER = 15;
const BROADCAST = 0xFFFF;

/** CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF, no reflection, no xorout). */
function crc16(data, len = data.length) {
  let crc = 0xFFFF;
  for (let k = 0; k < len; k++) {
    crc ^= (data[k] & 0xFF) << 8;
    for (let i = 0; i < 8; i++) {
      crc = (crc & 0x8000) ? ((crc << 1) ^ 0x1021) & 0xFFFF : (crc << 1) & 0xFFFF;
    }
  }
  return crc & 0xFFFF;
}

/** Serialise a frame object into a 17-byte Buffer, computing and appending the CRC. */
function encode({ version = 1, type = 0, seq = 0, src = 0, dst = 0, payload, tsUs = 0 }) {
  const pay = Buffer.from(payload);
  if (pay.length !== 4) throw new Error('payload must be exactly 4 bytes');
  const b = Buffer.alloc(FRAME_SIZE);
  b[0] = SYNC;
  b[1] = ((version & 0x0F) << 4) | (type & 0x0F);
  b.writeUInt16BE(seq & 0xFFFF, 2);
  b.writeUInt16BE(src & 0xFFFF, 4);
  b.writeUInt16BE(dst & 0xFFFF, 6);
  pay.copy(b, 8, 0, 4);
  b[12] = (tsUs >>> 16) & 0xFF;
  b[13] = (tsUs >>> 8) & 0xFF;
  b[14] = tsUs & 0xFF;
  b.writeUInt16BE(crc16(b, CRC_COVER), 15);
  return b;
}

/** Affine validity syndrome of a 17-byte word: CRC-valid iff this returns 0. */
function syndrome(word) {
  if (word.length !== FRAME_SIZE) throw new Error('need 17 bytes');
  const stored = ((word[15] & 0xFF) << 8) | (word[16] & 0xFF);
  return (crc16(word, CRC_COVER) ^ stored) & 0xFFFF;
}

/** Parse a 17-byte buffer, validating sync, version nibble, and CRC. */
function decode(word) {
  if (word.length !== FRAME_SIZE) throw new Error('length != 17');
  if ((word[0] & 0xFF) !== SYNC) throw new Error('bad sync byte');
  if (((word[1] & 0xFF) >>> 4) !== VERSION) throw new Error('bad version nibble');
  if (syndrome(word) !== 0) throw new Error('CRC mismatch');
  return {
    version: (word[1] & 0xFF) >>> 4,
    type: word[1] & 0x0F,
    seq: ((word[2] & 0xFF) << 8) | (word[3] & 0xFF),
    src: ((word[4] & 0xFF) << 8) | (word[5] & 0xFF),
    dst: ((word[6] & 0xFF) << 8) | (word[7] & 0xFF),
    payload: Buffer.from([word[8], word[9], word[10], word[11]]),
    tsUs: ((word[12] & 0xFF) << 16) | ((word[13] & 0xFF) << 8) | (word[14] & 0xFF),
  };
}

const EXAMPLE_FRAME_FULL = 'd31312340001ffffdeadbeefab12cd24c0';

// Self-certify on module load: throw (refuse to load) if the codec has diverged
// from the reference (CRC + example-frame anchors).
(function selfCert() {
  const c1 = crc16(Buffer.from('123456789', 'ascii'));
  if (c1 !== 0x29B1) throw new Error(`dcf: CRC anchor diverged: CRC("123456789")=0x${c1.toString(16)}`);
  const c0 = crc16(Buffer.alloc(15));
  if (c0 !== 0x4EC3) throw new Error(`dcf: CRC anchor diverged: CRC(0^15)=0x${c0.toString(16)}`);
  const got = encode({
    version: 1, type: 3, seq: 0x1234, src: 1, dst: BROADCAST,
    payload: Buffer.from([0xDE, 0xAD, 0xBE, 0xEF]), tsUs: 0xAB12CD,
  }).toString('hex');
  if (got !== EXAMPLE_FRAME_FULL) throw new Error(`dcf: exampleFrame anchor diverged: ${got}`);
})();

module.exports = { SYNC, VERSION, FRAME_SIZE, CRC_COVER, BROADCAST, crc16, encode, decode, syndrome };
