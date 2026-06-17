// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// Certifies src/frame.js byte-for-byte against the cross-language golden vectors in
// Documentation/golden_vectors.json. Dependency-free — stdlib `fs`/`path` + JSON.parse.
//   node JS/nodejs/test/certify.js   (from the repo root)
//   npm --prefix JS/nodejs run certify

const fs = require('fs');
const path = require('path');
const { crc16, encode, decode, syndrome, FRAME_SIZE, SYNC, CRC_COVER, BROADCAST } = require('../src/frame.js');

function loadGolden() {
  const candidates = [
    path.join(__dirname, '..', '..', '..', 'Documentation', 'golden_vectors.json'),
    path.join(__dirname, '..', '..', '..', 'python', 'MCP', 'golden_vectors.json'),
  ];
  for (const p of candidates) {
    if (fs.existsSync(p)) return JSON.parse(fs.readFileSync(p, 'utf8'));
  }
  throw new Error('golden_vectors.json not found in expected locations');
}

let fails = 0;
function check(cond, label) {
  console.log((cond ? '  PASS  ' : '  FAIL  ') + label);
  if (!cond) fails += 1;
}

function rawValid(b) {
  if (b.length !== FRAME_SIZE || (b[0] & 0xFF) !== SYNC) return false;
  const stored = ((b[15] & 0xFF) << 8) | (b[16] & 0xFF);
  return crc16(b, CRC_COVER) === stored;
}

const gv = loadGolden();

// 1. CRC anchors
const c1 = crc16(Buffer.from('123456789', 'ascii'));
check(c1 === 0x29B1, `CRC("123456789") = 0x${c1.toString(16)} (want 0x29b1)`);
const c0 = crc16(Buffer.alloc(15));
check(c0 === 0x4EC3, `CRC(0^15) = 0x${c0.toString(16)} (want 0x4ec3)`);

// 2. example frame anchor
const exGot = encode({
  version: 1, type: 3, seq: 0x1234, src: 1, dst: BROADCAST,
  payload: Buffer.from([0xDE, 0xAD, 0xBE, 0xEF]), tsUs: 0xAB12CD,
}).toString('hex');
check(exGot === gv.anchors.exampleFrame_full.toLowerCase(), `exampleFrame_full: got ${exGot}`);

// 3. encode_basis: raw-CRC-valid + (known types) decode/roundtrip
check(gv.encode_basis.length > 0, 'encode_basis vectors present');
let encFails = 0;
gv.encode_basis.forEach((v, i) => {
  const raw = Buffer.from(v.frame, 'hex');
  if (!rawValid(raw)) {
    encFails += 1;
    console.log(`  FAIL  encode_basis[${i}]: raw CRC invalid`);
    return;
  }
  if ((raw[1] & 0x0F) <= 3) {
    try {
      const f = decode(raw);
      if (!encode(f).equals(raw)) {
        encFails += 1;
        console.log(`  FAIL  encode_basis[${i}]: roundtrip mismatch`);
      }
    } catch (e) {
      encFails += 1;
      console.log(`  FAIL  encode_basis[${i}]: ${e.message}`);
    }
  }
});
fails += encFails;
if (encFails === 0) console.log(`  PASS  ${gv.encode_basis.length} encode_basis vectors (decode + roundtrip)`);

// 4. syndrome_basis: reproduce each basis word and check its syndrome
check(gv.syndrome_basis.length > 0, 'syndrome_basis vectors present');
let synFails = 0;
gv.syndrome_basis.forEach((v, i) => {
  const word = Buffer.alloc(FRAME_SIZE);
  if (typeof v.bit === 'number') {
    word[Math.floor(v.bit / 8)] = 1 << (7 - (v.bit % 8));
  } else if (typeof v.word === 'string') {
    Buffer.from(v.word, 'hex').copy(word);
  }
  const got = syndrome(word);
  if (got !== v.syndrome) {
    synFails += 1;
    console.log(`  FAIL  syndrome_basis[${i}]: got 0x${got.toString(16)} want 0x${v.syndrome.toString(16)}`);
  }
});
fails += synFails;
if (synFails === 0) console.log(`  PASS  ${gv.syndrome_basis.length} syndrome_basis vectors`);

console.log('');
if (fails === 0) {
  console.log(`ALL CHECKS PASSED — Node.js codec cemented (${gv.encode_basis.length} encode + ${gv.syndrome_basis.length} syndrome).`);
  process.exit(0);
} else {
  console.log(`${fails} certification check(s) FAILED`);
  process.exit(1);
}
