// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// Certifies src/superpack.js byte-for-byte against the cross-language golden
// vectors in Documentation/superpack_vectors.json. Dependency-free.
//   node JS/nodejs/test/certify_superpack.js   (from the repo root)

const fs = require('fs');
const path = require('path');
const { pack, unpack, isSuperpack, SUPER_LEN } = require('../src/superpack.js');
const { encode } = require('../src/frame.js');

function loadVectors() {
  const candidates = [
    path.join(__dirname, '..', '..', '..', 'Documentation', 'superpack_vectors.json'),
    path.join(__dirname, '..', '..', '..', 'python', 'MCP', 'superpack_vectors.json'),
  ];
  for (const p of candidates) {
    if (fs.existsSync(p)) return JSON.parse(fs.readFileSync(p, 'utf8'));
  }
  throw new Error('superpack_vectors.json not found (run gen_superpack_vectors.py)');
}

let fails = 0;
function check(cond, label) {
  console.log((cond ? '  PASS  ' : '  FAIL  ') + label);
  if (!cond) fails += 1;
}

const sv = loadVectors();

// 1. pack(A, B) matches the golden 32-byte container
let packOk = true;
for (const c of sv.cases) {
  const out = pack(Buffer.from(c.a, 'hex'), Buffer.from(c.b, 'hex'));
  if (out.length !== SUPER_LEN || !isSuperpack(out) || out.toString('hex') !== c.super) packOk = false;
}
check(packOk, `${sv.cases.length} SuperPack pairs pack byte-identically`);

// 2. unpack(S) reconstructs both frames bit-exact
let unpackOk = true;
for (const c of sv.cases) {
  const [a, b] = unpack(Buffer.from(c.super, 'hex'));
  if (a.toString('hex') !== c.a || b.toString('hex') !== c.b) unpackOk = false;
}
check(unpackOk, `${sv.cases.length} SuperPack pairs unpack losslessly`);

// 3. joint CRC is tamper-evident on case 0
{
  const sp = Buffer.from(sv.cases[0].super, 'hex');
  let rejected = 0;
  for (let i = 0; i < SUPER_LEN; i++) {
    const bad = Buffer.from(sp);
    bad[i] ^= 0x01;
    try { unpack(bad); } catch (e) { rejected += 1; }
  }
  check(rejected === SUPER_LEN, 'every single-bit flip rejected');
}

// 4. zero-core anchor: SuperPack of two all-zero-core frames has joint CRC 0x5B75
{
  const zero = encode({ version: 1, type: 0, payload: Buffer.alloc(4) });
  const spz = pack(zero, zero);
  const joint = spz.readUInt16BE(30);
  check(joint === 0x5B75 && joint === sv.anchors.zero_core_joint_crc, `zero-core joint CRC = 0x${joint.toString(16)} (want 0x5b75)`);
}

if (fails) { console.error(`\n${fails} CHECK(S) FAILED`); process.exit(1); }
console.log('\nALL SUPERPACK VECTORS HOLD — Node.js DCF SuperPack is cemented.');
