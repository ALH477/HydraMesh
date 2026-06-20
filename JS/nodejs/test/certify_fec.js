// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// Certifies src/fec.js byte-for-byte against the cross-language golden vectors in
// Documentation/fec_vectors.json (encode + the multi-codeword message format + the
// correction law). Dependency-free.
//   node JS/nodejs/test/certify_fec.js   (from the repo root)

const fs = require('fs');
const path = require('path');
const { rsEncode, rsDecode, encodeMessage, decodeMessage } = require('../src/fec.js');

function loadVectors() {
  const candidates = [
    path.join(__dirname, '..', '..', '..', 'Documentation', 'fec_vectors.json'),
    path.join(__dirname, '..', '..', '..', 'python', 'MCP', 'fec_vectors.json'),
  ];
  for (const p of candidates) {
    if (fs.existsSync(p)) return JSON.parse(fs.readFileSync(p, 'utf8'));
  }
  throw new Error('fec_vectors.json not found (run gen_fec_vectors.py)');
}

let fails = 0;
function check(cond, label) {
  console.log((cond ? '  PASS  ' : '  FAIL  ') + label);
  if (!cond) fails += 1;
}

const v = loadVectors();
const hx = (s) => Buffer.from(s, 'hex');

// 1. systematic encode byte-identical
let encOk = true;
for (const c of v.cases) {
  if (rsEncode(hx(c.msg), v.nparity).toString('hex') !== c.code) encOk = false;
}
check(encOk, `${v.cases.length} RS encode vectors byte-identical`);

// 2. decode corrects the pinned corrupted codewords
let decOk = true;
for (const c of v.correct) {
  const { msg, corrected } = rsDecode(hx(c.corrupt), v.nparity, 17);
  if (msg.toString('hex') !== c.msg || corrected !== c.nerr) decOk = false;
}
check(decOk, `${v.correct.length} corrupted codewords corrected to the original frame`);

// 3. multi-codeword messages: golden blob byte-identical + round-trip
let msgOk = true;
for (const m of v.messages) {
  const blob = encodeMessage(hx(m.msg), v.nparity);
  if (blob.toString('hex') !== m.blob) msgOk = false;
  const { msg: out } = decodeMessage(blob);
  if (out.toString('hex') !== m.msg) msgOk = false;
}
check(msgOk, `${v.messages.length} multi-codeword messages byte-identical + round-trip`);

// 4. interleaved burst across codewords is corrected
let burstOk = false;
try {
  const { msg } = decodeMessage(hx(v.message_burst.corrupt));
  burstOk = msg.toString('hex') === v.message_burst.msg;
} catch (e) {
  burstOk = false;
}
check(burstOk, 'interleaved burst across codewords corrected');

if (fails === 0) console.log('ALL FEC VECTORS MATCH');
else { console.log(`${fails} FAILURE(S)`); process.exit(1); }
