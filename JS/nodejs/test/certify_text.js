// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// Certifies src/text.js byte-for-byte against the cross-language golden vectors in
// Documentation/text_vectors.json (DCF-Text L2 framing: packetize bytes + the
// reassembly law), so a JS node meshes with the Python/Rust/C/Go DcfTextNode.
// Dependency-free.
//   node JS/nodejs/test/certify_text.js   (from the repo root)

const fs = require('fs');
const path = require('path');
const {
  packetize, TextReassembler,
  FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_PACKET_ID, FDATA,
} = require('../src/text.js');

function loadVectors() {
  const candidates = [
    path.join(__dirname, '..', '..', '..', 'Documentation', 'text_vectors.json'),
    path.join(__dirname, '..', '..', '..', 'python', 'MCP', 'text_vectors.json'),
  ];
  for (const p of candidates) {
    if (fs.existsSync(p)) return JSON.parse(fs.readFileSync(p, 'utf8'));
  }
  throw new Error('text_vectors.json not found (run gen_text_vectors.py)');
}

let fails = 0;
function check(cond, label) {
  console.log((cond ? '  PASS  ' : '  FAIL  ') + label);
  if (!cond) fails += 1;
}

const v = loadVectors();
const hx = (s) => Buffer.from(s, 'hex');

// 0. constants agree with the spec
const c = v.constants;
check(
  FRAG_BITS === c.frag_bits && MAX_FRAGS === c.max_frags && MAX_PAYLOAD === c.max_payload &&
  MAX_PACKET_ID === c.max_packet_id && FDATA === c.frame_type_data,
  'L2 constants match the spec',
);

// 1. packetize: descriptor + data frames byte-identical (framing list + the anchor)
const framingCases = [...v.framing, v.anchors.exampleTextMessage];
let frameOk = true;
for (const t of framingCases) {
  const frames = packetize(hx(t.payload), t.packet_id, t.ts_us, t.src, t.dst, t.flags);
  const got = frames.map((f) => f.toString('hex'));
  if (got.length !== t.frames.length || got.some((g, i) => g !== t.frames[i])) {
    frameOk = false;
    console.log(`    packetize mismatch (packet_id=${t.packet_id}): got ${JSON.stringify(got)} want ${JSON.stringify(t.frames)}`);
  }
}
check(frameOk, `${framingCases.length} packetize vectors byte-identical (incl. anchor exampleTextMessage)`);

// 2. reassembly: feed input frames, emitted messages + lost set match
let reasmOk = true;
for (const r of v.reassembly) {
  const ra = new TextReassembler(); // accept every channel
  const emitted = [];
  for (const f of r.input_frames) emitted.push(...ra.push(hx(f)));
  const lost = ra.finalize().map((e) => e.lost);

  const want = r.messages;
  let ok = emitted.length === want.length;
  for (let i = 0; ok && i < want.length; i++) {
    const e = emitted[i];
    const m = want[i];
    ok = e.packetId === m.packet_id && e.tsUs === m.ts_us && e.src === m.src &&
      e.dst === m.dst && e.flags === m.flags && e.text === m.text &&
      Buffer.from(e.text, 'utf8').toString('hex') === m.payload;
  }
  // lost packet ids (order-independent)
  const lostOk = JSON.stringify([...lost].sort((a, b) => a - b)) ===
    JSON.stringify([...r.lost].sort((a, b) => a - b));
  if (!ok || !lostOk) {
    reasmOk = false;
    console.log(`    reassembly mismatch (${r.name}): emitted ${JSON.stringify(emitted)} lost ${JSON.stringify(lost)}`);
  }
}
check(reasmOk, `${v.reassembly.length} reassembly scenarios match (order/drop/dup, + lost set)`);

if (fails === 0) console.log('ALL TEXT VECTORS MATCH');
else { console.log(`${fails} FAILURE(S)`); process.exit(1); }
