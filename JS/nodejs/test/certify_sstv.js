// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// Certifies src/sstv.js byte-for-byte against the cross-language golden vectors in
// Documentation/sstv_vectors.json (DCF-SSTV L2 framing: packetize bytes + the reassembly
// law), so a JS node meshes with the Python/Rust/C/Go DcfSstvNode. Dependency-free.
//   node JS/nodejs/test/certify_sstv.js   (from the repo root)

const fs = require('fs');
const path = require('path');
const {
  packetize, SstvReassembler,
  FRAG_BITS, MAX_FRAGS, MAX_PAYLOAD, MAX_IMAGE_ID, FDATA,
} = require('../src/sstv.js');

function loadVectors() {
  const candidates = [
    path.join(__dirname, '..', '..', '..', 'Documentation', 'sstv_vectors.json'),
    path.join(__dirname, '..', '..', '..', 'python', 'MCP', 'sstv_vectors.json'),
  ];
  for (const p of candidates) {
    if (fs.existsSync(p)) return JSON.parse(fs.readFileSync(p, 'utf8'));
  }
  throw new Error('sstv_vectors.json not found (run gen_sstv_vectors.py)');
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
  MAX_IMAGE_ID === c.max_image_id && FDATA === c.frame_type_data,
  'L2 constants match the spec',
);

// 1. packetize: descriptor + data frames byte-identical (framing list + the anchor)
const framingCases = [...v.framing, v.anchors.exampleImage];
let frameOk = true;
for (const t of framingCases) {
  const frames = packetize(hx(t.payload), t.image_id, t.ts_us, t.src, t.dst, t.format_id, t.flags);
  const got = frames.map((f) => f.toString('hex'));
  if (got.length !== t.frames.length || got.some((g, i) => g !== t.frames[i])) {
    frameOk = false;
    console.log(`    packetize mismatch (image_id=${t.image_id}): got ${JSON.stringify(got)} want ${JSON.stringify(t.frames)}`);
  }
}
check(frameOk, `${framingCases.length} packetize vectors byte-identical (incl. anchor exampleImage)`);

// 2. reassembly: feed input frames, emitted images + lost set match
let reasmOk = true;
for (const r of v.reassembly) {
  const ra = new SstvReassembler(); // accept every channel
  const emitted = [];
  for (const f of r.input_frames) emitted.push(...ra.push(hx(f)));
  const lost = ra.finalize().map((e) => e.lost);

  const want = r.images;
  let ok = emitted.length === want.length;
  for (let i = 0; ok && i < want.length; i++) {
    const e = emitted[i];
    const m = want[i];
    ok = e.imageId === m.image_id && e.tsUs === m.ts_us && e.src === m.src &&
      e.dst === m.dst && e.formatId === m.format_id && e.flags === m.flags &&
      e.data.toString('hex') === m.payload;
  }
  const lostOk = JSON.stringify([...lost].sort((a, b) => a - b)) ===
    JSON.stringify([...r.lost].sort((a, b) => a - b));
  if (!ok || !lostOk) {
    reasmOk = false;
    console.log(`    reassembly mismatch (${r.name}): emitted ${JSON.stringify(emitted)} lost ${JSON.stringify(lost)}`);
  }
}
check(reasmOk, `${v.reassembly.length} reassembly scenarios match (order/drop/dup, + lost set)`);

if (fails === 0) console.log('ALL SSTV VECTORS MATCH');
else { console.log(`${fails} FAILURE(S)`); process.exit(1); }
