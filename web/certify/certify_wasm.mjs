// SPDX-License-Identifier: LGPL-3.0-only
// Certify that the WASM codec is BYTE-IDENTICAL to the committed reference
// vectors. The WASM links the same `dcf-wire-codec` the native `cargo test
// --test certify` exercises, so this checks the wasm-bindgen surface against the
// concrete adapter / superpack / fec / roundtrip vectors plus the CRC anchors.
// Any drift fails CI exactly like the native certs.
//
//   node certify/certify_wasm.mjs
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import init, * as dcf from '../src/wasm/dcf_codec_wasm.js'

const doc = (f) => JSON.parse(readFileSync(new URL(`../../Documentation/${f}`, import.meta.url)))
const hex = (b) => Buffer.from(b).toString('hex')
const bytes = (h) => Uint8Array.from(Buffer.from(h, 'hex'))
const framesHex = (flat) => { const o = []; for (let i = 0; i + 17 <= flat.length; i += 17) o.push(hex(flat.slice(i, i + 17))); return o }

let pass = 0
const fails = []
function eq(name, got, want) {
  if (got === want) { pass++; return }
  fails.push(`${name}\n    got  ${got}\n    want ${want}`)
}
// Order-insensitive deep compare (object keys sorted) so field order doesn't matter.
const canon = (v) => JSON.stringify(v, (_k, val) =>
  val && typeof val === 'object' && !Array.isArray(val)
    ? Object.fromEntries(Object.keys(val).sort().map((k) => [k, val[k]]))
    : val)
function deep(name, got, want) {
  if (canon(got) === canon(want)) { pass++; return }
  fails.push(`${name}\n    got  ${canon(got)}\n    want ${canon(want)}`)
}

async function main() {
  const wasm = readFileSync(fileURLToPath(new URL('../src/wasm/dcf_codec_wasm_bg.wasm', import.meta.url)))
  try { await init({ module_or_path: wasm }) } catch { await init(wasm) }

  // ── CRC anchors + example frame (the wire quantum) ─────────────────────────
  const enc = new TextEncoder()
  eq('crc("123456789")', dcf.crc16(enc.encode('123456789')), 0x29b1)
  eq('crc(0^15)', dcf.crc16(new Uint8Array(15)), 0x4ec3)
  const gv = doc('golden_vectors.json')
  // example frame: version=1, CTRL(3), seq=0x1234, src=1, dst=0xffff, payload=deadbeef, ts=0xab12cd
  eq('exampleFrame_full',
    hex(dcf.encode_frame(1, 3, 0x1234, 1, 0xffff, bytes('deadbeef'), 0xab12cd)),
    gv.anchors.exampleFrame_full)

  // ── DCF-Text / Game / Audio L2 framing (concrete field→frame vectors) ──────
  const tv = doc('text_vectors.json')
  for (const c of tv.framing)
    deep(`text.framing src=${c.src} pid=${c.packet_id}`,
      framesHex(dcf.text_packetize(bytes(c.payload), c.packet_id, c.ts_us, c.src, c.dst, c.flags)), c.frames)
  for (const c of tv.reassembly) {
    const r = new dcf.TextReassembler(); const out = []
    for (const f of c.input_frames) {
      const p = r.push(bytes(f))
      // the vectors carry the full packet incl. the raw payload (hex of the text)
      if (p) out.push({ ...p, payload: hex(new TextEncoder().encode(p.text)) })
    }
    deep(`text.reassembly ${c.name}`, out, c.messages)
    deep(`text.reassembly ${c.name} lost`, Array.from(r.finalize()), c.lost)
  }

  const gmv = doc('game_vectors.json')
  for (const c of gmv.framing)
    deep(`game.framing type=${c.msg_type_id} pid=${c.packet_id}`,
      framesHex(dcf.game_packetize(c.msg_type_id, bytes(c.payload), c.packet_id, c.ts_us, c.src, c.dst, c.flags)), c.frames)
  for (const c of gmv.snapshot_roundtrip) {
    const fl = c.fields
    eq(`game.snapshot_pack`, hex(dcf.snapshot_pack(fl.x, fl.y, fl.z, fl.vx, fl.vy, fl.vz, fl.yaw)), c.bytes)
    deep(`game.snapshot_unpack`, dcf.snapshot_unpack(bytes(c.bytes)), fl)
  }
  for (const c of gmv.input_roundtrip) {
    eq(`game.input_pack`, hex(dcf.input_pack(c.fields.tick, c.fields.buttons)), c.bytes)
    deep(`game.input_unpack`, dcf.input_unpack(bytes(c.bytes)), c.fields)
  }

  const av = doc('audio_vectors.json')
  for (const c of av.framing)
    deep(`audio.framing codec=${c.codec_id} pid=${c.packet_id}`,
      framesHex(dcf.audio_packetize(c.codec_id, bytes(c.payload), c.packet_id, c.ts_us, c.src, c.dst, c.flags)), c.frames)
  for (const c of av.pcm_roundtrip) // determinism: decode→encode is identity
    eq(`audio.pcm_roundtrip`, hex(dcf.pcm_diag_encode(dcf.pcm_diag_decode(bytes(c.bytes)))), c.bytes)

  // ── SuperPack ──────────────────────────────────────────────────────────────
  const sv = doc('superpack_vectors.json')
  for (const c of sv.cases) {
    eq(`superpack.pack`, hex(dcf.superpack_pack(bytes(c.a), bytes(c.b))), c.super)
    const u = dcf.superpack_unpack(bytes(c.super))
    eq(`superpack.unpack.a`, hex(u.slice(0, 17)), c.a)
    eq(`superpack.unpack.b`, hex(u.slice(17, 34)), c.b)
    eq(`superpack.is_superpack`, dcf.is_superpack(bytes(c.super)), true)
  }

  // ── FEC (message layer: encode_message / decode_message) ───────────────────
  const fv = doc('fec_vectors.json')
  for (const c of fv.messages) {
    eq(`fec.encode len=${c.len}`, hex(dcf.fec_encode(bytes(c.msg), fv.nparity)), c.blob)
    eq(`fec.decode len=${c.len}`, hex(dcf.fec_decode(bytes(c.blob))), c.msg)
  }

  // ── report ─────────────────────────────────────────────────────────────────
  if (fails.length) {
    console.error(`\n✗ ${fails.length} WASM cert FAILURES (${pass} passed):\n`)
    for (const f of fails) console.error('  ✗ ' + f)
    process.exit(1)
  }
  console.error(`✓ WASM codec byte-identical to reference vectors — ${pass} checks passed.`)
}
main().catch((e) => { console.error(e); process.exit(1) })
