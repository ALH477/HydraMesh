// SPDX-License-Identifier: LGPL-3.0-only
// End-to-end loopback: prove the WASM codec + dcf-ws-bridge actually move real
// DeModFrames over real UDP/WebSocket, both directions, with no browser.
//
//   WS client ──(wasm text frames, SuperPacked)──▶ bridge ──UDP──▶ peer socket
//   peer socket ──(wasm text frames)──▶ bridge ──WS──▶ WS client  (reassembled)
//
// Run from the .#wasm shell after the wasm is built:
//   node web/certify/loopback_test.mjs
import { createSocket } from 'node:dgram'
import { spawn } from 'node:child_process'
import { once } from 'node:events'
import { fileURLToPath } from 'node:url'
import init, * as dcf from '../src/wasm/dcf_codec_wasm.js'
import { readFileSync } from 'node:fs'

const WS_PORT = 7099
const FRAME = 17
const die = (m) => { console.error('✗ ' + m); process.exit(1) }

if (!globalThis.WebSocket) { console.error('· SKIP loopback: this Node lacks a global WebSocket (need ≥ 22)'); process.exit(0) }

function framesOf(flat) { const o = []; for (let i = 0; i + FRAME <= flat.length; i += FRAME) o.push(flat.slice(i, i + FRAME)); return o }
function unpackDatagram(buf) {
  if (dcf.is_superpack(buf)) { const t = dcf.superpack_unpack(buf); return [t.slice(0, FRAME), t.slice(FRAME, 2 * FRAME)] }
  return framesOf(buf)
}
const withTimeout = (p, ms, what) => Promise.race([p, new Promise((_, r) => setTimeout(() => r(new Error('timeout: ' + what)), ms))])

async function main() {
  const wasm = readFileSync(fileURLToPath(new URL('../src/wasm/dcf_codec_wasm_bg.wasm', import.meta.url)))
  try { await init({ module_or_path: wasm }) } catch { await init(wasm) }
  const CH = dcf.channel_from_passphrase('duet')

  // 1) a stand-in mesh peer (what a Go/Rust/Node node would be) on UDP
  const peer = createSocket('udp4')
  await new Promise((res) => peer.bind(0, '127.0.0.1', res))
  const peerPort = peer.address().port
  const peerGot = once(peer, 'message') // first datagram + its sender (the bridge)

  // 2) the bridge
  const manifest = fileURLToPath(new URL('../bridge/Cargo.toml', import.meta.url))
  const bridge = spawn('cargo', ['run', '--quiet', '--manifest-path', manifest, '--',
    '--listen', `127.0.0.1:${WS_PORT}`, '--udp-bind', '127.0.0.1:0'], { stdio: ['ignore', 'inherit', 'pipe'] })
  // wait for the "listening" banner on stderr
  await withTimeout((async () => { for await (const c of bridge.stderr) if (String(c).includes('listening')) return })(), 60000, 'bridge start')

  // 3) the browser-side WS client
  const ws = new WebSocket(`ws://127.0.0.1:${WS_PORT}`)
  ws.binaryType = 'arraybuffer'
  await withTimeout(once(ws, 'open'), 5000, 'ws open')
  ws.send(JSON.stringify({ op: 'addpeer', host: '127.0.0.1', port: peerPort }))
  await new Promise((r) => setTimeout(r, 300)) // let addpeer resolve

  // ── browser → mesh ─────────────────────────────────────────────────────────
  const flat = dcf.text_packetize(new TextEncoder().encode('hi mesh'), 1, 0, 0x00a1, CH, 0x04)
  const frames = framesOf(flat)
  // batch into a SuperPack (the dialect) when there are ≥2 frames
  if (frames.length >= 2) ws.send(dcf.superpack_pack(frames[0], frames[1]))
  else ws.send(frames[0])
  for (let i = frames.length >= 2 ? 2 : 1; i < frames.length; i++) ws.send(frames[i])

  const [dgram, rinfo] = await withTimeout(peerGot, 5000, 'peer receive')
  const rx = new dcf.TextReassembler()
  let got = null
  for (const f of unpackDatagram(new Uint8Array(dgram))) { const p = rx.push(f); if (p) got = p.text }
  // a multi-frame message may arrive as more datagrams; drain a moment
  if (!got) {
    peer.on('message', (d) => { for (const f of unpackDatagram(new Uint8Array(d))) { const p = rx.push(f); if (p) got = p.text } })
    await new Promise((r) => setTimeout(r, 500))
  }
  if (got !== 'hi mesh') die(`browser→mesh: peer reassembled ${JSON.stringify(got)}, want "hi mesh"`)
  console.error('✓ browser → mesh: peer received + reassembled "hi mesh" over real UDP')

  // ── mesh → browser ─────────────────────────────────────────────────────────
  const wsGot = new Promise((resolve) => {
    const rxb = new dcf.TextReassembler()
    ws.addEventListener('message', (ev) => {
      if (!(ev.data instanceof ArrayBuffer)) return
      for (const f of unpackDatagram(new Uint8Array(ev.data))) { const p = rxb.push(f); if (p) resolve(p.text) }
    })
  })
  const reply = framesOf(dcf.text_packetize(new TextEncoder().encode('yo browser'), 2, 0, 0x00b2, CH, 0x04))
  for (const f of reply) peer.send(f, rinfo.port, rinfo.address) // reply to the bridge's UDP addr
  const back = await withTimeout(wsGot, 5000, 'ws receive')
  if (back !== 'yo browser') die(`mesh→browser: client reassembled ${JSON.stringify(back)}, want "yo browser"`)
  console.error('✓ mesh → browser: WS client received + reassembled "yo browser" over real UDP')

  ws.close(); peer.close(); bridge.kill('SIGTERM')
  console.error('✓ loopback OK — WASM codec + dcf-ws-bridge move DeModFrames both ways.')
  process.exit(0)
}
main().catch((e) => { console.error(e); process.exit(1) })
