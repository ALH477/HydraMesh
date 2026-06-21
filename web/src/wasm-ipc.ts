// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
// Browser transport for the redesigned comms UI. Mirrors the Tauri `ipc.ts`
// `api` + `on` surface EXACTLY, so `App.vue` is shared byte-for-byte between the
// desktop and web builds (selected by the `@ipc` alias). The DCF protocol runs
// here in WebAssembly (the certified codec); only opaque datagrams cross the
// WebSocket to `dcf-ws-bridge`, which relays them to the plaintext UDP mesh.
//
// Dialect: the bare-frame text/game/audio dialect of JS/nodejs/src/node.js —
// frames batched into 32-byte SuperPacks, rendezvous on the frame `dst`. Text
// rides the active channel; game rides a derived sub-channel (active ^ 1) so the
// two never collide on DATA(0) (the documented foot-gun); audio rides CTRL on
// the active channel.

import init, * as dcf from './wasm/dcf_codec_wasm.js'
import { WASM_B64 } from './wasm/wasm-inline'

// ── shared types (identical to client/src/ipc.ts) ───────────────────────────
export interface PeerArg { id: string; host: string; port: number }
export interface FrameJson {
  version: number; frame_type: string; seq: number; src: number; dst: number; payload: string; ts_us: number
}
export interface UiMessage { from: string; text: string; channel: number }
export interface UiAudioLevel { dir: 'rx' | 'tx'; src: number; channel: number }
export interface UiGameSnapshot { src: number; x: number; y: number; channel: number }
export interface UiGameEvent { src: number; text: string; channel: number }
export interface PeerStats { last_rtt: number; avg_rtt: number; jitter: number; packets_received: number }
export interface PeerDetail { id: string; host: string; port: number; stats: PeerStats }
export interface RecordingResult { dir: string; master: string | null; mixdown: string | null; tracks: string[] }

// ── constants (mirror the codec) ────────────────────────────────────────────
const BROADCAST = 0xffff
const FRAME = 17
const TEXT_MAX_PID = 63, GAME_MAX_PID = 2047
const TEXT_FLAG_RELIABLE = 0x04
const GMSG_SNAPSHOT = 0, GMSG_EVENT = 2
const GAME_FLAG_RELIABLE = 0x01
const CODEC_PCM_DIAG = 1
const PCM_RATE = 6000, PCM_BLOCK = 120 // 20 ms @ 6 kHz

// ── wasm load (inlined base64 → single self-contained HTML) ──────────────────
function b64ToBytes(b64: string): Uint8Array {
  const bin = atob(b64)
  const out = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i)
  return out
}
let wasmReady: Promise<void> | null = null
function ensureWasm(): Promise<void> {
  if (!wasmReady) {
    const bytes = b64ToBytes(WASM_B64)
    // Be tolerant of wasm-bindgen init signature variations across versions.
    wasmReady = (init as any)({ module_or_path: bytes }).catch(() => (init as any)(bytes))
  }
  return wasmReady!
}

// ── tiny event emitter (matches the Tauri `on` contract) ─────────────────────
type Cb = (payload: any) => void
const listeners = new Map<string, Set<Cb>>()
function emit(event: string, payload: any) {
  listeners.get(event)?.forEach((cb) => cb(payload))
}
export function on<T>(event: string, cb: (payload: T) => void): Promise<() => void> {
  let set = listeners.get(event)
  if (!set) { set = new Set(); listeners.set(event, set) }
  set.add(cb as Cb)
  return Promise.resolve(() => set!.delete(cb as Cb))
}

// ── connection / channel state ───────────────────────────────────────────────
function bridgeUrl(): string {
  const q = new URLSearchParams(location.search).get('bridge')
  return q || (localStorage.getItem('dcf_bridge') ?? 'ws://127.0.0.1:7000')
}
const accepts = (dst: number, my: number) => dst === my || dst === BROADCAST
const gameChannel = (active: number) => (active === BROADCAST ? BROADCAST : active ^ 0x0001)

const state = {
  ws: null as WebSocket | null,
  selfNum: 0,
  active: BROADCAST,
  peers: [] as PeerArg[],
  rxCount: 0,
  textPid: 0,
  gamePid: 0,
  textReasm: new Map<number, any>(), // per-src dcf.TextReassembler
  gameReasm: new Map<number, any>(),
  audioReasm: new Map<number, any>(),
}

function resetReassemblers() {
  state.textReasm.clear(); state.gameReasm.clear(); state.audioReasm.clear()
}

// ── outbound: split flat frames, batch into SuperPacks, send over WS ──────────
function framesOf(flat: Uint8Array): Uint8Array[] {
  const out: Uint8Array[] = []
  for (let i = 0; i + FRAME <= flat.length; i += FRAME) out.push(flat.slice(i, i + FRAME))
  return out
}
function sendDatagrams(frames: Uint8Array[]) {
  const ws = state.ws
  if (!ws || ws.readyState !== WebSocket.OPEN) return
  // Pair consecutive frames into 32-byte SuperPacks; a lone trailing frame goes raw.
  let i = 0
  for (; i + 1 < frames.length; i += 2) {
    try { ws.send(dcf.superpack_pack(frames[i], frames[i + 1])) }
    catch { ws.send(frames[i]); ws.send(frames[i + 1]) }
  }
  if (i < frames.length) ws.send(frames[i])
}
const nowTs = () => Math.floor(performance.now() * 1000) & 0xffffff

// ── inbound: unpack, route by frame type / channel, emit UI events ────────────
function handleDatagram(buf: Uint8Array) {
  let frames: Uint8Array[]
  if (dcf.is_superpack(buf)) {
    try { const two = dcf.superpack_unpack(buf); frames = [two.slice(0, FRAME), two.slice(FRAME, 2 * FRAME)] }
    catch { return }
  } else if (buf.length % FRAME === 0) {
    frames = framesOf(buf)
  } else { return }

  for (const f of frames) {
    const d = dcf.decode_frame(f) as any
    if (!d || !d.valid) continue
    state.rxCount++
    const { src, dst, frame_type } = d
    if (frame_type === 3) {
      // CTRL → audio, on the active channel
      if (!accepts(dst, state.active)) continue
      routeAudio(src, f)
    } else if (frame_type === 0) {
      // DATA → game (sub-channel) or text (active channel)
      if (accepts(dst, gameChannel(state.active))) routeGame(src, f)
      else if (accepts(dst, state.active)) routeText(src, f)
    }
  }
}

function routeText(src: number, frame: Uint8Array) {
  let r = state.textReasm.get(src)
  if (!r) { r = new dcf.TextReassembler(); state.textReasm.set(src, r) }
  const pkt = r.push(frame)
  if (pkt) emit('message', { from: `0x${src.toString(16).padStart(4, '0')}`, text: pkt.text, channel: state.active } as UiMessage)
}

function routeGame(src: number, frame: Uint8Array) {
  let r = state.gameReasm.get(src)
  if (!r) { r = new dcf.GameReassembler(); state.gameReasm.set(src, r) }
  const pkt = r.push(frame)
  if (!pkt) return
  if (pkt.msg_type_id === GMSG_SNAPSHOT && pkt.payload.length === 14) {
    const s = dcf.snapshot_unpack(pkt.payload) as any
    emit('game-snapshot', { src, x: s.x, y: s.y, channel: state.active } as UiGameSnapshot)
  } else if (pkt.msg_type_id === GMSG_EVENT) {
    emit('game-event', { src, text: new TextDecoder().decode(pkt.payload), channel: state.active } as UiGameEvent)
  }
}

function routeAudio(src: number, frame: Uint8Array) {
  let r = state.audioReasm.get(src)
  if (!r) { r = new dcf.AudioReassembler(); state.audioReasm.set(src, r) }
  const pkt = r.push(frame)
  if (!pkt) return
  emit('audio-level', { dir: 'rx', src, channel: state.active } as UiAudioLevel)
  if (pkt.codec_id === CODEC_PCM_DIAG) playPcm(pkt.payload as Uint8Array)
}

// ── Jam: Web Audio capture/playback (PCM-diag in-WASM) ───────────────────────
let audioCtx: AudioContext | null = null
let micStream: MediaStream | null = null
let micNode: ScriptProcessorNode | null = null
let playAt = 0
const capBuf: number[] = []

function playPcm(bytes: Uint8Array) {
  if (!audioCtx) return
  const samples = new Float32Array(dcf.pcm_diag_decode(bytes))
  if (!samples.length) return
  const ab = audioCtx.createBuffer(1, samples.length, PCM_RATE)
  ab.copyToChannel(samples, 0)
  const node = audioCtx.createBufferSource()
  node.buffer = ab; node.connect(audioCtx.destination)
  const t = Math.max(audioCtx.currentTime + 0.02, playAt)
  node.start(t); playAt = t + ab.duration
}

async function startJam(codec: string) {
  if (codec !== 'pcm') throw new Error(`codec "${codec}" not available in the browser build (PCM-diag only)`)
  if (!navigator.mediaDevices?.getUserMedia) throw new Error('Jam needs a secure context — serve over http(s)/localhost, not file://')
  await ensureWasm()
  audioCtx = new AudioContext()
  playAt = audioCtx.currentTime
  micStream = await navigator.mediaDevices.getUserMedia({ audio: true })
  const srcNode = audioCtx.createMediaStreamSource(micStream)
  micNode = audioCtx.createScriptProcessor(2048, 1, 1)
  const ratio = audioCtx.sampleRate / PCM_RATE
  let acc = 0
  micNode.onaudioprocess = (e) => {
    const inp = e.inputBuffer.getChannelData(0)
    for (let i = 0; i < inp.length; i++) { // naive decimation to 6 kHz
      acc += 1
      if (acc >= ratio) { acc -= ratio; capBuf.push(inp[i]) }
    }
    while (capBuf.length >= PCM_BLOCK) {
      const block = Float32Array.from(capBuf.splice(0, PCM_BLOCK))
      emit('audio-level', { dir: 'tx', src: state.selfNum, channel: state.active } as UiAudioLevel)
      const payload = dcf.pcm_diag_encode(block) as Uint8Array
      const flat = dcf.audio_packetize(CODEC_PCM_DIAG, payload, nextAudioPid(), nowTs(), state.selfNum, state.active, 0)
      sendDatagrams(framesOf(flat))
    }
  }
  srcNode.connect(micNode); micNode.connect(audioCtx.destination)
}
let audioPid = 0
const nextAudioPid = () => (audioPid = (audioPid + 1) % (GAME_MAX_PID + 1))
function stopJam() {
  micNode?.disconnect(); micNode = null
  micStream?.getTracks().forEach((t) => t.stop()); micStream = null
}

// ── hex helpers for the Wire inspector ───────────────────────────────────────
function hexToBytes(hex: string): Uint8Array {
  const s = (hex || '').replace(/[^0-9a-fA-F]/g, '')
  const out = new Uint8Array(s.length >> 1)
  for (let i = 0; i < out.length; i++) out[i] = parseInt(s.substr(i * 2, 2), 16)
  return out
}

// ── the `api` facade (same shape as client/src/ipc.ts) ───────────────────────
export const capabilities = { recording: false, radio: false, opus: false }

export const api = {
  capabilities,

  async connect(node_id: string, _host: string, _port: number, peers: PeerArg[]): Promise<string> {
    await ensureWasm()
    state.selfNum = dcf.crc16(new TextEncoder().encode(node_id))
    state.peers = [...peers]
    await openWs()
    for (const p of state.peers) sendControl({ op: 'addpeer', host: p.host, port: p.port })
    return node_id
  },
  async disconnect(): Promise<void> {
    stopJam(); state.ws?.close(); state.ws = null; resetReassemblers()
  },
  async addPeer(id: string, host: string, port: number): Promise<void> {
    state.peers.push({ id, host, port }); sendControl({ op: 'addpeer', host, port })
  },
  async setChannel(freq: number | null, passphrase: string | null): Promise<number> {
    await ensureWasm()
    state.active = passphrase ? dcf.channel_from_passphrase(passphrase) : ((freq ?? 0) & 0xffff)
    resetReassemblers()
    return state.active
  },
  async sendMessage(text: string): Promise<void> {
    await ensureWasm()
    const payload = new TextEncoder().encode(text)
    const flat = dcf.text_packetize(payload, nextTextPid(), nowTs(), state.selfNum, state.active, TEXT_FLAG_RELIABLE)
    sendDatagrams(framesOf(flat))
  },
  startJam: (codec: string) => startJam(codec),
  stopJam: async () => stopJam(),
  async startGame(): Promise<number> { return state.selfNum & 0xff },
  async stopGame(): Promise<void> {},
  async sendGamePosition(x: number, y: number): Promise<void> {
    await ensureWasm()
    const snap = dcf.snapshot_pack(x, y, 0, 0, 0, 0, 0)
    const flat = dcf.game_packetize(GMSG_SNAPSHOT, snap, nextGamePid(), nowTs(), state.selfNum, gameChannel(state.active), 0)
    sendDatagrams(framesOf(flat))
  },
  async sendGameAction(text: string): Promise<void> {
    await ensureWasm()
    const body = new TextEncoder().encode(text)
    const flat = dcf.game_packetize(GMSG_EVENT, body, nextGamePid(), nowTs(), state.selfNum, gameChannel(state.active), GAME_FLAG_RELIABLE)
    sendDatagrams(framesOf(flat))
  },
  async decodeFrame(hex: string): Promise<FrameJson> {
    await ensureWasm()
    const d = dcf.decode_frame(hexToBytes(hex)) as any
    if (!d.valid) throw new Error(d.error || 'invalid frame')
    return { version: d.version, frame_type: d.frame_type_name, seq: d.seq, src: d.src, dst: d.dst,
      payload: Array.from(d.payload as Uint8Array).map((b) => b.toString(16).padStart(2, '0')).join(' '), ts_us: d.ts_us }
  },
  async metrics(): Promise<Record<string, unknown>> {
    return { node_id: state.selfNum, channel: state.active, rx: state.rxCount }
  },
  async peers(): Promise<PeerDetail[]> {
    // No RTT from the relay; surface configured peers so the roster shows them live.
    return state.peers.map((p) => ({ id: p.id, host: p.host, port: p.port,
      stats: { last_rtt: 0, avg_rtt: 0, jitter: 0, packets_received: state.rxCount } }))
  },
  // Host-only features — unavailable in the browser (gated off via capabilities).
  async startRecording(_dir: string): Promise<void> { throw new Error('recording is desktop-only') },
  async stopRecording(): Promise<RecordingResult> { throw new Error('recording is desktop-only') },
  async recordingStatus(): Promise<boolean> { return false },
  async startLocalRadio(_bind: string, _http: string, _archive: string): Promise<string> { throw new Error('radio is desktop-only') },
  async stopLocalRadio(): Promise<void> {},
  async radioStatus(): Promise<boolean> { return false },
  async openUrl(url: string): Promise<void> { window.open(url, '_blank') },
}

// ── packet-id counters ───────────────────────────────────────────────────────
const nextTextPid = () => (state.textPid = (state.textPid + 1) % (TEXT_MAX_PID + 1))
const nextGamePid = () => (state.gamePid = (state.gamePid + 1) % (GAME_MAX_PID + 1))

// ── WebSocket plumbing ───────────────────────────────────────────────────────
function sendControl(obj: unknown) {
  const ws = state.ws
  if (ws && ws.readyState === WebSocket.OPEN) ws.send(JSON.stringify(obj))
}
function openWs(): Promise<void> {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(bridgeUrl())
    ws.binaryType = 'arraybuffer'
    ws.onopen = () => { state.ws = ws; emit('status', 'connected'); resolve() }
    ws.onerror = () => reject(new Error(`cannot reach bridge at ${bridgeUrl()}`))
    ws.onclose = () => { if (state.ws === ws) state.ws = null; emit('status', 'disconnected') }
    ws.onmessage = (ev) => {
      if (ev.data instanceof ArrayBuffer) handleDatagram(new Uint8Array(ev.data))
    }
  })
}
