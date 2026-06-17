<!-- SPDX-License-Identifier: LGPL-3.0-only -->
<!-- Copyright (c) 2026 DeMoD LLC. -->
<script setup lang="ts">
import { ref, reactive, onMounted } from 'vue'
import { api, on, type UiMessage, type FrameJson, type PeerDetail, type RecordingResult,
  type UiGameSnapshot, type UiGameEvent } from './ipc'

const tabs = ['Connect', 'Peers', 'Messages', 'Jam', 'Game', 'Wire'] as const
const tab = ref<(typeof tabs)[number]>('Connect')

const conn = reactive({ node_id: 'me', host: '0.0.0.0', port: 50551, connected: false, selfId: '' })
const peerForm = reactive({ id: '', host: '127.0.0.1', port: 50552 })
const peers = ref<{ id: string; host: string; port: number }[]>([])
const peerDetails = ref<PeerDetail[]>([])
const rec = reactive({ on: false, dir: 'hydramesh-rec', result: null as RecordingResult | null })

const chan = reactive({ freq: 1420, passphrase: '', active: 0xffff })
const chatText = ref('')
const messages = ref<UiMessage[]>([])

const jam = reactive({ codec: 'opus', running: false })
const level = reactive({ rx: 0 })

// Dot-arena demo: cursor = unreliable SNAPSHOT; "ping" = reliable EVENT.
const ARENA_W = 10, ARENA_H = 7.5 // metres mapped onto the arena box
const game = reactive({
  running: false,
  playerId: 0,
  me: { x: ARENA_W / 2, y: ARENA_H / 2 },
  others: {} as Record<number, { x: number; y: number; t: number }>,
  events: [] as { src: number; text: string }[],
})
let lastPosSent = 0

const wireHex = ref('D31302000001FFFF06020100010203CAD7')
const wire = ref<FrameJson | null>(null)
const err = ref('')

function fail(e: unknown) { err.value = String(e) }

async function connect() {
  err.value = ''
  try {
    conn.selfId = await api.connect(conn.node_id, conn.host, conn.port, peers.value)
    conn.connected = true
    await applyChannel()
  } catch (e) { fail(e) }
}
async function disconnect() { try { await api.disconnect(); conn.connected = false } catch (e) { fail(e) } }
async function addPeer() {
  try { await api.addPeer(peerForm.id, peerForm.host, peerForm.port); peers.value.push({ ...peerForm }) } catch (e) { fail(e) }
}
async function applyChannel() {
  try { chan.active = await api.setChannel(chan.passphrase ? null : chan.freq, chan.passphrase || null) } catch (e) { fail(e) }
}
async function send() {
  if (!chatText.value) return
  try { await api.sendMessage(chatText.value); messages.value.push({ from: 'me', text: chatText.value, channel: chan.active }); chatText.value = '' } catch (e) { fail(e) }
}
async function toggleJam() {
  try {
    if (jam.running) { await api.stopJam(); jam.running = false }
    else { await api.startJam(jam.codec); jam.running = true }
  } catch (e) { fail(e) }
}
async function toggleGame() {
  try {
    if (game.running) { await api.stopGame(); game.running = false }
    else { game.playerId = await api.startGame(); game.running = true }
  } catch (e) { fail(e) }
}
function onArenaMove(e: MouseEvent) {
  if (!game.running) return
  const box = (e.currentTarget as HTMLElement).getBoundingClientRect()
  game.me.x = ((e.clientX - box.left) / box.width) * ARENA_W
  game.me.y = ((e.clientY - box.top) / box.height) * ARENA_H
  const now = performance.now()
  if (now - lastPosSent < 50) return // throttle SNAPSHOTs to ~20 Hz
  lastPosSent = now
  api.sendGamePosition(game.me.x, game.me.y).catch(fail)
}
async function pingArena() {
  try { await api.sendGameAction(`ping from ${game.playerId}`) } catch (e) { fail(e) }
}
async function decode() { try { wire.value = await api.decodeFrame(wireHex.value) } catch (e) { fail(e) } }
async function refreshPeers() { if (conn.connected) { try { peerDetails.value = await api.peers() } catch (e) { fail(e) } } }
async function toggleRec() {
  try {
    if (rec.on) { rec.result = await api.stopRecording(); rec.on = false }
    else { await api.startRecording(rec.dir); rec.on = true }
  } catch (e) { fail(e) }
}

onMounted(async () => {
  await on<UiMessage>('message', (m) => messages.value.push(m))
  await on<{ dir: string; rms: number }>('audio-level', (l) => { if (l.dir === 'rx') level.rx = l.rms })
  await on<UiGameSnapshot>('game-snapshot', (s) => { game.others[s.src] = { x: s.x, y: s.y, t: Date.now() } })
  await on<UiGameEvent>('game-event', (e) => { game.events.unshift({ src: e.src, text: e.text }); game.events.splice(8) })
  await on<string>('status', () => {})
  decode()
  setInterval(refreshPeers, 2000)
  // Drop opponents that have gone silent (>2 s without a snapshot).
  setInterval(() => {
    const cut = Date.now() - 2000
    for (const k of Object.keys(game.others)) if (game.others[+k].t < cut) delete game.others[+k]
  }, 1000)
})
</script>

<template>
  <div class="wrap">
    <header>
      <b>HydraMesh</b> <span class="dim">comms</span>
      <span class="spacer" />
      <span :class="['lamp', conn.connected ? 'on' : 'off']" />
      <span class="dim">{{ conn.connected ? `node ${conn.selfId}` : 'offline' }}</span>
      <span class="dim">· CH {{ chan.active }}</span>
    </header>

    <nav>
      <button v-for="t in tabs" :key="t" :class="{ sel: tab === t }" @click="tab = t">{{ t }}</button>
    </nav>

    <p v-if="err" class="err">{{ err }}</p>

    <section v-show="tab === 'Connect'">
      <label>Node id <input v-model="conn.node_id" /></label>
      <label>Bind host <input v-model="conn.host" /></label>
      <label>UDP port <input v-model.number="conn.port" type="number" /></label>
      <div class="row">
        <button v-if="!conn.connected" @click="connect">Connect</button>
        <button v-else @click="disconnect">Disconnect</button>
      </div>
      <h4>Rendezvous channel (handshakeless)</h4>
      <label>Frequency <input v-model.number="chan.freq" type="number" :disabled="!!chan.passphrase" /></label>
      <label>Passphrase <input v-model="chan.passphrase" placeholder="(optional shared word)" /></label>
      <div class="row"><button @click="applyChannel">Set channel</button><span class="dim">active CH {{ chan.active }}</span></div>
    </section>

    <section v-show="tab === 'Peers'">
      <label>Peer id <input v-model="peerForm.id" /></label>
      <label>Host <input v-model="peerForm.host" /></label>
      <label>Port <input v-model.number="peerForm.port" type="number" /></label>
      <div class="row"><button @click="addPeer">Add peer</button><button @click="refreshPeers">Refresh</button></div>
      <table>
        <tr class="dim"><td>peer</td><td>addr</td><td>rtt</td><td>jitter</td><td>rx</td></tr>
        <tr v-for="p in peerDetails" :key="p.id">
          <td>{{ p.id }}</td><td>{{ p.host }}:{{ p.port }}</td>
          <td>{{ p.stats.avg_rtt.toFixed(1) }} ms</td>
          <td>{{ p.stats.jitter.toFixed(1) }}</td>
          <td>{{ p.stats.packets_received }}</td>
        </tr>
      </table>
      <p v-if="!peerDetails.length" class="dim">no peers yet — add one and they'll appear with live RTT.</p>
    </section>

    <section v-show="tab === 'Messages'">
      <div class="log"><div v-for="(m, i) in messages" :key="i"><b>{{ m.from }}</b>: {{ m.text }} <span class="dim">CH{{ m.channel }}</span></div></div>
      <div class="row"><input v-model="chatText" placeholder="message…" @keyup.enter="send" /><button @click="send">Send</button></div>
    </section>

    <section v-show="tab === 'Jam'">
      <label>Codec
        <select v-model="jam.codec"><option value="opus">Opus</option><option value="pcm">PCM-diag</option><option value="pm">Faust-PM</option></select>
      </label>
      <div class="row"><button @click="toggleJam">{{ jam.running ? 'Stop jam' : 'Start jam' }}</button><span class="dim">CH {{ chan.active }}</span></div>
      <div>rx level <div class="meter"><div :style="{ width: Math.min(100, level.rx * 100) + '%' }" /></div></div>
      <h4>Record (sample-synced, ffmpeg on stop)</h4>
      <label>Output dir <input v-model="rec.dir" /></label>
      <div class="row">
        <button @click="toggleRec">{{ rec.on ? '■ Stop recording' : '● Record' }}</button>
        <span class="dim">{{ rec.on ? 'recording…' : 'idle' }}</span>
      </div>
      <div v-if="rec.result" class="dim">
        master: {{ rec.result.master || '(ffmpeg n/a — raw .opus tracks)' }}<br>
        mixdown: {{ rec.result.mixdown || '—' }}<br>
        tracks: {{ rec.result.tracks.length }}
      </div>
      <p class="dim">Peers tuned to the same channel hear each other. Each source is recorded as a
        bit-exact Opus track on a shared timeline; ffmpeg muxes a multitrack master + a mixdown.</p>
    </section>

    <section v-show="tab === 'Game'">
      <div class="row">
        <button @click="toggleGame">{{ game.running ? 'Leave arena' : 'Join arena' }}</button>
        <button :disabled="!game.running" @click="pingArena">Ping (reliable event)</button>
        <span class="dim">CH {{ chan.active }} · you are #{{ game.playerId }}</span>
      </div>
      <div class="arena" @mousemove="onArenaMove">
        <div class="dot me" :style="{ left: (game.me.x / ARENA_W * 100) + '%', top: (game.me.y / ARENA_H * 100) + '%' }">
          <span class="tag">you</span>
        </div>
        <div v-for="(o, src) in game.others" :key="src" class="dot other"
          :style="{ left: (o.x / ARENA_W * 100) + '%', top: (o.y / ARENA_H * 100) + '%' }">
          <span class="tag">#{{ src }}</span>
        </div>
      </div>
      <p class="dim">Move your cursor in the box: your position streams as unreliable SNAPSHOTs
        (dead-reckoned, latest-wins per source). "Ping" sends a reliable, ordered EVENT —
        both ride the certified DCF-Game framing over channel {{ chan.active }}.</p>
      <div class="log" style="height:90px">
        <div v-for="(e, i) in game.events" :key="i"><b>#{{ e.src }}</b>: {{ e.text }}</div>
      </div>
    </section>

    <section v-show="tab === 'Wire'">
      <label>DeModFrame hex <input v-model="wireHex" @keyup.enter="decode" /></label>
      <div class="row"><button @click="decode">Decode</button></div>
      <table v-if="wire">
        <tr><td>version</td><td>{{ wire.version }}</td></tr>
        <tr><td>type</td><td>{{ wire.frame_type }}</td></tr>
        <tr><td>seq</td><td>{{ wire.seq }}</td></tr>
        <tr><td>src</td><td>{{ wire.src }}</td></tr>
        <tr><td>dst (channel)</td><td>{{ wire.dst }}</td></tr>
        <tr><td>payload</td><td>{{ wire.payload }}</td></tr>
        <tr><td>ts_us</td><td>{{ wire.ts_us }}</td></tr>
      </table>
    </section>
  </div>
</template>

<style>
:root { color-scheme: dark }
body { margin: 0; background: #0a0a0f; color: #e8e8f0; font: 14px/1.5 system-ui, sans-serif }
.wrap { max-width: 760px; margin: 0 auto; padding: 16px }
header { display: flex; align-items: center; gap: 8px; padding-bottom: 8px }
.spacer { flex: 1 }
.dim { color: #6a6a86 }
.lamp { width: 10px; height: 10px; border-radius: 50% }
.lamp.on { background: #4cff82 } .lamp.off { background: #f05a5a }
nav { display: flex; gap: 4px; margin: 8px 0 16px }
nav button { background: #14141f; color: #aaa; border: 0; padding: 6px 12px; border-radius: 6px; cursor: pointer }
nav button.sel { background: #1e2234; color: #50e6dc }
section { display: flex; flex-direction: column; gap: 8px }
label { display: flex; gap: 8px; align-items: center; justify-content: space-between }
input, select { background: #14141f; color: #e8e8f0; border: 1px solid #26263a; border-radius: 6px; padding: 6px 8px; min-width: 200px }
.row { display: flex; gap: 8px; align-items: center }
button { background: #1e2234; color: #50e6dc; border: 0; padding: 6px 14px; border-radius: 6px; cursor: pointer }
.err { color: #f05a5a }
.log { height: 280px; overflow: auto; background: #0d0d14; border: 1px solid #20202e; border-radius: 6px; padding: 8px }
.meter { height: 8px; background: #14141f; border-radius: 4px; overflow: hidden }
.meter > div { height: 100%; background: #4cff82 }
.arena { position: relative; height: 300px; background: #0d0d14; border: 1px solid #26263a; border-radius: 8px; overflow: hidden; cursor: crosshair }
.dot { position: absolute; width: 14px; height: 14px; border-radius: 50%; transform: translate(-50%, -50%); transition: left .05s linear, top .05s linear }
.dot.me { background: #50e6dc; box-shadow: 0 0 8px #50e6dc }
.dot.other { background: #ffb454; box-shadow: 0 0 8px #ffb454 }
.dot .tag { position: absolute; left: 16px; top: -2px; font-size: 11px; color: #9a9ab0; white-space: nowrap }
table td { padding: 2px 12px 2px 0 } table td:first-child { color: #6a6a86 }
</style>
