// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
// Typed wrappers over the Tauri command/event bridge to the Rust comms core.
import { invoke } from '@tauri-apps/api/core'
import { listen, type UnlistenFn } from '@tauri-apps/api/event'

export interface PeerArg { id: string; host: string; port: number }
export interface FrameJson {
  version: number; frame_type: string; seq: number; src: number; dst: number; payload: string; ts_us: number
}
export interface UiMessage { from: string; text: string; channel: number }
export interface UiAudioLevel { dir: 'rx' | 'tx'; src: number; channel: number }
export interface UiGameSnapshot { src: number; x: number; y: number; channel: number }
export interface UiGameEvent { src: number; text: string; channel: number }

export const api = {
  connect: (node_id: string, host: string, port: number, peers: PeerArg[]) =>
    invoke<string>('connect', { args: { node_id, host, port, peers } }),
  disconnect: () => invoke<void>('disconnect'),
  addPeer: (id: string, host: string, port: number) => invoke<void>('add_peer', { id, host, port }),
  setChannel: (freq: number | null, passphrase: string | null) =>
    invoke<number>('set_channel', { args: { freq, passphrase } }),
  sendMessage: (text: string) => invoke<void>('send_message', { text }),
  startJam: (codec: string) => invoke<void>('start_jam', { codec }),
  stopJam: () => invoke<void>('stop_jam'),
  startGame: () => invoke<number>('start_game'),
  stopGame: () => invoke<void>('stop_game'),
  sendGamePosition: (x: number, y: number) => invoke<void>('send_game_position', { x, y }),
  sendGameAction: (text: string) => invoke<void>('send_game_action', { text }),
  decodeFrame: (hex: string) => invoke<FrameJson>('decode_frame', { hex }),
  metrics: () => invoke<Record<string, unknown>>('metrics'),
  peers: () => invoke<PeerDetail[]>('peers'),
  startRecording: (dir: string) => invoke<void>('start_recording', { dir }),
  stopRecording: () => invoke<RecordingResult>('stop_recording'),
  recordingStatus: () => invoke<boolean>('recording_status'),
  startLocalRadio: (bind: string, http: string, archive: string) =>
    invoke<string>('start_local_radio', { args: { bind, http, archive } }),
  stopLocalRadio: () => invoke<void>('stop_local_radio'),
  radioStatus: () => invoke<boolean>('radio_status'),
  openUrl: (url: string) => invoke<void>('open_url', { url }),
}

export interface PeerStats { last_rtt: number; avg_rtt: number; jitter: number; packets_received: number }
export interface PeerDetail { id: string; host: string; port: number; stats: PeerStats }
export interface RecordingResult { dir: string; master: string | null; mixdown: string | null; tracks: string[] }

export function on<T>(event: string, cb: (payload: T) => void): Promise<UnlistenFn> {
  return listen<T>(event, (e) => cb(e.payload))
}
