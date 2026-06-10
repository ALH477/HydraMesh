// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//! Node lifecycle + the receive handler that bridges the SDK into Tauri UI events
//! and the audio engine. Audio is reassembled **per source** (keyed by frame src_id)
//! so multiple peers on the same channel don't collide, then forwarded as `AudioRx`.

use dcf_rust_sdk::{audio, reassemble_audio_payload, GameEvent, MessageHandler, Position};
use parking_lot::Mutex;
use serde::Serialize;
use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU16, Ordering};
use std::sync::mpsc::Sender;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};
use tauri::{AppHandle, Emitter};

use crate::channel;

/// GameEvent type byte we use for chat/data messages.
pub const CHAT_EVENT: u8 = 42;

/// One completed inbound audio packet, tagged with its source and timing — the unit
/// the audio engine (playback) and the recorder (per-source tracks) both consume.
pub struct AudioRx {
    pub src_id: u16,
    pub packet_id: u16,
    pub recv_us: i64,
    pub codec_id: u8,
    pub opus: Vec<u8>,
}

#[derive(Serialize, Clone)]
pub struct UiMessage {
    pub from: String,
    pub text: String,
    pub channel: u16,
}

#[derive(Serialize, Clone)]
pub struct UiAudioLevel {
    pub dir: &'static str, // "rx" | "tx"
    pub src: u16,
    pub channel: u16,
}

/// Encode a channel into a chat payload: "<channel>\u{1}<text>".
pub fn tag_message(ch: u16, text: &str) -> String {
    format!("{}\u{1}{}", ch, text)
}

fn now_us() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_micros() as i64)
        .unwrap_or(0)
}

/// Implements the SDK receive trait, forwarding to the UI and the audio sink.
pub struct UiHandler {
    app: AppHandle,
    channel: Arc<AtomicU16>,
    reasm: Mutex<HashMap<u16, audio::AudioReassembler>>, // per-source reassembly
    audio_tx: Mutex<Option<Sender<AudioRx>>>,
}

impl UiHandler {
    pub fn new(app: AppHandle, channel: Arc<AtomicU16>) -> Arc<Self> {
        Arc::new(Self {
            app,
            channel,
            reasm: Mutex::new(HashMap::new()),
            audio_tx: Mutex::new(None),
        })
    }

    /// Route completed audio packets to this sink (set when a jam starts).
    pub fn set_audio_sink(&self, tx: Option<Sender<AudioRx>>) {
        *self.audio_tx.lock() = tx;
    }
}

impl MessageHandler for UiHandler {
    fn handle_position(&self, _p: Position, _from: SocketAddr) {}

    fn handle_audio(&self, data: &[u8], _from: SocketAddr) {
        if data.len() != 17 {
            return;
        }
        let my = self.channel.load(Ordering::Relaxed);
        // Handshakeless rendezvous: drop frames not on our channel (or broadcast).
        match channel::frame_channel(data) {
            Some(dst) if channel::accepts(dst, my) => {}
            _ => return,
        }
        let src = channel::frame_src(data).unwrap_or(0);
        let frame: [u8; 17] = match data.try_into() {
            Ok(f) => f,
            Err(_) => return,
        };
        let mut map = self.reasm.lock();
        let r = map.entry(src).or_default();
        if let Some(pkt) = reassemble_audio_payload(r, &frame) {
            if let Some(tx) = self.audio_tx.lock().as_ref() {
                let _ = tx.send(AudioRx {
                    src_id: src,
                    packet_id: pkt.packet_id,
                    recv_us: now_us(),
                    codec_id: pkt.codec_id,
                    opus: pkt.payload,
                });
            }
            let _ = self
                .app
                .emit("audio-level", UiAudioLevel { dir: "rx", src, channel: my });
        }
    }

    fn handle_game_event(&self, e: GameEvent, from: SocketAddr) {
        if e.event_type != CHAT_EVENT {
            return;
        }
        let my = self.channel.load(Ordering::Relaxed);
        if let Some((ch, text)) = e.data.split_once('\u{1}') {
            if ch.parse::<u16>().ok() == Some(my) {
                let _ = self.app.emit(
                    "message",
                    UiMessage { from: from.to_string(), text: text.to_string(), channel: my },
                );
            }
        }
    }
}
