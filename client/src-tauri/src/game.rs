// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//! DCF-Game session for the client's demo "dot arena": each player's cursor is sent as
//! an unreliable SNAPSHOT (dead-reckoned / latest-wins on receive), and discrete actions
//! (e.g. a score bump) as a reliable EVENT. Both ride the certified DCF-Game L2 framing
//! (`dcf_rust_sdk::game`) over the same frequency-channel rendezvous as audio/messages.

use dcf_rust_sdk::{game, DcfNode};
use std::sync::atomic::{AtomicU16, Ordering};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::channel;

/// A live game session bound to a node, a rendezvous channel, and this peer's player id.
pub struct GameSession {
    node: Arc<DcfNode>,
    channel: Arc<AtomicU16>,
    player_id: u16,
    packet_id: AtomicU16,
}

fn now_us() -> u32 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_micros() as u32 & 0x00FF_FFFF)
        .unwrap_or(0)
}

impl GameSession {
    pub fn new(node: Arc<DcfNode>, channel: Arc<AtomicU16>, node_id: &str) -> Self {
        // Derive a stable u16 player id from the node id via the certified CRC-16
        // (same hash the channel rendezvous uses), so opponents are distinguishable
        // by frame `src` on the wire.
        let player_id = channel::channel_from_passphrase(node_id);
        Self { node, channel, player_id, packet_id: AtomicU16::new(0) }
    }

    pub fn player_id(&self) -> u16 {
        self.player_id
    }

    fn next_pid(&self) -> u16 {
        let p = self.packet_id.fetch_add(1, Ordering::Relaxed);
        p % (game::MAX_PACKET_ID + 1)
    }

    /// Broadcast this player's 2-D position as an unreliable SNAPSHOT.
    pub fn send_position(&self, x: f32, y: f32) -> Result<(), String> {
        let snap = game::Snapshot { x, y, ..Default::default() };
        let body = game::snapshot_pack(&snap);
        let ch = self.channel.load(Ordering::Relaxed);
        self.node
            .send_game_dcf(game::GMSG_SNAPSHOT, &body, self.next_pid(), now_us(), self.player_id, ch, false)
            .map_err(|e| e.to_string())
    }

    /// Broadcast a discrete action (opaque EVENT body) reliably (ARQ + ordered).
    pub fn send_action(&self, text: &str) -> Result<(), String> {
        let body: &[u8] = text.as_bytes();
        let body = &body[..body.len().min(game::MAX_PAYLOAD)];
        let ch = self.channel.load(Ordering::Relaxed);
        self.node
            .send_game_dcf(game::GMSG_EVENT, body, self.next_pid(), now_us(), self.player_id, ch, true)
            .map_err(|e| e.to_string())
    }
}
