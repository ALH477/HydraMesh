// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-Game: low-latency multiplayer state/event transport over the DeModFrame wire.
//!
//! Game traffic is an *adapter* over the 17-byte DeModFrame quantum (`frame.rs`), not a
//! new wire format — exactly like DCF-Audio (`audio.rs`). One game message (a state
//! snapshot, an input frame, or an opaque event) is serialised into `1 + frag_total`
//! ordinary DATA frames. This L2 framing is message-type-agnostic and byte-deterministic
//! across C/Rust/Python — it is pinned by `Documentation/game_vectors.json`. See
//! `Documentation/DCF_GAME_SPEC.md`.
//!
//! Layout (all frames version=1, type=DATA(0), big-endian):
//!   seq = packet_id[15:5] (11 bits) | frag_idx[4:0] (5 bits)
//!   frag_idx 0  descriptor : payload = [payload_len, frag_total, msg_type_id, flags]
//!   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
//!   frag_total = ceil(payload_len/4)  (<= 31  =>  payload_len <= 124 bytes / message)
//!
//! Audio uses CTRL(3); game uses DATA(0), so the two adapters never collide on the wire.

use crate::{Frame, FrameType};
use std::collections::BTreeMap;

// ── L2 constants ────────────────────────────────────────────────────────────
pub const FRAG_BITS: u16 = 5;
pub const FRAG_MASK: u16 = 0x1F;
pub const MAX_FRAGS: usize = 31;
pub const MAX_PAYLOAD: usize = MAX_FRAGS * 4; // 124 bytes / message
pub const MAX_PACKET_ID: u16 = (1 << (16 - FRAG_BITS)) - 1; // 2047

// Message-type registry ids (profiles in DCF_GAME_SPEC.md).
pub const GMSG_SNAPSHOT: u8 = 0; // packed player state (byte-deterministic)
pub const GMSG_INPUT: u8 = 1; // input bitfield + tick (byte-deterministic)
pub const GMSG_EVENT: u8 = 2; // opaque application bytes (not certified)
pub const GMSG_JOIN: u8 = 3; // lobby membership (byte-deterministic)
// ids 4..=255 reserved.

// Descriptor flag bits — request transport behaviour; do not change the L2 bytes.
pub const FLAG_RELIABLE: u8 = 0x01;
pub const FLAG_ORDERED: u8 = 0x02;
pub const FLAG_END_TICK: u8 = 0x04;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameError {
    /// Message exceeds the 124-byte cap.
    PayloadTooLarge,
    /// packet_id exceeds the 11-bit field.
    BadPacketId,
}

// ── L2: packetize ───────────────────────────────────────────────────────────
/// Serialise one game message into DeModFrame DATA frames (descriptor first, then
/// data fragments in order). Each frame is a fully valid 17-byte DeModFrame.
pub fn packetize(
    msg_type_id: u8,
    payload: &[u8],
    packet_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
) -> Result<Vec<[u8; 17]>, GameError> {
    if payload.len() > MAX_PAYLOAD {
        return Err(GameError::PayloadTooLarge);
    }
    if packet_id > MAX_PACKET_ID {
        return Err(GameError::BadPacketId);
    }
    let frag_total = ((payload.len() + 3) / 4) as u8;
    let mut frames = Vec::with_capacity(1 + frag_total as usize);

    // frag_idx 0 — descriptor
    let desc_seq = packet_id << FRAG_BITS;
    let desc = [payload.len() as u8, frag_total, msg_type_id, flags];
    frames.push(Frame::new(1, FrameType::Data, desc_seq, src, dst, desc, ts_us).encode());

    // frag_idx 1..frag_total — data, last chunk zero-padded
    for k in 1..=frag_total {
        let off = (k as usize - 1) * 4;
        let mut chunk = [0u8; 4];
        let end = (off + 4).min(payload.len());
        chunk[..end - off].copy_from_slice(&payload[off..end]);
        let seq = (packet_id << FRAG_BITS) | k as u16;
        frames.push(Frame::new(1, FrameType::Data, seq, src, dst, chunk, ts_us).encode());
    }
    Ok(frames)
}

// ── L2: reassembler ─────────────────────────────────────────────────────────
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GamePacket {
    pub packet_id: u16,
    pub ts_us: u32,
    pub msg_type_id: u8,
    pub flags: u8,
    pub payload: Vec<u8>,
}

#[derive(Default)]
struct Slot {
    desc: Option<(u8, u8, u8, u8)>, // (payload_len, frag_total, msg_type_id, flags)
    ts_us: u32,
    frags: BTreeMap<u8, [u8; 4]>,
}

/// Stateful reassembler. `push` emits a completed message as soon as its descriptor
/// and every data fragment have arrived; duplicates are ignored; `finalize` reports
/// any still-incomplete message as lost. Mirrors the C and Python references.
#[derive(Default)]
pub struct GameReassembler {
    slots: BTreeMap<u16, Slot>,
}

impl GameReassembler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, frame: &[u8; 17]) -> Option<GamePacket> {
        let d = Frame::decode(&frame[..]).ok()?;
        if d.frame_type != FrameType::Data {
            return None;
        }
        let packet_id = d.seq >> FRAG_BITS;
        let frag_idx = (d.seq & FRAG_MASK) as u8;
        let entry = self.slots.entry(packet_id).or_default();
        entry.ts_us = d.timestamp_us;
        if frag_idx == 0 {
            if entry.desc.is_none() {
                entry.desc = Some((d.payload[0], d.payload[1], d.payload[2], d.payload[3]));
            }
        } else {
            entry.frags.entry(frag_idx).or_insert(d.payload);
        }
        self.try_emit(packet_id)
    }

    fn try_emit(&mut self, packet_id: u16) -> Option<GamePacket> {
        let entry = self.slots.get(&packet_id)?;
        let (payload_len, frag_total, msg_type_id, flags) = entry.desc?;
        if (1..=frag_total).any(|k| !entry.frags.contains_key(&k)) {
            return None;
        }
        let mut raw = Vec::with_capacity(frag_total as usize * 4);
        for k in 1..=frag_total {
            raw.extend_from_slice(&entry.frags[&k]);
        }
        raw.truncate(payload_len as usize);
        let ts_us = entry.ts_us;
        self.slots.remove(&packet_id);
        Some(GamePacket { packet_id, ts_us, msg_type_id, flags, payload: raw })
    }

    /// Report every still-incomplete message as lost (ascending packet_id), clearing state.
    pub fn finalize(&mut self) -> Vec<u16> {
        let lost: Vec<u16> = self.slots.keys().copied().collect(); // BTreeMap => sorted
        self.slots.clear();
        lost
    }
}

// ── L1: SNAPSHOT body (msg_type 0) — 14-byte player state, Q8.8 fixed-point ──
pub const SNAPSHOT_LEN: usize = 14;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Snapshot {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub vx: f32,
    pub vy: f32,
    pub vz: f32,
    pub yaw: u16,
}

fn q88(v: f32) -> [u8; 2] {
    let q = (v * 256.0).round().clamp(-32768.0, 32767.0) as i32 as i16;
    (q as u16).to_be_bytes()
}
fn unq88(b: &[u8]) -> f32 {
    (i16::from_be_bytes([b[0], b[1]]) as f32) / 256.0
}

/// Pack a snapshot into 14 deterministic bytes. The byte layout is certified.
pub fn snapshot_pack(s: &Snapshot) -> [u8; SNAPSHOT_LEN] {
    let mut out = [0u8; SNAPSHOT_LEN];
    for (i, v) in [s.x, s.y, s.z, s.vx, s.vy, s.vz].iter().enumerate() {
        out[i * 2..i * 2 + 2].copy_from_slice(&q88(*v));
    }
    out[12..14].copy_from_slice(&s.yaw.to_be_bytes());
    out
}

/// Unpack 14 bytes into a snapshot. `snapshot_pack(snapshot_unpack(b)) == b`.
pub fn snapshot_unpack(b: &[u8; SNAPSHOT_LEN]) -> Snapshot {
    Snapshot {
        x: unq88(&b[0..2]),
        y: unq88(&b[2..4]),
        z: unq88(&b[4..6]),
        vx: unq88(&b[6..8]),
        vy: unq88(&b[8..10]),
        vz: unq88(&b[10..12]),
        yaw: u16::from_be_bytes([b[12], b[13]]),
    }
}

// ── L1: INPUT body (msg_type 1) — 6 bytes: tick u32 + buttons u16 bitfield ───
pub const INPUT_LEN: usize = 6;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Input {
    pub tick: u32,
    pub buttons: u16,
}

pub fn input_pack(p: &Input) -> [u8; INPUT_LEN] {
    let mut out = [0u8; INPUT_LEN];
    out[0..4].copy_from_slice(&p.tick.to_be_bytes());
    out[4..6].copy_from_slice(&p.buttons.to_be_bytes());
    out
}
pub fn input_unpack(b: &[u8; INPUT_LEN]) -> Input {
    Input {
        tick: u32::from_be_bytes([b[0], b[1], b[2], b[3]]),
        buttons: u16::from_be_bytes([b[4], b[5]]),
    }
}

// ── L1: JOIN body (msg_type 3) — player_id u16 + len-prefixed UTF-8 name ─────
pub fn join_pack(player_id: u16, name: &str) -> Vec<u8> {
    let nb = name.as_bytes();
    let n = nb.len().min(MAX_PAYLOAD - 3);
    let mut out = Vec::with_capacity(3 + n);
    out.extend_from_slice(&player_id.to_be_bytes());
    out.push(n as u8);
    out.extend_from_slice(&nb[..n]);
    out
}
pub fn join_unpack(b: &[u8]) -> Option<(u16, String)> {
    if b.len() < 3 {
        return None;
    }
    let player_id = u16::from_be_bytes([b[0], b[1]]);
    let n = b[2] as usize;
    let name = String::from_utf8_lossy(&b[3..(3 + n).min(b.len())]).into_owned();
    Some((player_id, name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packetize_reassemble_is_identity_under_reorder() {
        let body: Vec<u8> = (0..30u8).collect(); // spans 8 fragments
        let frames = packetize(GMSG_EVENT, &body, 42, 0x010203, 0x00A1, 0xFFFF, FLAG_RELIABLE).unwrap();
        assert_eq!(frames.len(), 1 + (body.len() + 3) / 4);
        // feed in reverse order; reassembly must still recover the exact message
        let mut r = GameReassembler::new();
        let mut got = None;
        for f in frames.iter().rev() {
            if let Some(p) = r.push(f) {
                got = Some(p);
            }
        }
        let p = got.expect("message reassembles regardless of frame order");
        assert_eq!(p.packet_id, 42);
        assert_eq!(p.msg_type_id, GMSG_EVENT);
        assert_eq!(p.flags, FLAG_RELIABLE);
        assert_eq!(p.payload, body);
        assert!(r.finalize().is_empty());
    }

    #[test]
    fn dropped_fragment_is_reported_lost() {
        let body = [1u8, 2, 3, 4, 5, 6, 7];
        let frames = packetize(GMSG_SNAPSHOT, &body, 7, 0, 1, 1, 0).unwrap();
        let mut r = GameReassembler::new();
        for (i, f) in frames.iter().enumerate() {
            if i == 2 {
                continue; // drop one data fragment
            }
            assert!(r.push(f).is_none());
        }
        assert_eq!(r.finalize(), vec![7]); // incomplete => lost
    }

    #[test]
    fn snapshot_input_byte_roundtrip() {
        let s = Snapshot { x: 1.5, y: -2.25, z: 0.0, vx: 0.125, vy: -0.5, vz: 3.0, yaw: 40000 };
        assert_eq!(snapshot_unpack(&snapshot_pack(&s)), s); // Q8.8-representable values
        let i = Input { tick: 0xDEADBEEF, buttons: 0xBEEF };
        assert_eq!(input_unpack(&input_pack(&i)), i);
        let jb = join_pack(0xABCD, "neo");
        assert_eq!(join_unpack(&jb), Some((0xABCD, "neo".to_string())));
    }
}
