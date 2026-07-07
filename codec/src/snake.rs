// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-Snake: the record plane of a synchronized cat5e audio snake.
//!
//! Carries synchronized, quanta-coded audio from a star of source ("spoke") nodes to one
//! central "mixer" hub, for studio multitrack capture. Like DCF-SSTV/Text/Game it is an
//! *adapter* over the 17-byte DeModFrame quantum (`frame.rs`): one self-delimiting quanta
//! QSS commit-hop packet is fragmented into `1 + frag_total` ordinary CTRL(3) frames whose
//! 4-byte payloads carry the opaque QSS bytes. This L2 framing is byte-deterministic across
//! C/Rust/Python — pinned by `Documentation/snake_vectors.json`. See `DCF_SNAKE_SPEC.md`.
//!
//! The record plane rides CTRL(3) (the audio/control family, alongside DCF-Audio) with a
//! *different* seq split (DCF-Audio is 11:5, DCF-Snake is 5:11 — a wider 11-bit fragment
//! index so a whole QSS packet fits one message). `mode_id` (quanta live/near/relaxed) is an
//! opaque hint that never changes these vectors. A node runs one reassembler per `dst`.
//!
//! This module also hosts the BEACON grandmaster media clock (byte-certified 16-byte payload)
//! and the `unwrap_pid` timeline primitive ported from `client/src-tauri/src/sync.rs`.
//!
//! Layout (all frames version=1, big-endian): record type=Ctrl(3), clock type=Beacon(2):
//!   seq = stream_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
//!   frag_idx 0  descriptor : payload = [len_hi, len_lo, kind, flags]  (kind = mode_id/clk_ver)
//!   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)

use crate::{crc16_ccitt, Frame, FrameType};
use std::collections::BTreeMap;

// ── L2 constants ────────────────────────────────────────────────────────────
pub const FRAG_BITS: u16 = 11;
pub const FRAG_MASK: u16 = 0x7FF;
pub const MAX_FRAGS: usize = 2047;
pub const MAX_PAYLOAD: usize = MAX_FRAGS * 4; // 8188 bytes / message
pub const MAX_STREAM_ID: u16 = (1 << (16 - FRAG_BITS)) - 1; // 31
pub const BROADCAST: u16 = 0xFFFF;

// quanta streaming modes — opaque hints (L2 never parses the QSS bytes).
pub const MODE_LIVE: u8 = 0;
pub const MODE_NEAR: u8 = 1;
pub const MODE_RELAXED: u8 = 2;

// Descriptor flag bits — opaque to L2 (they do not change the framing bytes).
pub const FLAG_MORE: u8 = 0x01;
pub const FLAG_ANCHOR: u8 = 0x02;
pub const FLAG_END: u8 = 0x04;

// BEACON grandmaster media clock.
pub const CLOCK_LEN: usize = 16;
pub const CLOCK_VER: u8 = 1;
pub const NOMINAL_RATE_MHZ_48K: u32 = 48_000_000;

// unwrap_pid rolling space (ported verbatim from sync.rs PID_MOD).
pub const PID_MOD: u64 = 2048;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SnakeError {
    /// Message exceeds the 8188-byte cap.
    PayloadTooLarge,
    /// stream_id exceeds the 5-bit field.
    BadStreamId,
}

/// Map a human channel/passphrase to a 16-bit rendezvous `dst`. Empty => broadcast.
pub fn channel_id(name: &str) -> u16 {
    if name.is_empty() {
        BROADCAST
    } else {
        crc16_ccitt(name.as_bytes())
    }
}

// ── L2: packetize (generic 5:11 fragmenter, shared by CTRL record + BEACON clock) ─────────
fn frag_packetize(
    payload: &[u8],
    stream_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    frame_type: FrameType,
    kind: u8,
    flags: u8,
) -> Result<Vec<[u8; 17]>, SnakeError> {
    if payload.len() > MAX_PAYLOAD {
        return Err(SnakeError::PayloadTooLarge);
    }
    if stream_id > MAX_STREAM_ID {
        return Err(SnakeError::BadStreamId);
    }
    let frag_total = ((payload.len() + 3) / 4) as u16;
    let mut frames = Vec::with_capacity(1 + frag_total as usize);

    let desc_seq = stream_id << FRAG_BITS;
    let len = payload.len() as u16;
    let desc = [(len >> 8) as u8, (len & 0xFF) as u8, kind, flags];
    frames.push(Frame::new(1, frame_type, desc_seq, src, dst, desc, ts_us).encode());

    for k in 1..=frag_total {
        let off = (k as usize - 1) * 4;
        let mut chunk = [0u8; 4];
        let end = (off + 4).min(payload.len());
        chunk[..end - off].copy_from_slice(&payload[off..end]);
        let seq = (stream_id << FRAG_BITS) | k;
        frames.push(Frame::new(1, frame_type, seq, src, dst, chunk, ts_us).encode());
    }
    Ok(frames)
}

/// Serialise one QSS commit-hop packet into CTRL(3) record frames.
pub fn packetize(
    payload: &[u8],
    stream_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    mode_id: u8,
    flags: u8,
) -> Result<Vec<[u8; 17]>, SnakeError> {
    frag_packetize(payload, stream_id, ts_us, src, dst, FrameType::Ctrl, mode_id, flags)
}

/// Serialise a packed 16-byte grandmaster clock payload into BEACON(2) frames.
pub fn beacon_packetize(
    clock: &[u8; CLOCK_LEN],
    beacon_slot: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
) -> Result<Vec<[u8; 17]>, SnakeError> {
    frag_packetize(clock, beacon_slot, ts_us, src, dst, FrameType::Beacon, CLOCK_VER, flags)
}

// ── L2: reassembler ─────────────────────────────────────────────────────────
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SnakeMessage {
    pub stream_id: u16,
    pub ts_us: u32,
    pub src: u16,
    pub dst: u16,
    pub mode_id: u8, // kind byte: mode_id (record) or clk_ver (beacon)
    pub flags: u8,
    pub data: Vec<u8>,
}

#[derive(Default)]
struct Slot {
    desc: Option<(u16, u8, u8)>, // (payload_len, kind, flags)
    ts_us: u32,
    src: u16,
    dst: u16,
    frags: BTreeMap<u16, [u8; 4]>,
}

/// Stateful reassembler filtering a chosen frame type (Ctrl for record, Beacon for the clock).
/// `push` emits a completed message as soon as its descriptor and every data fragment have
/// arrived; duplicates are ignored; `finalize` reports any still-incomplete message as lost.
pub struct SnakeReassembler {
    accept: FrameType,
    slots: BTreeMap<u16, Slot>,
}

impl Default for SnakeReassembler {
    fn default() -> Self {
        Self { accept: FrameType::Ctrl, slots: BTreeMap::new() }
    }
}

impl SnakeReassembler {
    /// A record-plane reassembler (accepts CTRL(3) frames).
    pub fn new() -> Self {
        Self::default()
    }

    /// A reassembler filtering the given frame type (e.g. `FrameType::Beacon` for the clock).
    pub fn with_type(accept: FrameType) -> Self {
        Self { accept, slots: BTreeMap::new() }
    }

    pub fn push(&mut self, frame: &[u8; 17]) -> Option<SnakeMessage> {
        let d = Frame::decode(&frame[..]).ok()?;
        if d.frame_type != self.accept {
            return None;
        }
        let stream_id = d.seq >> FRAG_BITS;
        let frag_idx = d.seq & FRAG_MASK;
        let entry = self.slots.entry(stream_id).or_default();
        entry.ts_us = d.timestamp_us;
        entry.src = d.src_id;
        entry.dst = d.dst_id;
        if frag_idx == 0 {
            if entry.desc.is_none() {
                let len = ((d.payload[0] as u16) << 8) | d.payload[1] as u16;
                entry.desc = Some((len, d.payload[2], d.payload[3]));
            }
        } else {
            entry.frags.entry(frag_idx).or_insert(d.payload);
        }
        self.try_emit(stream_id)
    }

    fn try_emit(&mut self, stream_id: u16) -> Option<SnakeMessage> {
        let entry = self.slots.get(&stream_id)?;
        let (len, mode_id, flags) = entry.desc?;
        let frag_total = ((len as usize + 3) / 4) as u16;
        if (1..=frag_total).any(|k| !entry.frags.contains_key(&k)) {
            return None;
        }
        let mut raw = Vec::with_capacity(frag_total as usize * 4);
        for k in 1..=frag_total {
            raw.extend_from_slice(&entry.frags[&k]);
        }
        raw.truncate(len as usize);
        let (ts_us, src, dst) = (entry.ts_us, entry.src, entry.dst);
        self.slots.remove(&stream_id);
        Some(SnakeMessage { stream_id, ts_us, src, dst, mode_id, flags, data: raw })
    }

    /// Report every still-incomplete message as lost (ascending stream_id), clearing state.
    pub fn finalize(&mut self) -> Vec<u16> {
        let lost: Vec<u16> = self.slots.keys().copied().collect(); // BTreeMap => sorted
        self.slots.clear();
        lost
    }
}

// ── BEACON grandmaster media clock (byte-certified 16-byte payload) ──────────────────────
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Clock {
    pub gm_sample_count: u64,
    pub nominal_rate_mhz: u32,
    pub tx_seq: u16,
    pub epoch: u16,
}

/// Pack the media clock into the 16-byte big-endian BEACON payload.
pub fn pack_clock(c: &Clock) -> [u8; CLOCK_LEN] {
    let mut o = [0u8; CLOCK_LEN];
    o[0..8].copy_from_slice(&c.gm_sample_count.to_be_bytes());
    o[8..12].copy_from_slice(&c.nominal_rate_mhz.to_be_bytes());
    o[12..14].copy_from_slice(&c.tx_seq.to_be_bytes());
    o[14..16].copy_from_slice(&c.epoch.to_be_bytes());
    o
}

/// Inverse of `pack_clock`.
pub fn unpack_clock(b: &[u8; CLOCK_LEN]) -> Clock {
    Clock {
        gm_sample_count: u64::from_be_bytes(b[0..8].try_into().unwrap()),
        nominal_rate_mhz: u32::from_be_bytes(b[8..12].try_into().unwrap()),
        tx_seq: u16::from_be_bytes(b[12..14].try_into().unwrap()),
        epoch: u16::from_be_bytes(b[14..16].try_into().unwrap()),
    }
}

// ── unwrap_pid — ported verbatim from client/src-tauri/src/sync.rs ───────────────────────
/// Unwrap a rolling packet_id to a monotonic absolute index relative to `prev_abs` (forward
/// progress dominates; a small backward delta = reorder → step back / saturating_sub).
pub fn unwrap_pid(prev_abs: u64, raw: u16, modulus: u64) -> u64 {
    let prev_lo = prev_abs % modulus;
    let raw = raw as u64 % modulus;
    let fwd = (raw + modulus - prev_lo) % modulus;
    if fwd <= modulus / 2 {
        prev_abs + fwd
    } else {
        prev_abs.saturating_sub(modulus - fwd)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packetize_reassemble_is_identity_under_reorder() {
        let pkt: Vec<u8> = (0..800u32).map(|i| (i * 7) as u8).collect(); // a live-size QSS packet
        let frames = packetize(&pkt, 5, 0x010203, 0x00A1, 0xFFFF, MODE_LIVE, FLAG_ANCHOR).unwrap();
        assert_eq!(frames.len(), 1 + (pkt.len() + 3) / 4);
        let mut r = SnakeReassembler::new();
        let mut got = None;
        for f in frames.iter().rev() {
            if let Some(p) = r.push(f) {
                got = Some(p);
            }
        }
        let p = got.expect("message reassembles regardless of frame order");
        assert_eq!((p.stream_id, p.src, p.mode_id, p.flags), (5, 0x00A1, MODE_LIVE, FLAG_ANCHOR));
        assert_eq!(p.data, pkt);
        assert!(r.finalize().is_empty());
    }

    #[test]
    fn dropped_fragment_is_reported_lost() {
        let frames = packetize(&[0u8; 20], 9, 0, 1, 2, MODE_NEAR, 0).unwrap();
        let mut r = SnakeReassembler::new();
        for (i, f) in frames.iter().enumerate() {
            if i == 2 {
                continue;
            }
            assert!(r.push(f).is_none());
        }
        assert_eq!(r.finalize(), vec![9]);
    }

    #[test]
    fn clock_and_unwrap_and_bounds() {
        // clock pack/unpack + fragmented BEACON reassembly is identity
        let c = Clock { gm_sample_count: 0x0123456789ABCDEF, nominal_rate_mhz: NOMINAL_RATE_MHZ_48K,
                        tx_seq: 0x1234, epoch: 7 };
        let packed = pack_clock(&c);
        assert_eq!(unpack_clock(&packed), c);
        let frames = beacon_packetize(&packed, 1, 0xABCD, 0x00A1, BROADCAST, 0).unwrap();
        assert_eq!(frames.len(), 1 + CLOCK_LEN / 4);
        let mut r = SnakeReassembler::with_type(FrameType::Beacon);
        let mut got = None;
        for f in &frames {
            if let Some(m) = r.push(f) {
                got = Some(m);
            }
        }
        let m = got.unwrap();
        assert_eq!(m.data, packed);
        assert_eq!(m.mode_id, CLOCK_VER);
        assert_eq!(unpack_clock(&m.data.as_slice().try_into().unwrap()), c);

        // unwrap_pid matches sync.rs
        assert_eq!(unwrap_pid(0, 1, PID_MOD), 1);
        assert_eq!(unwrap_pid(2047, 0, PID_MOD), 2048);
        assert_eq!(unwrap_pid(100, 98, PID_MOD), 98);
        assert_eq!(unwrap_pid(0, 2047, PID_MOD), 0);

        // bounds
        assert_eq!(packetize(&vec![0u8; MAX_PAYLOAD + 1], 0, 0, 1, 2, 0, 0), Err(SnakeError::PayloadTooLarge));
        assert_eq!(packetize(&[0u8; 4], MAX_STREAM_ID + 1, 0, 1, 2, 0, 0), Err(SnakeError::BadStreamId));
        assert_eq!(channel_id("123456789"), 0x29B1);
        assert_eq!(channel_id(""), BROADCAST);
    }
}
