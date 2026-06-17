// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-Text: chat / agent-to-agent text transport over the DeModFrame wire.
//!
//! Text traffic is an *adapter* over the 17-byte DeModFrame quantum (`frame.rs`), not a
//! new wire format — exactly like DCF-Audio (`audio.rs`) and DCF-Game (`game.rs`). One
//! UTF-8 message is fragmented into `1 + frag_total` ordinary DATA frames whose 4-byte
//! payloads carry the bytes. This L2 framing is byte-deterministic across C/Rust/Python —
//! it is pinned by `Documentation/text_vectors.json`. See `Documentation/DCF_TEXT_SPEC.md`.
//!
//! Layout (all frames version=1, type=DATA(0), big-endian):
//!   seq = packet_id[15:10] (6 bits, 0..63) | frag_idx[9:0] (10 bits, 0..1023)
//!   frag_idx 0  descriptor : payload = [len_hi, len_lo, flags, 0]
//!   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
//!   frag_total = ceil(len/4)  (<= 1023  =>  len <= 4092 bytes / message)
//!
//! Audio uses CTRL(3); text and game ride DATA(0) but use different seq splits (text's
//! 10-bit fragment index vs game's 5-bit), and the descriptor that opens every message
//! tells a receiver which adapter owns the packet_id.

use crate::{crc16_ccitt, Frame, FrameType};
use std::collections::BTreeMap;

// ── L2 constants ────────────────────────────────────────────────────────────
pub const FRAG_BITS: u16 = 10;
pub const FRAG_MASK: u16 = 0x3FF;
pub const MAX_FRAGS: usize = 1023;
pub const MAX_PAYLOAD: usize = MAX_FRAGS * 4; // 4092 bytes / message
pub const MAX_PACKET_ID: u16 = (1 << (16 - FRAG_BITS)) - 1; // 63
pub const BROADCAST: u16 = 0xFFFF;

// Descriptor flag bits — opaque to L2 (they do not change the framing bytes).
pub const FLAG_AGENT: u8 = 0x01;
pub const FLAG_MORE: u8 = 0x02;
pub const FLAG_RELIABLE: u8 = 0x04;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextError {
    /// Message exceeds the 4092-byte cap.
    PayloadTooLarge,
    /// packet_id exceeds the 6-bit field.
    BadPacketId,
}

/// Map a human channel/passphrase to a 16-bit rendezvous `dst` (the same frequency-channel
/// hash the rest of the repo uses). Empty => broadcast.
pub fn channel_id(name: &str) -> u16 {
    if name.is_empty() {
        BROADCAST
    } else {
        crc16_ccitt(name.as_bytes())
    }
}

// ── L2: packetize ───────────────────────────────────────────────────────────
/// Serialise one UTF-8 message (its raw bytes) into DeModFrame DATA frames (descriptor
/// first, then data fragments in order). Each frame is a fully valid 17-byte DeModFrame.
pub fn packetize(
    payload: &[u8],
    packet_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
) -> Result<Vec<[u8; 17]>, TextError> {
    if payload.len() > MAX_PAYLOAD {
        return Err(TextError::PayloadTooLarge);
    }
    if packet_id > MAX_PACKET_ID {
        return Err(TextError::BadPacketId);
    }
    let frag_total = ((payload.len() + 3) / 4) as u16;
    let mut frames = Vec::with_capacity(1 + frag_total as usize);

    // frag_idx 0 — descriptor (2-byte big-endian length so the last fragment can be un-padded)
    let desc_seq = packet_id << FRAG_BITS;
    let len = payload.len() as u16;
    let desc = [(len >> 8) as u8, (len & 0xFF) as u8, flags, 0];
    frames.push(Frame::new(1, FrameType::Data, desc_seq, src, dst, desc, ts_us).encode());

    // frag_idx 1..frag_total — data, last chunk zero-padded
    for k in 1..=frag_total {
        let off = (k as usize - 1) * 4;
        let mut chunk = [0u8; 4];
        let end = (off + 4).min(payload.len());
        chunk[..end - off].copy_from_slice(&payload[off..end]);
        let seq = (packet_id << FRAG_BITS) | k;
        frames.push(Frame::new(1, FrameType::Data, seq, src, dst, chunk, ts_us).encode());
    }
    Ok(frames)
}

// ── L2: reassembler ─────────────────────────────────────────────────────────
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextPacket {
    pub packet_id: u16,
    pub ts_us: u32,
    pub src: u16,
    pub dst: u16,
    pub flags: u8,
    pub text: String,
}

#[derive(Default)]
struct Slot {
    desc: Option<(u16, u8)>, // (payload_len, flags)
    ts_us: u32,
    src: u16,
    dst: u16,
    frags: BTreeMap<u16, [u8; 4]>,
}

/// Stateful reassembler. `push` emits a completed message as soon as its descriptor and
/// every data fragment have arrived; duplicates are ignored; `finalize` reports any
/// still-incomplete message as lost. Mirrors the C and Python references. The reassembler
/// is channel-agnostic — channel filtering, if any, is the caller's job (as in the SDK).
#[derive(Default)]
pub struct TextReassembler {
    slots: BTreeMap<u16, Slot>,
}

impl TextReassembler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, frame: &[u8; 17]) -> Option<TextPacket> {
        let d = Frame::decode(&frame[..]).ok()?;
        if d.frame_type != FrameType::Data {
            return None;
        }
        let packet_id = d.seq >> FRAG_BITS;
        let frag_idx = d.seq & FRAG_MASK;
        let entry = self.slots.entry(packet_id).or_default();
        entry.ts_us = d.timestamp_us;
        entry.src = d.src_id;
        entry.dst = d.dst_id;
        if frag_idx == 0 {
            if entry.desc.is_none() {
                let len = ((d.payload[0] as u16) << 8) | d.payload[1] as u16;
                entry.desc = Some((len, d.payload[2]));
            }
        } else {
            entry.frags.entry(frag_idx).or_insert(d.payload);
        }
        self.try_emit(packet_id)
    }

    fn try_emit(&mut self, packet_id: u16) -> Option<TextPacket> {
        let entry = self.slots.get(&packet_id)?;
        let (len, flags) = entry.desc?;
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
        self.slots.remove(&packet_id);
        Some(TextPacket {
            packet_id,
            ts_us,
            src,
            dst,
            flags,
            text: String::from_utf8_lossy(&raw).into_owned(),
        })
    }

    /// Report every still-incomplete message as lost (ascending packet_id), clearing state.
    pub fn finalize(&mut self) -> Vec<u16> {
        let lost: Vec<u16> = self.slots.keys().copied().collect(); // BTreeMap => sorted
        self.slots.clear();
        lost
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packetize_reassemble_is_identity_under_reorder() {
        let msg = "hello over DeModFrame — fragment me"; // spans many fragments
        let frames = packetize(msg.as_bytes(), 5, 0x010203, 0x00A1, 0xFFFF, FLAG_AGENT).unwrap();
        assert_eq!(frames.len(), 1 + (msg.len() + 3) / 4);
        // feed in reverse order; reassembly must still recover the exact message
        let mut r = TextReassembler::new();
        let mut got = None;
        for f in frames.iter().rev() {
            if let Some(p) = r.push(f) {
                got = Some(p);
            }
        }
        let p = got.expect("message reassembles regardless of frame order");
        assert_eq!(p.packet_id, 5);
        assert_eq!(p.src, 0x00A1);
        assert_eq!(p.flags, FLAG_AGENT);
        assert_eq!(p.text, msg);
        assert!(r.finalize().is_empty());
    }

    #[test]
    fn dropped_fragment_is_reported_lost() {
        let frames = packetize(b"dropme over the wire", 9, 0, 1, 2, 0).unwrap();
        let mut r = TextReassembler::new();
        for (i, f) in frames.iter().enumerate() {
            if i == 2 {
                continue; // drop one data fragment
            }
            assert!(r.push(f).is_none());
        }
        assert_eq!(r.finalize(), vec![9]); // incomplete => lost
    }

    #[test]
    fn empty_and_unicode_roundtrip() {
        for msg in ["", "agent ⇄ agent 🚀"] {
            let frames = packetize(msg.as_bytes(), 0, 0, 1, 2, 0).unwrap();
            let mut r = TextReassembler::new();
            let mut got = None;
            for f in &frames {
                if let Some(p) = r.push(f) {
                    got = Some(p);
                }
            }
            assert_eq!(got.unwrap().text, msg);
        }
        assert_eq!(channel_id("123456789"), 0x29B1);
        assert_eq!(channel_id(""), BROADCAST);
    }
}
