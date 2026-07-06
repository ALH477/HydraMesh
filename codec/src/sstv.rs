// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-SSTV: slow-scan television (still-image) transport over the DeModFrame wire.
//!
//! Image traffic is an *adapter* over the 17-byte DeModFrame quantum (`frame.rs`), not a
//! new wire format — exactly like DCF-Text (`text.rs`) and DCF-Game (`game.rs`). One still
//! image is fragmented into `1 + frag_total` ordinary DATA frames whose 4-byte payloads
//! carry the opaque image bytes. This L2 framing is byte-deterministic across
//! C/Rust/Python/Go/Node — it is pinned by `Documentation/sstv_vectors.json`. See
//! `Documentation/DCF_SSTV_SPEC.md`.
//!
//! Layout (all frames version=1, type=DATA(0), big-endian):
//!   seq = image_id[15:11] (5 bits, 0..31) | frag_idx[10:0] (11 bits, 0..2047)
//!   frag_idx 0  descriptor : payload = [len_hi, len_lo, format_id, flags]
//!   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
//!   frag_total = ceil(len/4)  (<= 2047  =>  len <= 8188 bytes / image)
//!
//! Text and game also ride DATA(0) but use different seq splits (text 6:10, game 11:5,
//! sstv 5:11); the descriptor that opens every image tells a receiver which adapter owns
//! the id, and a node runs exactly one reassembler per `dst` channel. The image bytes are
//! opaque; `format_id` is a hint (JPEG/PNG/raw) that never changes these vectors.

use crate::{crc16_ccitt, Frame, FrameType};
use std::collections::BTreeMap;

// ── L2 constants ────────────────────────────────────────────────────────────
pub const FRAG_BITS: u16 = 11;
pub const FRAG_MASK: u16 = 0x7FF;
pub const MAX_FRAGS: usize = 2047;
pub const MAX_PAYLOAD: usize = MAX_FRAGS * 4; // 8188 bytes / image
pub const MAX_IMAGE_ID: u16 = (1 << (16 - FRAG_BITS)) - 1; // 31
pub const BROADCAST: u16 = 0xFFFF;

// Image format ids — opaque hints (L2 never parses the bytes).
pub const FMT_RAW: u8 = 0;
pub const FMT_JPEG: u8 = 1;
pub const FMT_PNG: u8 = 2;
pub const FMT_WEBP: u8 = 3;
pub const FMT_RGB565: u8 = 4;

// Descriptor flag bits — opaque to L2 (they do not change the framing bytes).
pub const FLAG_MORE: u8 = 0x01;
pub const FLAG_KEYFRAME: u8 = 0x02;
pub const FLAG_RELIABLE: u8 = 0x04;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SstvError {
    /// Image exceeds the 8188-byte cap.
    PayloadTooLarge,
    /// image_id exceeds the 5-bit field.
    BadImageId,
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
/// Serialise one image (its raw opaque bytes) into DeModFrame DATA frames (descriptor
/// first, then data fragments in order). Each frame is a fully valid 17-byte DeModFrame.
pub fn packetize(
    payload: &[u8],
    image_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    format_id: u8,
    flags: u8,
) -> Result<Vec<[u8; 17]>, SstvError> {
    if payload.len() > MAX_PAYLOAD {
        return Err(SstvError::PayloadTooLarge);
    }
    if image_id > MAX_IMAGE_ID {
        return Err(SstvError::BadImageId);
    }
    let frag_total = ((payload.len() + 3) / 4) as u16;
    let mut frames = Vec::with_capacity(1 + frag_total as usize);

    // frag_idx 0 — descriptor (2-byte big-endian length so the last fragment can be un-padded)
    let desc_seq = image_id << FRAG_BITS;
    let len = payload.len() as u16;
    let desc = [(len >> 8) as u8, (len & 0xFF) as u8, format_id, flags];
    frames.push(Frame::new(1, FrameType::Data, desc_seq, src, dst, desc, ts_us).encode());

    // frag_idx 1..frag_total — data, last chunk zero-padded
    for k in 1..=frag_total {
        let off = (k as usize - 1) * 4;
        let mut chunk = [0u8; 4];
        let end = (off + 4).min(payload.len());
        chunk[..end - off].copy_from_slice(&payload[off..end]);
        let seq = (image_id << FRAG_BITS) | k;
        frames.push(Frame::new(1, FrameType::Data, seq, src, dst, chunk, ts_us).encode());
    }
    Ok(frames)
}

// ── L2: reassembler ─────────────────────────────────────────────────────────
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SstvImage {
    pub image_id: u16,
    pub ts_us: u32,
    pub src: u16,
    pub dst: u16,
    pub format_id: u8,
    pub flags: u8,
    pub data: Vec<u8>,
}

#[derive(Default)]
struct Slot {
    desc: Option<(u16, u8, u8)>, // (payload_len, format_id, flags)
    ts_us: u32,
    src: u16,
    dst: u16,
    frags: BTreeMap<u16, [u8; 4]>,
}

/// Stateful reassembler. `push` emits a completed image as soon as its descriptor and every
/// data fragment have arrived; duplicates are ignored; `finalize` reports any still-incomplete
/// image as lost. Mirrors the C, Python and Go references. The reassembler is channel-agnostic
/// — channel filtering, if any, is the caller's job.
#[derive(Default)]
pub struct SstvReassembler {
    slots: BTreeMap<u16, Slot>,
}

impl SstvReassembler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, frame: &[u8; 17]) -> Option<SstvImage> {
        let d = Frame::decode(&frame[..]).ok()?;
        if d.frame_type != FrameType::Data {
            return None;
        }
        let image_id = d.seq >> FRAG_BITS;
        let frag_idx = d.seq & FRAG_MASK;
        let entry = self.slots.entry(image_id).or_default();
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
        self.try_emit(image_id)
    }

    fn try_emit(&mut self, image_id: u16) -> Option<SstvImage> {
        let entry = self.slots.get(&image_id)?;
        let (len, format_id, flags) = entry.desc?;
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
        self.slots.remove(&image_id);
        Some(SstvImage {
            image_id,
            ts_us,
            src,
            dst,
            format_id,
            flags,
            data: raw,
        })
    }

    /// Report every still-incomplete image as lost (ascending image_id), clearing state.
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
        let img: Vec<u8> = (0..300u32).map(|i| (i * 7) as u8).collect(); // spans many fragments
        let frames =
            packetize(&img, 5, 0x010203, 0x00A1, 0xFFFF, FMT_JPEG, FLAG_KEYFRAME).unwrap();
        assert_eq!(frames.len(), 1 + (img.len() + 3) / 4);
        // feed in reverse order; reassembly must still recover the exact image
        let mut r = SstvReassembler::new();
        let mut got = None;
        for f in frames.iter().rev() {
            if let Some(p) = r.push(f) {
                got = Some(p);
            }
        }
        let p = got.expect("image reassembles regardless of frame order");
        assert_eq!(p.image_id, 5);
        assert_eq!(p.src, 0x00A1);
        assert_eq!(p.format_id, FMT_JPEG);
        assert_eq!(p.flags, FLAG_KEYFRAME);
        assert_eq!(p.data, img);
        assert!(r.finalize().is_empty());
    }

    #[test]
    fn dropped_fragment_is_reported_lost() {
        let frames = packetize(&[0u8; 20], 9, 0, 1, 2, FMT_RAW, 0).unwrap();
        let mut r = SstvReassembler::new();
        for (i, f) in frames.iter().enumerate() {
            if i == 2 {
                continue; // drop one data fragment
            }
            assert!(r.push(f).is_none());
        }
        assert_eq!(r.finalize(), vec![9]); // incomplete => lost
    }

    #[test]
    fn empty_roundtrip_and_bounds() {
        let frames = packetize(&[], 0, 0, 1, 2, FMT_RAW, 0).unwrap();
        let mut r = SstvReassembler::new();
        let mut got = None;
        for f in &frames {
            if let Some(p) = r.push(f) {
                got = Some(p);
            }
        }
        assert!(got.unwrap().data.is_empty());
        assert_eq!(packetize(&vec![0u8; MAX_PAYLOAD + 1], 0, 0, 1, 2, 0, 0), Err(SstvError::PayloadTooLarge));
        assert_eq!(packetize(&[0u8; 4], MAX_IMAGE_ID + 1, 0, 1, 2, 0, 0), Err(SstvError::BadImageId));
        assert_eq!(channel_id("123456789"), 0x29B1);
        assert_eq!(channel_id(""), BROADCAST);
    }
}
