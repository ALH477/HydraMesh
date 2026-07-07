// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-Cue: the low-latency PCM cue plane of the audio snake.
//!
//! The bidirectional uncompressed-PCM monitor bus on a second cat5e wire — source nodes send
//! tiny (~1 ms) PCM blocks up to the mixer, which returns each node its own cue mix. An
//! *adapter* over the 17-byte DeModFrame quantum (`frame.rs`): one PCM block is fragmented
//! into ordinary CTRL(3) frames carrying raw little-endian PCM. Byte-deterministic across
//! C/Rust/Python — pinned by `Documentation/monitor_vectors.json`. See `DCF_SNAKE_SPEC.md`.
//!
//! Layout (all frames version=1, type=Ctrl(3), big-endian):
//!   seq = block_seq[15:7] (9 bits, 0..511) | frag_idx[6:0] (7 bits, 0..127)
//!   frag_idx 0  descriptor : payload = [block_samples, format, channels, flags]
//!   frag_idx k  data       : payload = pcm[(k-1)*4 .. +4]  (last frame zero-padded)
//!   length = block_samples * channels * bytes_per_sample(format)  (derived, self-describing)

use crate::{crc16_ccitt, Frame, FrameType};
use std::collections::BTreeMap;

// ── L2 constants ────────────────────────────────────────────────────────────
pub const FRAG_BITS: u16 = 7;
pub const FRAG_MASK: u16 = 0x7F;
pub const MAX_FRAGS: usize = 127;
pub const MAX_PAYLOAD: usize = MAX_FRAGS * 4; // 508 bytes / block
pub const MAX_BLOCK_SEQ: u16 = 511;
pub const BLOCK_SEQ_MOD: u64 = 512;
pub const BROADCAST: u16 = 0xFFFF;

pub const FMT_S16: u8 = 0;
pub const FMT_S24: u8 = 1;
pub const FMT_F32: u8 = 2;

pub const FLAG_CUE_RETURN: u8 = 0x01;
pub const FLAG_END: u8 = 0x02;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CueError {
    /// Unknown PCM format id.
    BadFormat,
    /// pcm length does not match the descriptor geometry.
    GeometryMismatch,
    /// Block exceeds the 508-byte cap.
    PayloadTooLarge,
    /// block_seq exceeds the 9-bit field.
    BadBlockSeq,
}

/// Bytes per sample for a format id (None if unknown).
pub fn bytes_per_sample(format_id: u8) -> Option<usize> {
    match format_id {
        FMT_S16 => Some(2),
        FMT_S24 => Some(3),
        FMT_F32 => Some(4),
        _ => None,
    }
}

/// Self-describing PCM byte-length of a block of the given geometry.
pub fn pcm_len(block_samples: u8, format_id: u8, channels: u8) -> Option<usize> {
    Some(block_samples as usize * channels as usize * bytes_per_sample(format_id)?)
}

pub fn channel_id(name: &str) -> u16 {
    if name.is_empty() {
        BROADCAST
    } else {
        crc16_ccitt(name.as_bytes())
    }
}

// ── L2: packetize ───────────────────────────────────────────────────────────
/// Serialise one PCM block into CTRL(3) frames. `pcm.len()` must equal the geometry-derived
/// length.
#[allow(clippy::too_many_arguments)]
pub fn packetize(
    pcm: &[u8],
    block_seq: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    block_samples: u8,
    format_id: u8,
    channels: u8,
    flags: u8,
) -> Result<Vec<[u8; 17]>, CueError> {
    if block_seq > MAX_BLOCK_SEQ {
        return Err(CueError::BadBlockSeq);
    }
    let expect = pcm_len(block_samples, format_id, channels).ok_or(CueError::BadFormat)?;
    if pcm.len() != expect {
        return Err(CueError::GeometryMismatch);
    }
    if pcm.len() > MAX_PAYLOAD {
        return Err(CueError::PayloadTooLarge);
    }
    let frag_total = ((pcm.len() + 3) / 4) as u16;
    let mut frames = Vec::with_capacity(1 + frag_total as usize);

    let desc_seq = block_seq << FRAG_BITS;
    let desc = [block_samples, format_id, channels, flags];
    frames.push(Frame::new(1, FrameType::Ctrl, desc_seq, src, dst, desc, ts_us).encode());

    for k in 1..=frag_total {
        let off = (k as usize - 1) * 4;
        let mut chunk = [0u8; 4];
        let end = (off + 4).min(pcm.len());
        chunk[..end - off].copy_from_slice(&pcm[off..end]);
        let seq = (block_seq << FRAG_BITS) | k;
        frames.push(Frame::new(1, FrameType::Ctrl, seq, src, dst, chunk, ts_us).encode());
    }
    Ok(frames)
}

// ── L2: reassembler ─────────────────────────────────────────────────────────
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CueBlock {
    pub block_seq: u16,
    pub ts_us: u32,
    pub src: u16,
    pub dst: u16,
    pub block_samples: u8,
    pub format: u8,
    pub channels: u8,
    pub flags: u8,
    pub data: Vec<u8>,
}

#[derive(Default)]
struct Slot {
    desc: Option<(u8, u8, u8, u8, usize)>, // (block_samples, format, channels, flags, len)
    ts_us: u32,
    src: u16,
    dst: u16,
    frags: BTreeMap<u16, [u8; 4]>,
}

/// Stateful cue-plane reassembler (CTRL(3) frames). Mirrors the C and Python references.
#[derive(Default)]
pub struct CueReassembler {
    slots: BTreeMap<u16, Slot>,
}

impl CueReassembler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, frame: &[u8; 17]) -> Option<CueBlock> {
        let d = Frame::decode(&frame[..]).ok()?;
        if d.frame_type != FrameType::Ctrl {
            return None;
        }
        let block_seq = d.seq >> FRAG_BITS;
        let frag_idx = d.seq & FRAG_MASK;
        let entry = self.slots.entry(block_seq).or_default();
        entry.ts_us = d.timestamp_us;
        entry.src = d.src_id;
        entry.dst = d.dst_id;
        if frag_idx == 0 {
            if entry.desc.is_none() {
                let (bs, fmt, ch, fl) = (d.payload[0], d.payload[1], d.payload[2], d.payload[3]);
                if let Some(len) = pcm_len(bs, fmt, ch) {
                    if len > 0 && len <= MAX_PAYLOAD {
                        entry.desc = Some((bs, fmt, ch, fl, len));
                    }
                }
            }
        } else {
            entry.frags.entry(frag_idx).or_insert(d.payload);
        }
        self.try_emit(block_seq)
    }

    fn try_emit(&mut self, block_seq: u16) -> Option<CueBlock> {
        let entry = self.slots.get(&block_seq)?;
        let (bs, fmt, ch, fl, len) = entry.desc?;
        let frag_total = ((len + 3) / 4) as u16;
        if (1..=frag_total).any(|k| !entry.frags.contains_key(&k)) {
            return None;
        }
        let mut raw = Vec::with_capacity(frag_total as usize * 4);
        for k in 1..=frag_total {
            raw.extend_from_slice(&entry.frags[&k]);
        }
        raw.truncate(len);
        let (ts_us, src, dst) = (entry.ts_us, entry.src, entry.dst);
        self.slots.remove(&block_seq);
        Some(CueBlock {
            block_seq,
            ts_us,
            src,
            dst,
            block_samples: bs,
            format: fmt,
            channels: ch,
            flags: fl,
            data: raw,
        })
    }

    /// Report every still-incomplete block as lost (ascending block_seq), clearing state.
    pub fn finalize(&mut self) -> Vec<u16> {
        let lost: Vec<u16> = self.slots.keys().copied().collect();
        self.slots.clear();
        lost
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_under_reorder_and_bounds() {
        let n = pcm_len(48, FMT_S16, 2).unwrap();
        let pcm: Vec<u8> = (0..n as u32).map(|i| (i * 5) as u8).collect();
        let frames = packetize(&pcm, 5, 0x010203, 0x00A1, 0xFFFF, 48, FMT_S16, 2, FLAG_CUE_RETURN)
            .unwrap();
        let mut r = CueReassembler::new();
        let mut got = None;
        for f in frames.iter().rev() {
            if let Some(b) = r.push(f) {
                got = Some(b);
            }
        }
        let b = got.unwrap();
        assert_eq!((b.block_seq, b.block_samples, b.format, b.channels), (5, 48, FMT_S16, 2));
        assert_eq!(b.data, pcm);

        assert_eq!(packetize(&[0u8; 10], 0, 0, 1, 1, 48, FMT_S16, 1, 0), Err(CueError::GeometryMismatch));
        assert_eq!(packetize(&vec![0u8; 512], 0, 0, 1, 1, 128, FMT_S16, 2, 0), Err(CueError::PayloadTooLarge));
        assert_eq!(channel_id("123456789"), 0x29B1);
    }
}
