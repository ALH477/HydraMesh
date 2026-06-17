// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-Audio: collaborative audio over the DeModFrame wire.
//!
//! Audio is an *adapter* over the 17-byte DeModFrame quantum (`frame.rs`), not a new
//! wire format. A 20 ms codec block is serialised into `1 + frag_total` ordinary CTRL
//! frames. This L2 framing is codec-agnostic and byte-deterministic across C/Rust/Python
//! — it is pinned by `Documentation/audio_vectors.json`. See `Documentation/DCF_AUDIO_SPEC.md`.
//!
//! Layout (all frames version=1, type=CTRL(3), big-endian):
//!   seq = packet_id[15:5] (11 bits) | frag_idx[4:0] (5 bits)
//!   frag_idx 0  descriptor : payload = [payload_len, frag_total, codec_id, flags]
//!   frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
//!   frag_total = ceil(payload_len/4)  (<= 31  =>  payload_len <= 124 bytes / 20 ms)

use crate::{Frame, FrameType};
use std::collections::BTreeMap;

// ── L2 constants ────────────────────────────────────────────────────────────
pub const FRAG_BITS: u16 = 5;
pub const FRAG_MASK: u16 = 0x1F;
pub const MAX_FRAGS: usize = 31;
pub const MAX_PAYLOAD: usize = MAX_FRAGS * 4; // 124 bytes / 20 ms block
pub const MAX_PACKET_ID: u16 = (1 << (16 - FRAG_BITS)) - 1; // 2047

// Codec ids (profiles in DCF_AUDIO_SPEC.md).
pub const CODEC_OPUS: u8 = 0;
pub const CODEC_PCM_DIAG: u8 = 1;
pub const CODEC_FAUST_PM: u8 = 2;
// id 3 reserved (declined low-bitrate speech codec)

// Descriptor flag bits.
pub const FLAG_END_TALKSPURT: u8 = 0x01;
pub const FLAG_PM_VOICE: u8 = 0x02;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AudioError {
    /// Codec block exceeds the 124-byte / 20 ms cap.
    PayloadTooLarge,
    /// packet_id exceeds the 11-bit field.
    BadPacketId,
}

// ── L2: packetize ───────────────────────────────────────────────────────────
/// Serialise one codec block into DeModFrame CTRL frames (descriptor first, then
/// data fragments in order). Each frame is a fully valid 17-byte DeModFrame.
pub fn packetize(
    codec_id: u8,
    payload: &[u8],
    packet_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
) -> Result<Vec<[u8; 17]>, AudioError> {
    if payload.len() > MAX_PAYLOAD {
        return Err(AudioError::PayloadTooLarge);
    }
    if packet_id > MAX_PACKET_ID {
        return Err(AudioError::BadPacketId);
    }
    let frag_total = ((payload.len() + 3) / 4) as u8;
    let mut frames = Vec::with_capacity(1 + frag_total as usize);

    // frag_idx 0 — descriptor
    let desc_seq = packet_id << FRAG_BITS;
    let desc = [payload.len() as u8, frag_total, codec_id, flags];
    frames.push(Frame::new(1, FrameType::Ctrl, desc_seq, src, dst, desc, ts_us).encode());

    // frag_idx 1..frag_total — data, last chunk zero-padded
    for k in 1..=frag_total {
        let off = (k as usize - 1) * 4;
        let mut chunk = [0u8; 4];
        let end = (off + 4).min(payload.len());
        chunk[..end - off].copy_from_slice(&payload[off..end]);
        let seq = (packet_id << FRAG_BITS) | k as u16;
        frames.push(Frame::new(1, FrameType::Ctrl, seq, src, dst, chunk, ts_us).encode());
    }
    Ok(frames)
}

// ── L2: reassembler ─────────────────────────────────────────────────────────
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AudioPacket {
    pub packet_id: u16,
    pub ts_us: u32,
    pub codec_id: u8,
    pub flags: u8,
    pub payload: Vec<u8>,
}

#[derive(Default)]
struct Slot {
    desc: Option<(u8, u8, u8, u8)>, // (payload_len, frag_total, codec_id, flags)
    ts_us: u32,
    frags: BTreeMap<u8, [u8; 4]>,
}

/// Stateful reassembler. `push` emits a completed packet as soon as its descriptor
/// and every data fragment have arrived; duplicates are ignored; `finalize` reports
/// any still-incomplete packet as lost. Mirrors the C and Python references.
#[derive(Default)]
pub struct AudioReassembler {
    slots: BTreeMap<u16, Slot>,
}

impl AudioReassembler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, frame: &[u8; 17]) -> Option<AudioPacket> {
        let d = Frame::decode(&frame[..]).ok()?;
        if d.frame_type != FrameType::Ctrl {
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

    fn try_emit(&mut self, packet_id: u16) -> Option<AudioPacket> {
        let entry = self.slots.get(&packet_id)?;
        let (payload_len, frag_total, codec_id, flags) = entry.desc?;
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
        Some(AudioPacket { packet_id, ts_us, codec_id, flags, payload: raw })
    }

    /// Report every still-incomplete packet as lost (ascending packet_id), clearing state.
    pub fn finalize(&mut self) -> Vec<u16> {
        let lost: Vec<u16> = self.slots.keys().copied().collect(); // BTreeMap => sorted
        self.slots.clear();
        lost
    }
}

// ── L1: codec interface + registry ──────────────────────────────────────────
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodecProfile {
    pub codec_id: u8,
    pub sample_rate: u32,
    pub channels: u8,
    pub block_samples: u16,
    pub max_payload: u16,
}

/// One pluggable codec keyed by `codec_id`. Output is opaque to the L2 framing.
pub trait AudioCodec {
    fn profile(&self) -> CodecProfile;
    fn encode(&mut self, pcm: &[f32]) -> Vec<u8>;
    fn decode(&mut self, data: &[u8]) -> Vec<f32>;
    /// Packet-loss concealment: synthesise one block with no input.
    fn plc(&mut self) -> Vec<f32>;
}

/// Return a codec instance for `codec_id`, or `None` if not built in this config.
pub fn codec_for(codec_id: u8) -> Option<Box<dyn AudioCodec>> {
    match codec_id {
        CODEC_PCM_DIAG => Some(Box::new(PcmDiag)),
        #[cfg(feature = "opus")]
        CODEC_OPUS => opus_codec::OpusCodec::new().map(|c| Box::new(c) as Box<dyn AudioCodec>),
        #[cfg(feature = "pm")]
        CODEC_FAUST_PM => Some(Box::new(pm::FaustPmCodec::new())),
        _ => None,
    }
}

// ── L1: PCM-diagnostic codec (id 1) — 6 kHz 8-bit mono, 120 B/block ─────────
pub const PCM_DIAG_RATE: u32 = 6000;
pub const PCM_DIAG_BLOCK: usize = 120;

/// float [-1,1] -> unsigned 8-bit (mid 128). decode∘encode is byte-lossless.
pub fn pcm_diag_encode(samples: &[f32]) -> Vec<u8> {
    samples
        .iter()
        .map(|&s| ((s * 128.0).round() as i32 + 128).clamp(0, 255) as u8)
        .collect()
}
/// unsigned 8-bit -> float [-1,1].
pub fn pcm_diag_decode(data: &[u8]) -> Vec<f32> {
    data.iter().map(|&b| (b as f32 - 128.0) / 128.0).collect()
}

pub struct PcmDiag;
impl AudioCodec for PcmDiag {
    fn profile(&self) -> CodecProfile {
        CodecProfile {
            codec_id: CODEC_PCM_DIAG,
            sample_rate: PCM_DIAG_RATE,
            channels: 1,
            block_samples: PCM_DIAG_BLOCK as u16,
            max_payload: PCM_DIAG_BLOCK as u16,
        }
    }
    fn encode(&mut self, pcm: &[f32]) -> Vec<u8> {
        pcm_diag_encode(pcm)
    }
    fn decode(&mut self, data: &[u8]) -> Vec<f32> {
        pcm_diag_decode(data)
    }
    fn plc(&mut self) -> Vec<f32> {
        vec![0.0; PCM_DIAG_BLOCK]
    }
}

// ── L1: PM (Faust phase-mod, id 2) — 8-byte parameter layout (certified) ────
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct PmParams {
    pub f0: u16,
    pub amp: u8,
    pub mod_index: u8,
    pub mod_ratio: u8,
    pub bright: u8,
    pub env: u8,
    pub flags: u8,
}

pub fn pm_pack(p: &PmParams) -> [u8; 8] {
    [
        (p.f0 >> 8) as u8,
        p.f0 as u8,
        p.amp,
        p.mod_index,
        p.mod_ratio,
        p.bright,
        p.env,
        p.flags,
    ]
}
pub fn pm_unpack(b: &[u8; 8]) -> PmParams {
    PmParams {
        f0: ((b[0] as u16) << 8) | b[1] as u16,
        amp: b[2],
        mod_index: b[3],
        mod_ratio: b[4],
        bright: b[5],
        env: b[6],
        flags: b[7],
    }
}

// ── L1: Opus codec (id 0) — feature "opus" ──────────────────────────────────
#[cfg(feature = "opus")]
mod opus_codec {
    use super::*;
    use opus::{Application, Channels, Decoder, Encoder};

    pub const OPUS_RATE: u32 = 48000;
    pub const OPUS_BLOCK: usize = 960; // 20 ms @ 48 kHz

    pub struct OpusCodec {
        enc: Encoder,
        dec: Decoder,
    }
    impl OpusCodec {
        pub fn new() -> Option<Self> {
            let mut enc = Encoder::new(OPUS_RATE, Channels::Mono, Application::Audio).ok()?;
            let _ = enc.set_bitrate(opus::Bitrate::Bits(24000));
            let dec = Decoder::new(OPUS_RATE, Channels::Mono).ok()?;
            Some(Self { enc, dec })
        }
    }
    impl AudioCodec for OpusCodec {
        fn profile(&self) -> CodecProfile {
            CodecProfile {
                codec_id: CODEC_OPUS,
                sample_rate: OPUS_RATE,
                channels: 1,
                block_samples: OPUS_BLOCK as u16,
                max_payload: MAX_PAYLOAD as u16,
            }
        }
        fn encode(&mut self, pcm: &[f32]) -> Vec<u8> {
            let mut out = vec![0u8; MAX_PAYLOAD];
            match self.enc.encode_float(pcm, &mut out) {
                Ok(n) => {
                    out.truncate(n);
                    out
                }
                Err(_) => Vec::new(),
            }
        }
        fn decode(&mut self, data: &[u8]) -> Vec<f32> {
            let mut pcm = vec![0.0f32; OPUS_BLOCK];
            match self.dec.decode_float(data, &mut pcm, false) {
                Ok(n) => {
                    pcm.truncate(n);
                    pcm
                }
                Err(_) => vec![0.0; OPUS_BLOCK],
            }
        }
        fn plc(&mut self) -> Vec<f32> {
            // An empty input slice tells libopus to conceal one lost packet.
            let mut pcm = vec![0.0f32; OPUS_BLOCK];
            let _ = self.dec.decode_float(&[], &mut pcm, false);
            pcm
        }
    }
}

// ── L1: Faust phase-mod synthesis (id 2) — feature "pm" ─────────────────────
#[cfg(feature = "pm")]
mod pm {
    use super::*;

    pub const PM_RATE: u32 = 48000;
    pub const PM_BLOCK: usize = 960;

    extern "C" {
        // Provided by codec/faust/dcf_pm_codec.gen.c (compiled by build.rs).
        // Synthesises `n` samples from the 8-byte param block.
        fn dcf_pm_synth_block_ffi(params8: *const u8, out: *mut f32, n: i32);
    }

    pub struct FaustPmCodec;
    impl FaustPmCodec {
        pub fn new() -> Self {
            FaustPmCodec
        }
    }
    impl AudioCodec for FaustPmCodec {
        fn profile(&self) -> CodecProfile {
            CodecProfile {
                codec_id: CODEC_FAUST_PM,
                sample_rate: PM_RATE,
                channels: 1,
                block_samples: PM_BLOCK as u16,
                max_payload: 8,
            }
        }
        fn encode(&mut self, _pcm: &[f32]) -> Vec<u8> {
            // Host analysis fills PmParams; here we emit a neutral block. Real analysis
            // lives in the demo/SDK and packs via pm_pack().
            pm_pack(&PmParams::default()).to_vec()
        }
        fn decode(&mut self, data: &[u8]) -> Vec<f32> {
            if data.len() != 8 {
                return vec![0.0; PM_BLOCK];
            }
            let mut out = vec![0.0f32; PM_BLOCK];
            unsafe { dcf_pm_synth_block_ffi(data.as_ptr(), out.as_mut_ptr(), PM_BLOCK as i32) };
            out
        }
        fn plc(&mut self) -> Vec<f32> {
            vec![0.0; PM_BLOCK]
        }
    }
}
