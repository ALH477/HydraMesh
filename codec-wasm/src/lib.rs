// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
//! `dcf-codec-wasm` — the WebAssembly surface over the certified `dcf-wire-codec`.
//!
//! Thin `#[wasm_bindgen]` wrappers only; every byte is produced by the reference
//! codec, so the WASM build stays byte-identical to C/Rust/Python/Go and the
//! 246-vector wire certificate (+ adapter vectors). Names mirror
//! `JS/nodejs/src/*` so the browser speaks the same bare-frame dialect as the
//! Node.js / Python text nodes (frames batched into 32-byte SuperPacks, channel
//! rendezvous on the frame `dst`).
//!
//! Frame groups are returned as a flat `Uint8Array` of N×17 bytes; the JS side
//! slices into 17-byte frames (and pairs them into SuperPacks). Reassemblers are
//! exported as stateful objects — run one per source `src_id`.

use dcf_wire_codec::{audio, fec, game, superpack, text, Frame, FrameType};
use serde::Serialize;
use wasm_bindgen::prelude::*;

// ── helpers ─────────────────────────────────────────────────────────────────

fn flat(frames: Vec<[u8; 17]>) -> Vec<u8> {
    let mut out = Vec::with_capacity(frames.len() * 17);
    for f in frames {
        out.extend_from_slice(&f);
    }
    out
}

fn to_value<T: Serialize>(v: &T) -> JsValue {
    serde_wasm_bindgen::to_value(v).unwrap_or(JsValue::NULL)
}

fn arr17(b: &[u8]) -> Result<[u8; 17], JsError> {
    b.try_into()
        .map_err(|_| JsError::new("frame must be exactly 17 bytes"))
}

// ── frame ───────────────────────────────────────────────────────────────────

/// The certified CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF). Also the
/// frequency-channel hash: `crc("123456789") == 0x29B1`.
#[wasm_bindgen]
pub fn crc16(data: &[u8]) -> u16 {
    dcf_wire_codec::crc16_ccitt(data)
}

/// Map a passphrase to a 16-bit rendezvous channel (empty → broadcast 0xFFFF).
#[wasm_bindgen]
pub fn channel_from_passphrase(name: &str) -> u16 {
    text::channel_id(name)
}

/// Encode a 17-byte DeModFrame. `payload` is padded/truncated to 4 bytes.
#[wasm_bindgen]
pub fn encode_frame(
    version: u8,
    frame_type: u8,
    seq: u16,
    src: u16,
    dst: u16,
    payload: &[u8],
    ts_us: u32,
) -> Result<Vec<u8>, JsError> {
    let ft = match frame_type & 0x0F {
        0 => FrameType::Data,
        1 => FrameType::Ack,
        2 => FrameType::Beacon,
        3 => FrameType::Ctrl,
        _ => return Err(JsError::new("frame_type nibble must be 0..3")),
    };
    let mut p = [0u8; 4];
    let n = payload.len().min(4);
    p[..n].copy_from_slice(&payload[..n]);
    Ok(Frame::new(version, ft, seq, src, dst, p, ts_us).encode().to_vec())
}

#[derive(Serialize)]
struct DecodedFrame {
    valid: bool,
    error: Option<String>,
    version: u8,
    frame_type: u8,
    frame_type_name: &'static str,
    seq: u16,
    src: u16,
    dst: u16,
    payload: Vec<u8>,
    ts_us: u32,
}

/// Decode (and validate) a 17-byte frame; returns a structured object with
/// `valid`/`error` rather than throwing, so the Wire inspector can render
/// rejects too.
#[wasm_bindgen]
pub fn decode_frame(buf: &[u8]) -> JsValue {
    match Frame::decode(buf) {
        Ok(f) => {
            let (id, name) = match f.frame_type {
                FrameType::Data => (0u8, "FData"),
                FrameType::Ack => (1, "FAck"),
                FrameType::Beacon => (2, "FBeacon"),
                FrameType::Ctrl => (3, "FCtrl"),
            };
            to_value(&DecodedFrame {
                valid: true,
                error: None,
                version: f.version,
                frame_type: id,
                frame_type_name: name,
                seq: f.seq,
                src: f.src_id,
                dst: f.dst_id,
                payload: f.payload.to_vec(),
                ts_us: f.timestamp_us,
            })
        }
        Err(e) => to_value(&DecodedFrame {
            valid: false,
            error: Some(format!("{e:?}")),
            version: 0,
            frame_type: 0,
            frame_type_name: "",
            seq: 0,
            src: 0,
            dst: 0,
            payload: vec![],
            ts_us: 0,
        }),
    }
}

/// True iff the 17-byte slice is a structurally valid frame (sync + CRC).
#[wasm_bindgen]
pub fn is_valid(buf: &[u8]) -> bool {
    Frame::is_valid(buf)
}

// ── superpack ───────────────────────────────────────────────────────────────

/// Pack two 17-byte frames into one 32-byte SuperPack.
#[wasm_bindgen]
pub fn superpack_pack(a: &[u8], b: &[u8]) -> Result<Vec<u8>, JsError> {
    let (fa, fb) = (arr17(a)?, arr17(b)?);
    superpack::pack(&fa, &fb)
        .map(|p| p.to_vec())
        .map_err(|e| JsError::new(&format!("{e:?}")))
}

/// Unpack a 32-byte SuperPack into 34 bytes (two 17-byte frames, concatenated).
#[wasm_bindgen]
pub fn superpack_unpack(buf: &[u8]) -> Result<Vec<u8>, JsError> {
    superpack::unpack(buf)
        .map(|(a, b)| {
            let mut out = Vec::with_capacity(34);
            out.extend_from_slice(&a);
            out.extend_from_slice(&b);
            out
        })
        .map_err(|e| JsError::new(&format!("{e:?}")))
}

/// True iff the buffer is a SuperPack (32 bytes, sync + sflags 0x15).
#[wasm_bindgen]
pub fn is_superpack(buf: &[u8]) -> bool {
    superpack::is_superpack(buf)
}

// ── text (DCF-Text, DATA frames) ──────────────────────────────────────────────

/// Fragment a UTF-8 message into DATA frames (flat N×17 bytes).
#[wasm_bindgen]
pub fn text_packetize(
    payload: &[u8],
    packet_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
) -> Result<Vec<u8>, JsError> {
    text::packetize(payload, packet_id, ts_us, src, dst, flags)
        .map(flat)
        .map_err(|e| JsError::new(&format!("{e:?}")))
}

#[derive(Serialize)]
struct TextPacketJs {
    packet_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
    text: String,
}

/// Stateful per-source text reassembler. `push` returns a completed message
/// object (or `null`); `finalize` returns the still-incomplete packet ids.
#[wasm_bindgen]
pub struct TextReassembler(text::TextReassembler);

#[wasm_bindgen]
impl TextReassembler {
    #[wasm_bindgen(constructor)]
    pub fn new() -> TextReassembler {
        TextReassembler(text::TextReassembler::new())
    }

    pub fn push(&mut self, frame: &[u8]) -> Result<JsValue, JsError> {
        let f = arr17(frame)?;
        Ok(match self.0.push(&f) {
            Some(p) => to_value(&TextPacketJs {
                packet_id: p.packet_id,
                ts_us: p.ts_us,
                src: p.src,
                dst: p.dst,
                flags: p.flags,
                text: p.text,
            }),
            None => JsValue::NULL,
        })
    }

    pub fn finalize(&mut self) -> Vec<u16> {
        self.0.finalize()
    }
}

impl Default for TextReassembler {
    fn default() -> Self {
        Self::new()
    }
}

// ── game (DCF-Game, DATA frames) ──────────────────────────────────────────────

/// Fragment a game message (snapshot / input / event / join) into DATA frames.
#[wasm_bindgen]
pub fn game_packetize(
    msg_type_id: u8,
    payload: &[u8],
    packet_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
) -> Result<Vec<u8>, JsError> {
    game::packetize(msg_type_id, payload, packet_id, ts_us, src, dst, flags)
        .map(flat)
        .map_err(|e| JsError::new(&format!("{e:?}")))
}

#[derive(Serialize)]
struct GamePacketJs {
    packet_id: u16,
    ts_us: u32,
    msg_type_id: u8,
    flags: u8,
    payload: Vec<u8>,
}

/// Stateful per-source game reassembler.
#[wasm_bindgen]
pub struct GameReassembler(game::GameReassembler);

#[wasm_bindgen]
impl GameReassembler {
    #[wasm_bindgen(constructor)]
    pub fn new() -> GameReassembler {
        GameReassembler(game::GameReassembler::new())
    }

    pub fn push(&mut self, frame: &[u8]) -> Result<JsValue, JsError> {
        let f = arr17(frame)?;
        Ok(match self.0.push(&f) {
            Some(p) => to_value(&GamePacketJs {
                packet_id: p.packet_id,
                ts_us: p.ts_us,
                msg_type_id: p.msg_type_id,
                flags: p.flags,
                payload: p.payload,
            }),
            None => JsValue::NULL,
        })
    }

    pub fn finalize(&mut self) -> Vec<u16> {
        self.0.finalize()
    }
}

impl Default for GameReassembler {
    fn default() -> Self {
        Self::new()
    }
}

/// Pack a player snapshot (positions in metres, yaw u16) into 14 certified bytes.
#[wasm_bindgen]
pub fn snapshot_pack(x: f32, y: f32, z: f32, vx: f32, vy: f32, vz: f32, yaw: u16) -> Vec<u8> {
    game::snapshot_pack(&game::Snapshot { x, y, z, vx, vy, vz, yaw }).to_vec()
}

#[derive(Serialize)]
struct SnapshotJs {
    x: f32,
    y: f32,
    z: f32,
    vx: f32,
    vy: f32,
    vz: f32,
    yaw: u16,
}

/// Unpack 14 snapshot bytes.
#[wasm_bindgen]
pub fn snapshot_unpack(b: &[u8]) -> Result<JsValue, JsError> {
    let buf: [u8; game::SNAPSHOT_LEN] = b
        .try_into()
        .map_err(|_| JsError::new("snapshot must be 14 bytes"))?;
    let s = game::snapshot_unpack(&buf);
    Ok(to_value(&SnapshotJs {
        x: s.x,
        y: s.y,
        z: s.z,
        vx: s.vx,
        vy: s.vy,
        vz: s.vz,
        yaw: s.yaw,
    }))
}

/// Pack an input frame (tick u32 + buttons u16) into 6 certified bytes.
#[wasm_bindgen]
pub fn input_pack(tick: u32, buttons: u16) -> Vec<u8> {
    game::input_pack(&game::Input { tick, buttons }).to_vec()
}

#[derive(Serialize)]
struct InputJs {
    tick: u32,
    buttons: u16,
}

/// Unpack 6 input bytes.
#[wasm_bindgen]
pub fn input_unpack(b: &[u8]) -> Result<JsValue, JsError> {
    let buf: [u8; game::INPUT_LEN] = b
        .try_into()
        .map_err(|_| JsError::new("input must be 6 bytes"))?;
    let i = game::input_unpack(&buf);
    Ok(to_value(&InputJs { tick: i.tick, buttons: i.buttons }))
}

// ── audio (DCF-Audio L2, CTRL frames; PCM-diag codec in-WASM) ─────────────────

/// Fragment a 20 ms audio block into CTRL frames (flat N×17 bytes).
#[wasm_bindgen]
pub fn audio_packetize(
    codec_id: u8,
    payload: &[u8],
    packet_id: u16,
    ts_us: u32,
    src: u16,
    dst: u16,
    flags: u8,
) -> Result<Vec<u8>, JsError> {
    audio::packetize(codec_id, payload, packet_id, ts_us, src, dst, flags)
        .map(flat)
        .map_err(|e| JsError::new(&format!("{e:?}")))
}

#[derive(Serialize)]
struct AudioPacketJs {
    packet_id: u16,
    ts_us: u32,
    codec_id: u8,
    flags: u8,
    payload: Vec<u8>,
}

/// Stateful per-source audio reassembler.
#[wasm_bindgen]
pub struct AudioReassembler(audio::AudioReassembler);

#[wasm_bindgen]
impl AudioReassembler {
    #[wasm_bindgen(constructor)]
    pub fn new() -> AudioReassembler {
        AudioReassembler(audio::AudioReassembler::new())
    }

    pub fn push(&mut self, frame: &[u8]) -> Result<JsValue, JsError> {
        let f = arr17(frame)?;
        Ok(match self.0.push(&f) {
            Some(p) => to_value(&AudioPacketJs {
                packet_id: p.packet_id,
                ts_us: p.ts_us,
                codec_id: p.codec_id,
                flags: p.flags,
                payload: p.payload,
            }),
            None => JsValue::NULL,
        })
    }

    pub fn finalize(&mut self) -> Vec<u16> {
        self.0.finalize()
    }
}

impl Default for AudioReassembler {
    fn default() -> Self {
        Self::new()
    }
}

/// PCM-diag (codec id 1) encode: f32 samples → certified 6 kHz / 8-bit bytes.
#[wasm_bindgen]
pub fn pcm_diag_encode(samples: &[f32]) -> Vec<u8> {
    audio::pcm_diag_encode(samples)
}

/// PCM-diag decode: bytes → f32 samples.
#[wasm_bindgen]
pub fn pcm_diag_decode(data: &[u8]) -> Vec<f32> {
    audio::pcm_diag_decode(data)
}

// ── fec (multi-codeword Reed-Solomon message layer) ──────────────────────────

/// Reed-Solomon encode a message (self-protecting header + parity).
#[wasm_bindgen]
pub fn fec_encode(msg: &[u8], nparity: usize) -> Vec<u8> {
    fec::encode_message(msg, nparity)
}

/// Reed-Solomon decode/repair a message blob; errors if unrecoverable.
#[wasm_bindgen]
pub fn fec_decode(blob: &[u8]) -> Result<Vec<u8>, JsError> {
    fec::decode_message(blob)
        .map(|(msg, _n)| msg)
        .map_err(|_| JsError::new("FEC: unrecoverable"))
}
