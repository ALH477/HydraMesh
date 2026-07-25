// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-Pipe — the control-plane codec for lossless bulk transfer.
//!
//! DCF frames carry a small control vocabulary (OPEN/CREDIT/SACK/NACK/DONE/
//! ABORT) that steers a dumb UDP data lane of numbered chunks; the flow control,
//! loss recovery, and completion logic live in these messages. Byte-identical to
//! `python/MCP/pipelab_core.py` and `codec/demod_pipe.h`, pinned by
//! `Documentation/pipe_vectors.json`. The wire certificate is untouched — these
//! are payloads over ordinary frames, not a new frame format.
//!
//! All multi-byte fields are big-endian.

pub const PIPE_VERSION: u8 = 1;

pub const PIPE_OPEN: u8 = 0;
pub const PIPE_CREDIT: u8 = 1;
pub const PIPE_SACK: u8 = 2;
pub const PIPE_NACK: u8 = 3;
pub const PIPE_DONE: u8 = 4;
pub const PIPE_ABORT: u8 = 5;

pub const ABORT_CHECKSUM: u8 = 0;
pub const ABORT_TIMEOUT: u8 = 1;
pub const ABORT_POLICY: u8 = 2;
pub const ABORT_PEER: u8 = 3;

pub const CHUNK_HDR_LEN: usize = 6;

/// FNV-1a 32-bit — the whole-object checksum carried in OPEN.
pub fn fnv1a32(data: &[u8]) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for &b in data {
        h ^= b as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

/// Message type of a control payload, or -1 if empty.
pub fn msg_type(buf: &[u8]) -> i32 {
    if buf.is_empty() { -1 } else { buf[0] as i32 }
}

fn rd16(b: &[u8], o: usize) -> u16 {
    ((b[o] as u16) << 8) | b[o + 1] as u16
}
fn rd32(b: &[u8], o: usize) -> u32 {
    ((b[o] as u32) << 24) | ((b[o + 1] as u32) << 16) | ((b[o + 2] as u32) << 8) | b[o + 3] as u32
}

// ── OPEN ────────────────────────────────────────────────────────────────────
pub fn pack_open(session_id: u16, total_len: u32, chunk_size: u16, checksum: u32) -> Vec<u8> {
    let mut o = vec![PIPE_OPEN, PIPE_VERSION];
    o.extend_from_slice(&session_id.to_be_bytes());
    o.extend_from_slice(&total_len.to_be_bytes());
    o.extend_from_slice(&chunk_size.to_be_bytes());
    o.extend_from_slice(&checksum.to_be_bytes());
    o
}
/// Returns (session_id, total_len, chunk_size, checksum).
pub fn unpack_open(b: &[u8]) -> Option<(u16, u32, u16, u32)> {
    if b.len() < 14 || b[0] != PIPE_OPEN || b[1] != PIPE_VERSION {
        return None;
    }
    Some((rd16(b, 2), rd32(b, 4), rd16(b, 8), rd32(b, 10)))
}

// ── CREDIT ──────────────────────────────────────────────────────────────────
pub fn pack_credit(session_id: u16, credit: u32) -> Vec<u8> {
    let mut o = vec![PIPE_CREDIT, PIPE_VERSION];
    o.extend_from_slice(&session_id.to_be_bytes());
    o.extend_from_slice(&credit.to_be_bytes());
    o
}
pub fn unpack_credit(b: &[u8]) -> Option<(u16, u32)> {
    if b.len() < 8 || b[0] != PIPE_CREDIT || b[1] != PIPE_VERSION {
        return None;
    }
    Some((rd16(b, 2), rd32(b, 4)))
}

// ── SACK ────────────────────────────────────────────────────────────────────
pub fn pack_sack(session_id: u16, base: u32, bitmap: &[u8]) -> Vec<u8> {
    assert!(bitmap.len() <= 255, "sack bitmap too long");
    let mut o = vec![PIPE_SACK, PIPE_VERSION];
    o.extend_from_slice(&session_id.to_be_bytes());
    o.extend_from_slice(&base.to_be_bytes());
    o.push(bitmap.len() as u8);
    o.extend_from_slice(bitmap);
    o
}
/// Returns (session_id, base, bitmap).
pub fn unpack_sack(b: &[u8]) -> Option<(u16, u32, Vec<u8>)> {
    if b.len() < 9 || b[0] != PIPE_SACK || b[1] != PIPE_VERSION {
        return None;
    }
    let nbytes = b[8] as usize;
    if b.len() < 9 + nbytes {
        return None;
    }
    Some((rd16(b, 2), rd32(b, 4), b[9..9 + nbytes].to_vec()))
}
pub fn sack_has(bitmap: &[u8], base: u32, seq: u32) -> bool {
    if seq < base {
        return true;
    }
    let off = (seq - base) as usize;
    let (byte, bit) = (off / 8, off % 8);
    byte < bitmap.len() && (bitmap[byte] & (1 << bit)) != 0
}

// ── NACK ────────────────────────────────────────────────────────────────────
pub fn pack_nack(session_id: u16, missing: &[u32]) -> Vec<u8> {
    assert!(missing.len() <= 255, "too many NACK entries");
    let mut o = vec![PIPE_NACK, PIPE_VERSION];
    o.extend_from_slice(&session_id.to_be_bytes());
    o.push(missing.len() as u8);
    for &s in missing {
        o.extend_from_slice(&s.to_be_bytes());
    }
    o
}
pub fn unpack_nack(b: &[u8]) -> Option<(u16, Vec<u32>)> {
    if b.len() < 5 || b[0] != PIPE_NACK || b[1] != PIPE_VERSION {
        return None;
    }
    let n = b[4] as usize;
    if b.len() < 5 + 4 * n {
        return None;
    }
    Some((rd16(b, 2), (0..n).map(|i| rd32(b, 5 + 4 * i)).collect()))
}

// ── DONE / ABORT ────────────────────────────────────────────────────────────
pub fn pack_done(session_id: u16) -> Vec<u8> {
    let mut o = vec![PIPE_DONE, PIPE_VERSION];
    o.extend_from_slice(&session_id.to_be_bytes());
    o
}
pub fn unpack_done(b: &[u8]) -> Option<u16> {
    if b.len() < 4 || b[0] != PIPE_DONE || b[1] != PIPE_VERSION {
        return None;
    }
    Some(rd16(b, 2))
}
pub fn pack_abort(session_id: u16, reason: u8) -> Vec<u8> {
    let mut o = vec![PIPE_ABORT, PIPE_VERSION];
    o.extend_from_slice(&session_id.to_be_bytes());
    o.push(reason);
    o
}
pub fn unpack_abort(b: &[u8]) -> Option<(u16, u8)> {
    if b.len() < 5 || b[0] != PIPE_ABORT || b[1] != PIPE_VERSION {
        return None;
    }
    Some((rd16(b, 2), b[4]))
}

// ── data-plane chunk header ─────────────────────────────────────────────────
pub fn pack_chunk(session_id: u16, chunk_seq: u32, payload: &[u8]) -> Vec<u8> {
    let mut o = Vec::with_capacity(CHUNK_HDR_LEN + payload.len());
    o.extend_from_slice(&session_id.to_be_bytes());
    o.extend_from_slice(&chunk_seq.to_be_bytes());
    o.extend_from_slice(payload);
    o
}
/// Returns (session_id, chunk_seq, payload).
pub fn unpack_chunk(b: &[u8]) -> Option<(u16, u32, Vec<u8>)> {
    if b.len() < CHUNK_HDR_LEN {
        return None;
    }
    Some((rd16(b, 0), rd32(b, 2), b[CHUNK_HDR_LEN..].to_vec()))
}

/// Number of chunks a `total_len`-byte object splits into.
pub fn num_chunks(total_len: u32, chunk_size: u16) -> u32 {
    if chunk_size == 0 {
        return 0;
    }
    (total_len + chunk_size as u32 - 1) / chunk_size as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrips() {
        assert_eq!(unpack_open(&pack_open(7, 100000, 1400, 0xDEADBEEF)), Some((7, 100000, 1400, 0xDEADBEEF)));
        assert_eq!(unpack_credit(&pack_credit(7, 32)), Some((7, 32)));
        assert_eq!(unpack_done(&pack_done(7)), Some(7));
        assert_eq!(unpack_abort(&pack_abort(7, ABORT_CHECKSUM)), Some((7, ABORT_CHECKSUM)));
        assert_eq!(unpack_chunk(&pack_chunk(7, 42, b"hello")), Some((7, 42, b"hello".to_vec())));
        assert_eq!(fnv1a32(b""), 0x811C9DC5);
        assert_eq!(fnv1a32(b"hello"), 0x4F9F2CAB);
        assert_eq!(num_chunks(100000, 1400), 72);
    }
}
