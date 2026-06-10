// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//! Frequency rendezvous: the handshakeless protocol has no connection setup, so peers
//! pre-agree on a CHANNEL (the DeModFrame `dst` field). Tune a numeric channel, or
//! derive one from a shared passphrase via the certified CRC-16 — identical to the Lua
//! framework's `channel_from_passphrase` and cross-checked against `wirelab_core.crc16`.

use dcf_wire_codec::crc16_ccitt;

/// Broadcast channel — every tuned peer hears it.
pub const BROADCAST: u16 = 0xFFFF;

/// Map a shared passphrase to a u16 rendezvous channel (same word → same channel).
pub fn channel_from_passphrase(s: &str) -> u16 {
    crc16_ccitt(s.as_bytes())
}

/// A node tuned to `my_channel` accepts a frame addressed to its channel or broadcast.
pub fn accepts(frame_dst: u16, my_channel: u16) -> bool {
    frame_dst == my_channel || frame_dst == BROADCAST
}

/// Extract the `dst` (channel) from a 17-byte DeModFrame without a full decode.
pub fn frame_channel(frame: &[u8]) -> Option<u16> {
    if frame.len() != 17 {
        return None;
    }
    Some(((frame[6] as u16) << 8) | frame[7] as u16)
}

/// Extract the `src_id` (sender) from a 17-byte DeModFrame — the per-source key.
pub fn frame_src(frame: &[u8]) -> Option<u16> {
    if frame.len() != 17 {
        return None;
    }
    Some(((frame[4] as u16) << 8) | frame[5] as u16)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn passphrase_is_certified_crc() {
        // The CRC anchor pins the channel hash to the certified wire CRC.
        assert_eq!(channel_from_passphrase("123456789"), 0x29B1);
    }
    #[test]
    fn accepts_match_and_broadcast() {
        assert!(accepts(0x1234, 0x1234));
        assert!(accepts(BROADCAST, 0x1234));
        assert!(!accepts(0x1234, 0x5678));
    }
}
