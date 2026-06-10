// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//! Pure timeline math for sample-accurate multitrack recording — no audio/IO deps,
//! fully unit-tested. Turns a jittered/reordered/lossy stream of 20 ms DCF-Audio
//! packets (11-bit rolling `packet_id`) into exact sample positions on a shared
//! session timeline, so every per-source track lines up.
//!
//! Model: within a source, spacing is sample-exact (`packet_id` delta × 960). Across
//! sources, each track is anchored by its first packet's receive time minus the
//! one-way network delay, then padded with leading silence to the common session `t0`.

pub const BLOCK_SAMPLES: u64 = 960; // 20 ms @ 48 kHz
pub const BLOCK_US: i64 = 20_000;
pub const PID_MOD: u64 = 2048; // 11-bit packet_id space

/// Unwrap an 11-bit rolling `packet_id` to a monotonic absolute index relative to a
/// previous absolute value (forward progress dominates; small backward = reorder).
pub fn unwrap_pid(prev_abs: u64, raw: u16) -> u64 {
    let prev_lo = prev_abs % PID_MOD;
    let raw = raw as u64 % PID_MOD;
    let fwd = (raw + PID_MOD - prev_lo) % PID_MOD; // 0..2047 treating as forward
    if fwd <= PID_MOD / 2 {
        prev_abs + fwd
    } else {
        prev_abs.saturating_sub(PID_MOD - fwd) // reordered/duplicate -> step back
    }
}

/// What the recorder must do for an arriving packet.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Placement {
    /// Write `silence_before` synthetic 20 ms blocks, then this packet (1 block).
    Write { sample_pos: u64, silence_before: u64 },
    /// Reordered/duplicate/too-late packet that lands before the cursor — drop it.
    Drop,
}

/// Per-source position tracker on the session timeline (all times µs on the local clock).
#[derive(Debug, Clone)]
pub struct TrackTimeline {
    t0_us: i64,
    owd_us: i64,
    first_abs: Option<u64>,
    last_abs: u64,
    anchor_sample: u64, // sample index of this source's first packet on the session timeline
    cursor: u64,        // samples already emitted (real + silence) for this track
}

impl TrackTimeline {
    /// `t0_us` = session start; `owd_us` = estimated one-way delay (≈ rtt/2) for this source.
    pub fn new(t0_us: i64, owd_us: i64) -> Self {
        Self { t0_us, owd_us, first_abs: None, last_abs: 0, anchor_sample: 0, cursor: 0 }
    }

    /// Total samples emitted so far (track length); used to pad the tail on stop.
    pub fn cursor(&self) -> u64 {
        self.cursor
    }

    /// Place a packet that arrived at `recv_us` with rolling id `raw_pid`.
    pub fn place(&mut self, raw_pid: u16, recv_us: i64) -> Placement {
        let abs = match self.first_abs {
            None => {
                // Anchor: where this source's first block sits on the session timeline.
                let start_us = (recv_us - self.owd_us - self.t0_us).max(0);
                let blocks = (start_us as f64 / BLOCK_US as f64).round() as u64;
                self.anchor_sample = blocks * BLOCK_SAMPLES;
                let a = raw_pid as u64 % PID_MOD;
                self.first_abs = Some(a);
                self.last_abs = a;
                a
            }
            Some(_) => {
                let abs = unwrap_pid(self.last_abs, raw_pid);
                self.last_abs = abs.max(self.last_abs);
                abs
            }
        };

        let first = self.first_abs.unwrap();
        let sample_pos = self.anchor_sample + abs.saturating_sub(first) * BLOCK_SAMPLES;
        if sample_pos < self.cursor {
            return Placement::Drop; // reordered/duplicate/too-late — already past here
        }
        let silence_before = (sample_pos - self.cursor) / BLOCK_SAMPLES;
        self.cursor = sample_pos + BLOCK_SAMPLES;
        Placement::Write { sample_pos, silence_before }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unwrap_handles_rollover() {
        assert_eq!(unwrap_pid(2047, 0), 2048); // wraps forward
        assert_eq!(unwrap_pid(2046, 2047), 2047);
        assert_eq!(unwrap_pid(4096, 1), 4097); // continues past multiple wraps
        // reorder: a slightly-earlier id steps back, not forward a near-full lap
        assert_eq!(unwrap_pid(10, 8), 8);
    }

    #[test]
    fn first_packet_anchors_with_leading_silence() {
        // session t0=0, owd=0; first packet at 60 ms -> 3 blocks of leading silence.
        let mut t = TrackTimeline::new(0, 0);
        match t.place(0, 60_000) {
            Placement::Write { sample_pos, silence_before } => {
                assert_eq!(sample_pos, 3 * BLOCK_SAMPLES);
                assert_eq!(silence_before, 3);
            }
            p => panic!("{:?}", p),
        }
    }

    #[test]
    fn contiguous_stream_has_no_gaps() {
        let mut t = TrackTimeline::new(0, 0);
        let _ = t.place(0, 0);
        for pid in 1..50u16 {
            match t.place(pid, pid as i64 * BLOCK_US) {
                Placement::Write { silence_before, .. } => assert_eq!(silence_before, 0),
                p => panic!("pid {pid}: {:?}", p),
            }
        }
        assert_eq!(t.cursor(), 50 * BLOCK_SAMPLES);
    }

    #[test]
    fn lost_packet_becomes_one_silence_block() {
        let mut t = TrackTimeline::new(0, 0);
        let _ = t.place(0, 0);
        let _ = t.place(1, BLOCK_US);
        // pid 2 lost; pid 3 arrives -> exactly one silence block before it.
        match t.place(3, 3 * BLOCK_US) {
            Placement::Write { silence_before, sample_pos } => {
                assert_eq!(silence_before, 1);
                assert_eq!(sample_pos, 3 * BLOCK_SAMPLES);
            }
            p => panic!("{:?}", p),
        }
    }

    #[test]
    fn reordered_late_packet_is_dropped() {
        let mut t = TrackTimeline::new(0, 0);
        let _ = t.place(0, 0);
        let _ = t.place(1, BLOCK_US);
        let _ = t.place(2, 2 * BLOCK_US);
        // pid 1 arrives again late -> already written, drop.
        assert_eq!(t.place(1, 3 * BLOCK_US), Placement::Drop);
    }

    #[test]
    fn two_sources_align_on_session_timeline() {
        // Source A starts at t0; source B joins 100 ms later. Both anchor to the same
        // timeline, so B carries 5 blocks of leading silence -> aligned.
        let mut a = TrackTimeline::new(0, 0);
        let mut b = TrackTimeline::new(0, 0);
        let _ = a.place(0, 0);
        match b.place(0, 100_000) {
            Placement::Write { sample_pos, silence_before } => {
                assert_eq!(silence_before, 5);
                assert_eq!(sample_pos, 5 * BLOCK_SAMPLES);
            }
            p => panic!("{:?}", p),
        }
    }
}
