// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//! Multitrack recorder: each source's raw Opus frames are written verbatim into a
//! per-source Ogg-Opus file, placed on a sample-accurate session timeline (see
//! `sync.rs`) with gaps/leading-offset filled by a synthetic 20 ms Opus silence
//! frame so every track is continuous and starts at the same t0. On stop, host
//! `ffmpeg` produces a bit-exact multitrack master (`-c:a copy`) and a mixdown
//! (`amix`). No re-encode of real audio.
#![cfg(feature = "audio")]

use crate::sync::{Placement, TrackTimeline, BLOCK_SAMPLES};
use dcf_rust_sdk::audio;
use ogg::writing::{PacketWriteEndInfo, PacketWriter};
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::BufWriter;
use std::path::PathBuf;
use std::process::Command;

const SAMPLE_RATE: u32 = 48000;
const PRESKIP: u16 = 3840; // libopus 48 kHz lookahead; identical on every track → relative sync preserved

/// Build the OpusHead identification header (RFC 7845 §5.1), mono 48 kHz.
fn opus_head() -> Vec<u8> {
    let mut h = Vec::with_capacity(19);
    h.extend_from_slice(b"OpusHead");
    h.push(1); // version
    h.push(1); // channel count (mono)
    h.extend_from_slice(&PRESKIP.to_le_bytes());
    h.extend_from_slice(&SAMPLE_RATE.to_le_bytes()); // original input rate
    h.extend_from_slice(&0u16.to_le_bytes()); // output gain
    h.push(0); // channel mapping family 0
    h
}

fn opus_tags() -> Vec<u8> {
    let vendor = b"HydraMesh";
    let mut t = Vec::new();
    t.extend_from_slice(b"OpusTags");
    t.extend_from_slice(&(vendor.len() as u32).to_le_bytes());
    t.extend_from_slice(vendor);
    t.extend_from_slice(&0u32.to_le_bytes()); // user comment count
    t
}

/// One synthetic 20 ms mono Opus silence frame (encoded once, reused for gaps).
fn silence_frame() -> Vec<u8> {
    if let Some(mut c) = audio::codec_for(audio::CODEC_OPUS) {
        let pcm = vec![0.0f32; BLOCK_SAMPLES as usize];
        let f = c.encode(&pcm);
        if !f.is_empty() {
            return f;
        }
    }
    // Fallback: a minimal Opus "silence" TOC frame (config 0, mono, 1 frame).
    vec![0xF8, 0xFF, 0xFE]
}

struct Track {
    path: PathBuf,
    writer: PacketWriter<'static, BufWriter<File>>,
    serial: u32,
    timeline: TrackTimeline,
    granule: u64, // cumulative 48 kHz samples (Ogg granulepos, incl. preskip)
    samples: u64, // audio samples written (excl. preskip) — track length
}

impl Track {
    fn new(path: PathBuf, serial: u32, t0_us: i64, owd_us: i64) -> std::io::Result<Self> {
        let w = BufWriter::new(File::create(&path)?);
        let mut writer = PacketWriter::new(w);
        writer.write_packet(opus_head(), serial, PacketWriteEndInfo::EndPage, 0)?;
        writer.write_packet(opus_tags(), serial, PacketWriteEndInfo::EndPage, 0)?;
        Ok(Self { path, writer, serial, timeline: TrackTimeline::new(t0_us, owd_us), granule: PRESKIP as u64, samples: 0 })
    }

    fn write_frame(&mut self, data: Vec<u8>, end: PacketWriteEndInfo) -> std::io::Result<()> {
        self.granule += BLOCK_SAMPLES;
        self.samples += BLOCK_SAMPLES;
        self.writer.write_packet(data, self.serial, end, self.granule)
    }
}

/// Active recording session.
pub struct Recorder {
    dir: PathBuf,
    t0_us: i64,
    serial_next: u32,
    silence: Vec<u8>,
    tracks: HashMap<String, Track>, // key: "self" or "src-<id>"
}

pub struct RecordingResult {
    pub dir: String,
    pub master: Option<String>,
    pub mixdown: Option<String>,
    pub tracks: Vec<String>,
}

impl Recorder {
    pub fn start(dir: PathBuf, t0_us: i64) -> std::io::Result<Self> {
        fs::create_dir_all(&dir)?;
        Ok(Self { dir, t0_us, serial_next: 1, silence: silence_frame(), tracks: HashMap::new() })
    }

    fn track(&mut self, key: &str, owd_us: i64) -> std::io::Result<&mut Track> {
        if !self.tracks.contains_key(key) {
            let serial = self.serial_next;
            self.serial_next += 1;
            let path = self.dir.join(format!("{key}.opus"));
            self.tracks.insert(key.to_string(), Track::new(path, serial, self.t0_us, owd_us)?);
        }
        Ok(self.tracks.get_mut(key).unwrap())
    }

    /// Feed one received/sent Opus packet. `key` = "self" or "src-<id>"; `owd_us` is
    /// this source's one-way delay (0 for self). Gaps/leading-offset are silence-filled.
    pub fn push(&mut self, key: &str, raw_pid: u16, recv_us: i64, opus: Vec<u8>, owd_us: i64) {
        let silence = self.silence.clone();
        if let Ok(t) = self.track(key, owd_us) {
            match t.timeline.place(raw_pid, recv_us) {
                Placement::Drop => {}
                Placement::Write { silence_before, .. } => {
                    for _ in 0..silence_before {
                        let _ = t.write_frame(silence.clone(), PacketWriteEndInfo::NormalPacket);
                    }
                    let _ = t.write_frame(opus, PacketWriteEndInfo::NormalPacket);
                }
            }
        }
    }

    /// Pad all tracks to equal length, close streams, and mux with ffmpeg.
    pub fn finish(mut self) -> RecordingResult {
        let max_samples = self.tracks.values().map(|t| t.samples).max().unwrap_or(0);
        let silence = self.silence.clone();
        let mut track_paths = Vec::new();
        for t in self.tracks.values_mut() {
            while t.samples < max_samples {
                let _ = t.write_frame(silence.clone(), PacketWriteEndInfo::NormalPacket);
            }
            // Always terminate with an EndStream frame so the final Ogg page is
            // flushed + marked EOS (every track gets the same +1 block → equal length).
            let _ = t.write_frame(silence.clone(), PacketWriteEndInfo::EndStream);
            track_paths.push(t.path.to_string_lossy().to_string());
        }
        track_paths.sort();

        let master = self.mux_master(&track_paths);
        let mixdown = self.mux_mixdown(&track_paths);
        RecordingResult {
            dir: self.dir.to_string_lossy().to_string(),
            master,
            mixdown,
            tracks: track_paths,
        }
    }

    fn ffmpeg_ok() -> bool {
        Command::new("ffmpeg").arg("-version").output().map(|o| o.status.success()).unwrap_or(false)
    }

    fn mux_master(&self, tracks: &[String]) -> Option<String> {
        if tracks.is_empty() || !Self::ffmpeg_ok() {
            return None;
        }
        let out = self.dir.join("master.mka");
        let mut cmd = Command::new("ffmpeg");
        cmd.arg("-y");
        for tk in tracks {
            cmd.args(["-i", tk]);
        }
        for i in 0..tracks.len() {
            cmd.args(["-map", &format!("{i}:a")]);
        }
        cmd.args(["-c:a", "copy", out.to_str()?]); // bit-exact multitrack master
        cmd.status().ok().filter(|s| s.success()).map(|_| out.to_string_lossy().to_string())
    }

    fn mux_mixdown(&self, tracks: &[String]) -> Option<String> {
        if tracks.is_empty() || !Self::ffmpeg_ok() {
            return None;
        }
        let out = self.dir.join("mix.flac");
        let mut cmd = Command::new("ffmpeg");
        cmd.arg("-y");
        for tk in tracks {
            cmd.args(["-i", tk]);
        }
        let inputs: String = (0..tracks.len()).map(|i| format!("[{i}:a]")).collect();
        let filter = format!("{inputs}amix=inputs={}:duration=longest:normalize=0[a]", tracks.len());
        cmd.args(["-filter_complex", &filter, "-map", "[a]", "-c:a", "flac", out.to_str()?]);
        cmd.status().ok().filter(|s| s.success()).map(|_| out.to_string_lossy().to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sync::BLOCK_SAMPLES;
    use ogg::reading::PacketReader;
    use std::io::{Seek, SeekFrom};

    #[test]
    fn track_is_gapless_and_roundtrips() {
        // Build a track: pid 0,1, (2 lost), 3 -> expect 4 audio packets (one is silence fill).
        let dir = std::env::temp_dir().join(format!("dcf-rec-test-{}", std::process::id()));
        let _ = fs::create_dir_all(&dir);
        let path = dir.join("t.opus");
        {
            let mut rec = Recorder::start(dir.clone(), 0).unwrap();
            rec.push("self", 0, 0, vec![0xFC, 0x01, 0x02], 0);
            rec.push("self", 1, 20_000, vec![0xFC, 0x03, 0x04], 0);
            rec.push("self", 3, 60_000, vec![0xFC, 0x05, 0x06], 0); // pid 2 lost -> +1 silence
            let res = rec.finish();
            assert_eq!(res.tracks.len(), 1);
        }
        // Read it back: OpusHead, OpusTags, then >=4 audio packets, final granule monotonic.
        let mut f = File::open(dir.join("self.opus")).unwrap();
        let _ = f.seek(SeekFrom::Start(0));
        let mut rd = PacketReader::new(f);
        let p0 = rd.read_packet_expected().unwrap();
        assert!(p0.data.starts_with(b"OpusHead"), "first packet must be OpusHead");
        let p1 = rd.read_packet_expected().unwrap();
        assert!(p1.data.starts_with(b"OpusTags"), "second packet must be OpusTags");
        let mut audio_packets = 0u64;
        let mut last_gp = 0u64;
        while let Ok(Some(p)) = rd.read_packet() {
            audio_packets += 1;
            assert!(p.absgp_page() >= last_gp, "granulepos must be monotonic");
            last_gp = p.absgp_page();
        }
        // pids 0,1,silence(2),3 = 4 audio frames minimum.
        assert!(audio_packets >= 4, "expected >=4 audio frames, got {audio_packets}");
        let _ = std::fs::remove_dir_all(&dir);
        let _ = path;
        let _ = BLOCK_SAMPLES;
    }

    #[test]
    fn multitrack_master_muxes_per_source_streams() {
        // Two sources interleaved -> two .opus tracks -> (if ffmpeg present) a master
        // with two audio streams.
        let dir = std::env::temp_dir().join(format!("dcf-rec-mt-{}", std::process::id()));
        let res = {
            let mut rec = Recorder::start(dir.clone(), 0).unwrap();
            let sf = silence_frame();
            for pid in 0..5u16 {
                rec.push("self", pid, pid as i64 * 20_000, sf.clone(), 0);
                rec.push("src-7", pid, pid as i64 * 20_000, sf.clone(), 0);
            }
            rec.finish()
        };
        assert_eq!(res.tracks.len(), 2, "two per-source tracks");
        if let Some(master) = &res.master {
            let out = Command::new("ffprobe")
                .args(["-v", "error", "-select_streams", "a", "-show_entries", "stream=index", "-of", "csv=p=0", master])
                .output()
                .expect("ffprobe");
            let n = String::from_utf8_lossy(&out.stdout).lines().count();
            assert_eq!(n, 2, "master.mka must carry 2 audio tracks (got {n})");
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
}
