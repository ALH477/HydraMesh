// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//! Real-time audio jam: mic capture → Opus encode → DCF-Audio over the mesh; inbound
//! DCF-Audio → per-source decode → mix → speaker. cpal streams are `!Send`, so they
//! live on a dedicated audio thread; control/data crosses threads via channels.
//!
//! Behind the `audio` feature; `--no-default-features` compiles a stub so the rest of
//! the client (mesh / messaging / wire inspector) works without audio system libs.
//!
//! v1 assumes a 48 kHz mono-capable device (Opus is 48 kHz). Arbitrary-rate device
//! resampling is a documented refinement.

use crate::engine::AudioRx;
use std::sync::atomic::AtomicU16;
use std::sync::mpsc::Sender;
use std::sync::Arc;

#[cfg(feature = "audio")]
use std::path::PathBuf;

/// Handle to a running jam. Stopping it tears down the audio threads.
pub struct Jam {
    #[cfg(feature = "audio")]
    inner: imp::JamInner,
    audio_sink: Sender<AudioRx>,
    #[cfg(feature = "audio")]
    recorder: imp::SharedRecorder,
}

impl Jam {
    /// The sender the [`crate::engine::UiHandler`] uses to deliver received packets.
    pub fn audio_sink(&self) -> Sender<AudioRx> {
        self.audio_sink.clone()
    }

    #[cfg(feature = "audio")]
    pub fn start_recording(&self, dir: PathBuf) -> Result<(), String> {
        imp::start_recording(&self.recorder, dir)
    }

    #[cfg(feature = "audio")]
    pub fn stop_recording(&self) -> Option<crate::recorder::RecordingResult> {
        imp::stop_recording(&self.recorder)
    }

    #[cfg(feature = "audio")]
    pub fn is_recording(&self) -> bool {
        self.recorder.lock().is_some()
    }
}

#[cfg(feature = "audio")]
pub fn start(node: Arc<dcf_rust_sdk::DcfNode>, channel: Arc<AtomicU16>, codec_id: u8) -> Result<Jam, String> {
    let (inner, audio_sink, recorder) = imp::JamInner::start(node, channel, codec_id)?;
    Ok(Jam { inner, audio_sink, recorder })
}

#[cfg(feature = "audio")]
pub fn stop(jam: Jam) {
    jam.inner.stop();
}

#[cfg(not(feature = "audio"))]
pub fn start(_node: Arc<dcf_rust_sdk::DcfNode>, _channel: Arc<AtomicU16>, _codec_id: u8) -> Result<Jam, String> {
    Err("this build was compiled without the `audio` feature".into())
}

#[cfg(not(feature = "audio"))]
pub fn stop(_jam: Jam) {}

// ── real implementation ─────────────────────────────────────────────────────
#[cfg(feature = "audio")]
mod imp {
    use super::*;
    use crate::recorder::{Recorder, RecordingResult};
    use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
    use dcf_rust_sdk::{audio, DcfNode};
    use parking_lot::Mutex;
    use std::collections::{HashMap, VecDeque};
    use std::sync::atomic::Ordering;
    use std::sync::atomic::{AtomicBool, AtomicU16};
    use std::sync::mpsc::{Receiver, Sender};
    use std::thread::{self, JoinHandle};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    const BLOCK: usize = 960; // 20 ms @ 48 kHz, mono

    type Ring = Arc<Mutex<VecDeque<f32>>>;
    pub type SharedRecorder = Arc<Mutex<Option<Recorder>>>;

    fn now_us() -> i64 {
        SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_micros() as i64).unwrap_or(0)
    }

    pub struct JamInner {
        running: Arc<AtomicBool>,
        threads: Vec<JoinHandle<()>>,
    }

    pub fn start_recording(rec: &SharedRecorder, dir: PathBuf) -> Result<(), String> {
        let r = Recorder::start(dir, now_us()).map_err(|e| e.to_string())?;
        *rec.lock() = Some(r);
        Ok(())
    }

    pub fn stop_recording(rec: &SharedRecorder) -> Option<RecordingResult> {
        rec.lock().take().map(|r| r.finish())
    }

    impl JamInner {
        pub fn start(
            node: Arc<DcfNode>,
            channel: Arc<AtomicU16>,
            codec_id: u8,
        ) -> Result<(JamInner, Sender<AudioRx>, SharedRecorder), String> {
            let running = Arc::new(AtomicBool::new(true));
            let capture: Ring = Arc::new(Mutex::new(VecDeque::new()));
            let playback: Ring = Arc::new(Mutex::new(VecDeque::new()));
            let recorder: SharedRecorder = Arc::new(Mutex::new(None));
            let (sink_tx, sink_rx) = std::sync::mpsc::channel::<AudioRx>();

            // cpal streams (the !Send part) on a dedicated thread.
            let t_io = {
                let (run, cap, pb) = (running.clone(), capture.clone(), playback.clone());
                thread::Builder::new()
                    .name("dcf-audio-io".into())
                    .spawn(move || io_thread(run, cap, pb))
                    .map_err(|e| e.to_string())?
            };

            // Capture → encode → send (+ record self track).
            let t_tx = {
                let (run, cap, ch, rec, nd) =
                    (running.clone(), capture.clone(), channel.clone(), recorder.clone(), node.clone());
                thread::spawn(move || {
                    let mut codec = match audio::codec_for(codec_id) {
                        Some(c) => c,
                        None => return,
                    };
                    let block = codec.profile().block_samples as usize;
                    let mut pid: u16 = 0;
                    while run.load(Ordering::Relaxed) {
                        if let Some(pcm) = pull_block(&cap, block) {
                            let enc = codec.encode(&pcm);
                            if !enc.is_empty() {
                                let ch_now = ch.load(Ordering::Relaxed);
                                let _ = nd.send_audio_dcf(codec_id, &enc, pid, pid as u32 * 20_000, ch_now);
                                if let Some(r) = rec.lock().as_mut() {
                                    r.push("self", pid, now_us(), enc, 0);
                                }
                                pid = pid.wrapping_add(1) & 0x07FF;
                            }
                        } else {
                            thread::sleep(Duration::from_millis(2));
                        }
                    }
                })
            };

            // Receive → per-source decode → mix to playback (+ record peer tracks).
            let t_rx = {
                let (run, pb, rec) = (running.clone(), playback.clone(), recorder.clone());
                thread::spawn(move || decode_thread(run, sink_rx, pb, rec))
            };

            Ok((JamInner { running, threads: vec![t_io, t_tx, t_rx] }, sink_tx, recorder))
        }

        pub fn stop(self) {
            self.running.store(false, Ordering::Relaxed);
            for t in self.threads {
                let _ = t.join();
            }
        }
    }

    fn pull_block(ring: &Ring, n: usize) -> Option<Vec<f32>> {
        let mut q = ring.lock();
        if q.len() < n {
            return None;
        }
        Some(q.drain(..n).collect())
    }

    /// Mix a decoded block into the tail of the playback ring (concurrent sources sum).
    fn mix_into(pb: &Ring, block: &[f32]) {
        let mut q = pb.lock();
        let have_tail = q.len().min(block.len());
        let start = q.len() - have_tail;
        for (i, s) in block.iter().enumerate() {
            if i < have_tail {
                let idx = start + i;
                q[idx] = (q[idx] + s).clamp(-1.0, 1.0);
            } else {
                q.push_back(*s);
            }
        }
        while q.len() > BLOCK * 12 {
            q.pop_front();
        }
    }

    fn decode_thread(run: Arc<AtomicBool>, rx: Receiver<AudioRx>, pb: Ring, rec: SharedRecorder) {
        let mut decoders: HashMap<u16, Box<dyn audio::AudioCodec>> = HashMap::new();
        while run.load(Ordering::Relaxed) {
            match rx.recv_timeout(Duration::from_millis(100)) {
                Ok(m) => {
                    if let Some(r) = rec.lock().as_mut() {
                        r.push(&format!("src-{}", m.src_id), m.packet_id, m.recv_us, m.opus.clone(), 0);
                    }
                    let dec = decoders
                        .entry(m.src_id)
                        .or_insert_with(|| audio::codec_for(m.codec_id).unwrap_or_else(|| audio::codec_for(audio::CODEC_PCM_DIAG).unwrap()));
                    let pcm = dec.decode(&m.opus);
                    mix_into(&pb, &pcm);
                }
                Err(_) => {}
            }
        }
    }

    fn io_thread(run: Arc<AtomicBool>, capture: Ring, playback: Ring) {
        let host = cpal::default_host();
        let in_stream = host.default_input_device().and_then(|dev| {
            let cfg = dev.default_input_config().ok()?;
            let chans = cfg.channels() as usize;
            let cap = capture.clone();
            dev.build_input_stream(
                &cfg.config(),
                move |data: &[f32], _: &cpal::InputCallbackInfo| {
                    let mut q = cap.lock();
                    // downmix interleaved channels to mono
                    for frame in data.chunks(chans.max(1)) {
                        let m = frame.iter().sum::<f32>() / chans.max(1) as f32;
                        q.push_back(m);
                    }
                    while q.len() > BLOCK * 8 {
                        q.pop_front();
                    }
                },
                |e| log::error!("input stream error: {e}"),
                None,
            )
            .ok()
        });
        let out_stream = host.default_output_device().and_then(|dev| {
            let cfg = dev.default_output_config().ok()?;
            let chans = cfg.channels() as usize;
            let pb = playback.clone();
            dev.build_output_stream(
                &cfg.config(),
                move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                    let mut q = pb.lock();
                    for frame in data.chunks_mut(chans.max(1)) {
                        let s = q.pop_front().unwrap_or(0.0);
                        for o in frame.iter_mut() {
                            *o = s; // mono → all output channels
                        }
                    }
                },
                |e| log::error!("output stream error: {e}"),
                None,
            )
            .ok()
        });
        if let Some(s) = &in_stream {
            let _ = s.play();
        }
        if let Some(s) = &out_stream {
            let _ = s.play();
        }
        while run.load(Ordering::Relaxed) {
            thread::sleep(Duration::from_millis(50));
        }
    }
}
