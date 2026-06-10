//! jam_loopback — a headless 2-peer collaborative-audio jam over the DCF wire.
//!
//! Node A captures audio (here, a deterministic tone), runs it through the selected
//! codec, packetizes each 20 ms block into 17-byte DeModFrame CTRL frames, and sends
//! them over UDP. Node B reassembles the frames (tolerating loss/reorder), runs a
//! tiny ordered jitter buffer with packet-loss concealment, decodes, and reports
//! end-to-end latency, packet-loss %, and SNR. Everything is on 127.0.0.1 loopback.
//!
//!   cargo run --example jam_loopback -- --codec pcm                 # default, no deps
//!   cargo run --example jam_loopback -- --codec pcm --loss 0.05     # 5% frame loss
//!   cargo run --features opus --example jam_loopback -- --codec opus
//!   cargo run --features pm   --example jam_loopback -- --codec pm
//!
//! `--codec opus|pm` require the matching cargo feature (and libopus / the Faust synth).

use dcf_wire_codec::audio::{
    codec_for, packetize, pm_pack, AudioReassembler, PmParams, CODEC_FAUST_PM, CODEC_OPUS,
    CODEC_PCM_DIAG,
};
use std::collections::BTreeMap;
use std::net::UdpSocket;
use std::time::Instant;

struct Args {
    codec: u8,
    blocks: usize,
    loss: f64,
    seed: u64,
}

fn parse_args() -> Args {
    let mut a = Args { codec: CODEC_PCM_DIAG, blocks: 100, loss: 0.0, seed: 0xD3 };
    let mut it = std::env::args().skip(1);
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "--codec" => {
                a.codec = match it.next().as_deref() {
                    Some("opus") => CODEC_OPUS,
                    Some("pm") => CODEC_FAUST_PM,
                    _ => CODEC_PCM_DIAG,
                }
            }
            "--blocks" => a.blocks = it.next().and_then(|s| s.parse().ok()).unwrap_or(100),
            "--loss" => a.loss = it.next().and_then(|s| s.parse().ok()).unwrap_or(0.0),
            "--seed" => a.seed = it.next().and_then(|s| s.parse().ok()).unwrap_or(0xD3),
            _ => {}
        }
    }
    a
}

/// Tiny deterministic xorshift PRNG (so loss injection is reproducible).
struct Rng(u64);
impl Rng {
    fn next_f64(&mut self) -> f64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        (self.0 >> 11) as f64 / (1u64 << 53) as f64
    }
}

fn main() -> std::io::Result<()> {
    let args = parse_args();
    let codec_name = match args.codec {
        CODEC_OPUS => "opus",
        CODEC_FAUST_PM => "pm",
        _ => "pcm",
    };

    let mut enc = match codec_for(args.codec) {
        Some(c) => c,
        None => {
            eprintln!(
                "codec '{}' not built — rebuild with `--features {}`",
                codec_name, codec_name
            );
            std::process::exit(2);
        }
    };
    let mut dec = codec_for(args.codec).unwrap();
    let prof = enc.profile();
    let block = prof.block_samples as usize;
    let block_ms = 1000.0 * block as f64 / prof.sample_rate as f64;

    // Node B (receiver) and Node A (sender) over loopback.
    let rx = UdpSocket::bind("127.0.0.1:0")?;
    rx.set_nonblocking(true)?;
    let rx_addr = rx.local_addr()?;
    let tx = UdpSocket::bind("127.0.0.1:0")?;

    // Source "capture": a 440 Hz tone at the codec's sample rate.
    let tone = |i: usize| {
        (i as f64 * 440.0 * 2.0 * std::f64::consts::PI / prof.sample_rate as f64).sin() as f32
    };

    let mut rng = Rng(args.seed);
    let mut reasm = AudioReassembler::new();
    let mut received: BTreeMap<u16, Vec<u8>> = BTreeMap::new();
    let mut sent_pcm: Vec<Vec<f32>> = Vec::with_capacity(args.blocks);
    let (mut frames_sent, mut frames_dropped) = (0usize, 0usize);

    let t0 = Instant::now();
    for blk in 0..args.blocks {
        let packet_id = (blk as u16) & 2047;
        let ts_us = ((blk as u32) * (block_ms as u32) * 1000) & 0xFF_FFFF;

        // capture one block
        let pcm: Vec<f32> = (0..block).map(|s| tone(blk * block + s)).collect();
        sent_pcm.push(pcm.clone());

        // encode (PM: tiny host analysis -> params; others: codec.encode)
        let payload = if args.codec == CODEC_FAUST_PM {
            let rms = (pcm.iter().map(|x| x * x).sum::<f32>() / block as f32).sqrt();
            let p = PmParams {
                f0: 5353, // ~440 Hz under the cents mapping
                amp: (rms * 255.0).clamp(0.0, 255.0) as u8,
                mod_index: 64,
                mod_ratio: 16,
                bright: 40,
                env: 255,
                flags: 0,
            };
            pm_pack(&p).to_vec()
        } else {
            enc.encode(&pcm)
        };

        // packetize -> send each 17-byte CTRL frame (with optional loss)
        let frames =
            packetize(args.codec, &payload, packet_id, ts_us, 1, 2, 0).expect("payload within 124 B");
        for f in &frames {
            if rng.next_f64() < args.loss {
                frames_dropped += 1;
                continue;
            }
            tx.send_to(f, rx_addr)?;
            frames_sent += 1;
        }

        // drain whatever has arrived at B and reassemble
        let mut buf = [0u8; 17];
        while let Ok(17) = rx.recv(&mut buf) {
            if let Some(pkt) = reasm.push(&buf) {
                received.insert(pkt.packet_id, pkt.payload);
            }
        }
    }
    let lost = reasm.finalize();
    let wall = t0.elapsed();

    // ── Playout: ordered jitter buffer + PLC, decode each packet ──
    let mut out_pcm: Vec<Vec<f32>> = Vec::with_capacity(args.blocks);
    let mut concealed = 0usize;
    for blk in 0..args.blocks {
        let pid = (blk as u16) & 2047;
        match received.get(&pid) {
            Some(bytes) => out_pcm.push(dec.decode(bytes)),
            None => {
                concealed += 1;
                out_pcm.push(dec.plc()); // packet-loss concealment
            }
        }
    }

    // ── Metrics ──
    let recovered = received.len();
    let total = args.blocks;
    println!(
        "DCF jam_loopback  codec={}  {} Hz  {}-sample blocks ({:.1} ms)",
        codec_name, prof.sample_rate, block, block_ms
    );
    println!(
        "  packets   : {} sent, {} recovered, {} concealed (PLC), {} never completed",
        total,
        recovered,
        concealed,
        lost.len()
    );
    println!(
        "  frames    : {} sent, {} dropped ({:.1}% loss)",
        frames_sent,
        frames_dropped,
        100.0 * frames_dropped as f64 / (frames_sent + frames_dropped).max(1) as f64
    );
    println!(
        "  latency   : {:.1} ms/block, jitter target ~{:.0} ms; processed {} blocks in {:.1} ms wall",
        block_ms,
        2.0 * block_ms,
        total,
        wall.as_secs_f64() * 1000.0
    );

    if args.codec == CODEC_FAUST_PM {
        // PM is parametric synthesis (not waveform repro): report output energy.
        let (mut s, mut n) = (0.0f64, 0usize);
        for b in &out_pcm {
            for &x in b {
                s += (x * x) as f64;
                n += 1;
            }
        }
        let rms = (s / n.max(1) as f64).sqrt();
        println!("  synthesis : PM resynth output RMS = {:.4} (parametric; SNR N/A)", rms);
    } else {
        // SNR between captured and played-out PCM.
        let (mut sig, mut err) = (0.0f64, 0.0f64);
        for blk in 0..total {
            for (i, &x) in sent_pcm[blk].iter().enumerate() {
                let y = out_pcm[blk].get(i).copied().unwrap_or(0.0);
                sig += (x * x) as f64;
                err += ((x - y) * (x - y)) as f64;
            }
        }
        let snr = if err > 0.0 { 10.0 * (sig / err).log10() } else { f64::INFINITY };
        println!("  fidelity  : SNR = {:.1} dB (captured vs played-out, incl. PLC gaps)", snr);
    }

    let ok = recovered + concealed >= total && frames_sent > 0;
    println!("{}", if ok { "JAM OK" } else { "JAM INCOMPLETE" });
    Ok(())
}
