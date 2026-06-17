// SPDX-License-Identifier: LGPL-3.0-only
//! game_loopback — a headless 2-peer multiplayer demo over the DCF wire.
//!
//! Node A drives a player along a deterministic circular path. Each tick it sends an
//! unreliable SNAPSHOT (position + velocity) packetized into 17-byte DeModFrame DATA
//! frames, plus an occasional reliable EVENT. Node B reassembles the frames (tolerating
//! loss/reorder), applies latest-wins + **dead-reckoning** to hide lost snapshots, and
//! reports tracking error and event delivery. Everything is on 127.0.0.1 loopback.
//!
//!   cargo run --example game_loopback                      # no loss
//!   cargo run --example game_loopback -- --loss 0.05       # 5% per-frame loss
//!   cargo run --example game_loopback -- --ticks 600 --loss 0.1
//!
//! SNAPSHOTs ride the unreliable path (subject to --loss); EVENTs ride a no-loss path,
//! standing in for the SDK's reliable ARQ (`send_game_dcf(.., reliable=true)`).

use dcf_wire_codec::game::{
    input_pack, packetize, snapshot_pack, GameReassembler, Input, Snapshot, GMSG_EVENT,
    GMSG_SNAPSHOT,
};
use std::collections::BTreeMap;
use std::net::UdpSocket;

struct Args {
    ticks: usize,
    loss: f64,
    seed: u64,
}

fn parse_args() -> Args {
    let mut a = Args { ticks: 300, loss: 0.0, seed: 0xD3 };
    let mut it = std::env::args().skip(1);
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "--ticks" => a.ticks = it.next().and_then(|s| s.parse().ok()).unwrap_or(300),
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

/// True player path: a circle of radius 3 around (5, 3.75), one lap per 120 ticks.
fn true_state(tick: usize) -> (f32, f32, f32, f32) {
    let w = 2.0 * std::f32::consts::PI / 120.0;
    let t = tick as f32;
    let (cx, cy, r) = (5.0f32, 3.75f32, 3.0f32);
    let x = cx + r * (w * t).cos();
    let y = cy + r * (w * t).sin();
    let vx = -r * w * (w * t).sin(); // d/dtick → velocity per tick
    let vy = r * w * (w * t).cos();
    (x, y, vx, vy)
}

fn main() -> std::io::Result<()> {
    let args = parse_args();

    let rx = UdpSocket::bind("127.0.0.1:0")?;
    rx.set_nonblocking(true)?;
    let rx_addr = rx.local_addr()?;
    let tx = UdpSocket::bind("127.0.0.1:0")?;

    let mut rng = Rng(args.seed);
    let mut reasm = GameReassembler::new();
    // One monotonic packet_id counter for ALL messages (the reassembler demuxes by
    // packet_id, so snapshots and events must never share an id — the client's
    // GameSession does the same with a single counter).
    let mut next_pid = 0u16;
    let mut alloc_pid = |c: &mut u16| {
        let p = *c;
        *c = (*c + 1) & 2047;
        p
    };
    let mut snap_pid: Vec<u16> = Vec::with_capacity(args.ticks); // tick -> snapshot's pid
    let mut snapshots: BTreeMap<u16, Snapshot> = BTreeMap::new(); // recovered, by pid
    let mut events_recv = 0usize;
    let (mut frames_sent, mut frames_dropped) = (0usize, 0usize);
    let mut events_sent = 0usize;
    const SRC: u16 = 0x00A1; // player A id
    const CH: u16 = 0xFFFF; // lobby broadcast

    for tick in 0..args.ticks {
        let ts_us = ((tick as u32) * 16_000) & 0xFF_FFFF; // ~62.5 Hz tick

        // ── SNAPSHOT (unreliable, subject to loss) ──
        let pid = alloc_pid(&mut next_pid);
        snap_pid.push(pid);
        let (x, y, vx, vy) = true_state(tick);
        let snap = Snapshot { x, y, vx, vy, ..Default::default() };
        let body = snapshot_pack(&snap);
        let frames = packetize(GMSG_SNAPSHOT, &body, pid, ts_us, SRC, CH, 0).unwrap();
        for f in &frames {
            if rng.next_f64() < args.loss {
                frames_dropped += 1;
                continue;
            }
            tx.send_to(f, rx_addr)?;
            frames_sent += 1;
        }

        // ── EVENT every 50 ticks (reliable path: no loss injected) ──
        if tick % 50 == 0 {
            let epid = alloc_pid(&mut next_pid);
            let text = format!("score:{}", tick / 50);
            let ev = packetize(GMSG_EVENT, text.as_bytes(), epid, ts_us, SRC, CH, 1).unwrap();
            for f in &ev {
                tx.send_to(f, rx_addr)?;
            }
            events_sent += 1;
        }

        // ── drain B: reassemble whatever arrived ──
        let mut buf = [0u8; 17];
        while let Ok(17) = rx.recv(&mut buf) {
            if let Some(pkt) = reasm.push(&buf) {
                match pkt.msg_type_id {
                    GMSG_SNAPSHOT => {
                        let b: [u8; 14] = pkt.payload[..].try_into().unwrap();
                        snapshots.insert(pkt.packet_id, dcf_wire_codec::game::snapshot_unpack(&b));
                    }
                    GMSG_EVENT => events_recv += 1,
                    _ => {}
                }
            }
        }
    }

    // ── Playout: reconstruct the path with vs. without dead-reckoning ──
    const DR_CAP: f32 = 4.0; // stop extrapolating after ~4 ticks (~64 ms), like real clients
    let mut err_dr = 0.0f64; // dead-reckoned (extrapolate last pos + vel, capped)
    let mut err_hold = 0.0f64; // naive hold-last-position
    let mut last: Option<(usize, Snapshot)> = None; // (tick of last known, state)
    let mut recovered = 0usize;
    for tick in 0..args.ticks {
        let (tx_true, ty_true, _, _) = true_state(tick);
        if let Some(s) = snapshots.get(&snap_pid[tick]) {
            recovered += 1;
            last = Some((tick, *s));
            // exact when present → no error contribution
            continue;
        }
        if let Some((ltick, s)) = last {
            let dt = (tick - ltick) as f32;
            let dt_c = dt.min(DR_CAP);
            let (px, py) = (s.x + s.vx * dt_c, s.y + s.vy * dt_c); // dead reckon (capped)
            err_dr += (((px - tx_true).powi(2) + (py - ty_true).powi(2)) as f64).sqrt();
            err_hold += (((s.x - tx_true).powi(2) + (s.y - ty_true).powi(2)) as f64).sqrt();
        }
    }
    let lost = reasm.finalize();
    let missed = args.ticks - recovered;
    let denom = missed.max(1) as f64;

    // ── A demo of the certified INPUT body too (server-authoritative variant) ──
    let inp = Input { tick: args.ticks as u32, buttons: 0b1010 };
    let _ = input_pack(&inp);

    println!("DCF game_loopback  ticks={}  per-frame loss={:.0}%", args.ticks, args.loss * 100.0);
    println!(
        "  snapshots : {} recovered, {} missed (dead-reckoned), {} frames sent, {} dropped, {} never completed",
        recovered, missed, frames_sent, frames_dropped, lost.len()
    );
    println!("  events    : {}/{} reliable EVENTs delivered", events_recv, events_sent);
    println!(
        "  tracking  : mean error on missed ticks  dead-reckon={:.3} m   hold-last={:.3} m",
        err_dr / denom,
        err_hold / denom
    );
    if events_recv == events_sent {
        println!("  OK: every reliable EVENT arrived; dead-reckoning hid the lost SNAPSHOTs.");
    }
    Ok(())
}
