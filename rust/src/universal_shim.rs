use dcf_rust_sdk::{Position, ProtoMessage, msg_type};
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::{SystemTime, UNIX_EPOCH}; // FIX: Added missing time imports
use tokio::net::UdpSocket;

#[derive(Clone, Debug)]
pub struct KinematicState {
    pub timestamp: u64,
    pub pos: Position,
    pub velocity: (f32, f32, f32),
}

pub struct AdaptiveBuffer {
    pub history: VecDeque<KinematicState>,
}

impl AdaptiveBuffer {
    pub fn new() -> Self {
        Self {
            history: VecDeque::with_capacity(20),
        }
    }

    pub fn push(&mut self, new_pos: Position, ts: u64) {
        let mut velocity = (0.0, 0.0, 0.0);
        
        if let Some(last) = self.history.back() {
            let dt = (ts - last.timestamp) as f32 / 1_000_000.0;
            if dt > 0.0 {
                velocity = (
                    (new_pos.x - last.pos.x) / dt,
                    (new_pos.y - last.pos.y) / dt,
                    (new_pos.z - last.pos.z) / dt,
                );
            }
        }

        let state = KinematicState { timestamp: ts, pos: new_pos, velocity };
        let idx = self.history.partition_point(|s| s.timestamp < ts);
        self.history.insert(idx, state);

        if self.history.len() > 10 { self.history.pop_front(); }
    }

    pub fn get_state_at(&self, target_time: u64) -> Option<Position> {
        if self.history.is_empty() { return None; }
        let newest = self.history.back().unwrap();
        
        if target_time > newest.timestamp {
            let dt = (target_time - newest.timestamp) as f32 / 1_000_000.0;
            let cap = dt.min(0.050); 
            return Some(Position {
                x: newest.pos.x + (newest.velocity.0 * cap),
                y: newest.pos.y + (newest.velocity.1 * cap),
                z: newest.pos.z + (newest.velocity.2 * cap),
            });
        }

        for i in 0..self.history.len() - 1 {
            let left = &self.history[i];
            let right = &self.history[i+1];
            if left.timestamp <= target_time && right.timestamp >= target_time {
                let total_dt = (right.timestamp - left.timestamp) as f32;
                if total_dt <= 0.0 { continue; }
                let factor = (target_time - left.timestamp) as f32 / total_dt;
                return Some(Position {
                    x: left.pos.x + (right.pos.x - left.pos.x) * factor,
                    y: left.pos.y + (right.pos.y - left.pos.y) * factor,
                    z: left.pos.z + (right.pos.z - left.pos.z) * factor,
                });
            }
        }
        Some(newest.pos.clone())
    }
}

pub async fn run_playback_loop(
    buffer: Arc<Mutex<AdaptiveBuffer>>, 
    node: Arc<dcf_rust_sdk::DcfNode>,
    target_addr: String
) {
    let socket = UdpSocket::bind("0.0.0.0:0").await.unwrap();
    let mut interval = tokio::time::interval(std::time::Duration::from_millis(8));
    let seq_counter = Arc::new(AtomicU32::new(0));

    loop {
        interval.tick().await;
        let stats = node.get_network_stats();
        let jitter_us = (stats.jitter * 1000.0) as u64; 
        let dynamic_delay = (jitter_us * 2) + 5_000; 

        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64;

        if dynamic_delay > now { continue; }
        let target_time = now - dynamic_delay;

        let best_pos = {
            let buf = buffer.lock().unwrap();
            buf.get_state_at(target_time)
        };

        if let Some(pos) = best_pos {
            let payload = pos.encode();
            let seq = seq_counter.fetch_add(1, Ordering::Relaxed);
            let msg = ProtoMessage::new(msg_type::POSITION, seq, payload);
            let _ = socket.send_to(&msg.serialize(), &target_addr).await;
        }
    }
}
