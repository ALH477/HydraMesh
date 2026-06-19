// SPDX-License-Identifier: LGPL-3.0-only
//! Self-healing mesh runtime for the Rust SDK — the live counterpart of the
//! certified algorithms in `dcf_wire_codec::mesh`. A direct port of
//! `go/node/mesh_runtime.go` and `C_SDK/node/dcf_mesh_runtime.h`, byte-compatible
//! on the wire (DCF-Mesh REPORT/ROLE carried as a `MsgMesh` ProtoMessage payload),
//! so a Rust node meshes with the Go and C nodes.
//!
//! Per ~1s tick: fold each peer's PONG/timeout into a sliding health window
//! (`mesh::peer_status`), PING every peer, then by mode:
//!   - `auto`   — REPORT our peer view to the master + decentralized failover.
//!   - `master` — aggregate reporters' topology, `mesh::elect`, broadcast ROLE.
//!   - `p2p`    — health FSM only.

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use parking_lot::Mutex;

use dcf_wire_codec::mesh;

use crate::DcfNode;

// Health FSM tuning — identical to the Go/C runtimes.
const MESH_WINDOW_CAP: usize = 5; // sliding health-window size
const MESH_FAIL_THR: i32 = 3; // consecutive timeouts -> Unreachable
const MESH_OK_THR: i32 = 2; // consecutive PONGs -> recover from Unreachable
const MESH_RELAY_MIN_DEGREE: i32 = 2; // healthy degree required for a Relay role

/// Per-peer liveness state, advanced once per tick.
#[derive(Default)]
struct PeerHealth {
    answered: bool,  // set true by a PONG, consumed (reset) each tick
    window: Vec<u8>, // last <=MESH_WINDOW_CAP events (1 = ok, 0 = timeout)
    status: i32,     // mesh::HEALTHY | DEGRADED | UNREACHABLE
}

/// Mutable runtime state behind a single lock.
struct Inner {
    role: i32,
    master: u16,
    health: HashMap<String, PeerHealth>,
    // master-side aggregation: reporter node id -> its reported [peer_id, status, rtt]
    reports: HashMap<u16, Vec<[i32; 3]>>,
    // master-side: reporter node id -> source addr, for unicasting ROLE back
    report_addr: HashMap<u16, SocketAddr>,
}

/// The self-healing mesh runtime. Construct via [`DcfNode::enable_mesh`].
pub struct MeshRuntime {
    mode: String,        // "p2p" | "auto" | "master"
    node_id: u16,        // this node's numeric mesh id
    master_peer: String, // peer id (string) to REPORT to, in auto mode
    group_thr: i32,      // RTT threshold for the status-line group id
    inner: Mutex<Inner>,
}

fn status_name(s: i32) -> &'static str {
    match s {
        mesh::HEALTHY => "healthy",
        mesh::DEGRADED => "degraded",
        _ => "unreachable",
    }
}

fn role_name(r: i32) -> &'static str {
    match r {
        mesh::MASTER => "master",
        mesh::RELAY => "relay",
        _ => "leaf",
    }
}

impl MeshRuntime {
    /// Build a runtime. A `master` node starts as MASTER of itself; others as LEAF.
    pub fn new(mode: &str, node_id: u16, master_peer: &str, group_thr: i32) -> Self {
        let (role, master) = if mode == "master" {
            (mesh::MASTER, node_id)
        } else {
            (mesh::LEAF, 0)
        };
        MeshRuntime {
            mode: mode.to_string(),
            node_id,
            master_peer: master_peer.to_string(),
            group_thr: group_thr.max(1),
            inner: Mutex::new(Inner {
                role,
                master,
                health: HashMap::new(),
                reports: HashMap::new(),
                report_addr: HashMap::new(),
            }),
        }
    }

    /// Current status for a peer id (HEALTHY for a never-seen peer).
    fn status_of(inner: &Inner, id: &str) -> i32 {
        inner.health.get(id).map(|h| h.status).unwrap_or(mesh::HEALTHY)
    }

    /// Record a PONG for `id`; folded into the FSM on the next tick.
    pub fn mark_answered(&self, id: &str) {
        self.inner.lock().health.entry(id.to_string()).or_default().answered = true;
    }

    /// Handle an incoming DCF-Mesh control message (REPORT or ROLE).
    pub fn handle_control(&self, payload: &[u8], from: SocketAddr) {
        if let Some((nid, peers)) = mesh::unpack_report(payload) {
            // master side: stash the reporter's view + where to send its ROLE
            let mut inner = self.inner.lock();
            inner.reports.insert(nid as u16, peers);
            inner.report_addr.insert(nid as u16, from);
            return;
        }
        if let Some((nid, role, master)) = mesh::unpack_role(payload) {
            if nid as u16 != self.node_id {
                return;
            }
            let mut inner = self.inner.lock();
            let changed = inner.role != role || inner.master != master as u16;
            inner.role = role;
            inner.master = master as u16;
            drop(inner);
            if changed {
                log::info!("mesh: role assigned -> {} (master {})", role_name(role), master);
            }
        }
    }

    /// Fold this tick's PONG/timeout into every peer's health window.
    fn health_tick(&self, peer_ids: &[String]) {
        let mut inner = self.inner.lock();
        for id in peer_ids {
            let h = inner.health.entry(id.clone()).or_default();
            let ev: u8 = if h.answered { 1 } else { 0 };
            h.answered = false;
            h.window.push(ev);
            if h.window.len() > MESH_WINDOW_CAP {
                let cut = h.window.len() - MESH_WINDOW_CAP;
                h.window.drain(0..cut);
            }
            h.status = mesh::peer_status(&h.window, MESH_FAIL_THR, MESH_OK_THR);
        }
    }

    /// auto mode: REPORT our peer view to the configured master peer.
    fn send_report(&self, node: &DcfNode, peer_ids: &[String]) {
        if self.master_peer.is_empty() {
            return;
        }
        let peers: Vec<[i32; 3]> = {
            let inner = self.inner.lock();
            peer_ids
                .iter()
                .filter_map(|id| {
                    id.parse::<i32>()
                        .ok()
                        .map(|pid| [pid, Self::status_of(&inner, id), node.peer_rtt_ms(id)])
                })
                .collect()
        };
        let payload = mesh::pack_report(self.node_id as i32, &peers);
        if let Some((host, port)) = node.get_peer_info(&self.master_peer) {
            node.send_mesh(payload, &host, port);
        }
    }

    /// master mode: aggregate the reported topology, elect, broadcast ROLE.
    fn run_election(&self, node: &DcfNode) {
        // 1. Build the node-id set and the deduplicated, canonicalized edge set.
        let (idset, dedup, addrs) = {
            let inner = self.inner.lock();
            let mut idset: std::collections::BTreeSet<u16> = std::collections::BTreeSet::new();
            idset.insert(self.node_id);
            let mut dedup: HashMap<(u16, u16), i32> = HashMap::new();
            for (reporter, peers) in inner.reports.iter() {
                idset.insert(*reporter);
                for p in peers {
                    let peer_id = p[0] as u16;
                    idset.insert(peer_id);
                    if p[1] == mesh::UNREACHABLE {
                        continue; // skip dead links
                    }
                    let (a, b) = if *reporter <= peer_id {
                        (*reporter, peer_id)
                    } else {
                        (peer_id, *reporter)
                    };
                    let w = p[2];
                    dedup
                        .entry((a, b))
                        .and_modify(|cur| {
                            if w < *cur {
                                *cur = w;
                            }
                        })
                        .or_insert(w);
                }
            }
            (idset, dedup, inner.report_addr.clone())
        };

        // 2. Map node ids to a dense 0..N-1 index (sorted, deterministic).
        let ids: Vec<u16> = idset.into_iter().collect();
        let index: HashMap<u16, usize> = ids.iter().enumerate().map(|(i, id)| (*id, i)).collect();
        let edges: Vec<[i32; 3]> = dedup
            .iter()
            .map(|((a, b), w)| [index[a] as i32, index[b] as i32, *w])
            .collect();

        // 3. Elect.
        let (master_idx, roles) = mesh::elect(ids.len(), &edges, MESH_RELAY_MIN_DEGREE);
        let master_id = ids[master_idx as usize];

        // 4. Adopt our own role + master.
        {
            let mut inner = self.inner.lock();
            if let Some(&self_idx) = index.get(&self.node_id) {
                inner.role = roles[self_idx];
            }
            inner.master = master_id;
        }

        // 5. Unicast each reporter its assigned role.
        for (reporter, addr) in addrs {
            if let Some(&idx) = index.get(&reporter) {
                let payload = mesh::pack_role(reporter as i32, roles[idx], master_id as i32);
                node.send_mesh_addr(payload, addr);
            }
        }
    }

    /// auto mode: if our master peer is Unreachable, locally re-elect the lowest-id
    /// healthy node (decentralized failover — no central point of failure).
    fn check_master_failover(&self, peer_ids: &[String]) {
        let mut inner = self.inner.lock();
        if self.master_peer.is_empty()
            || Self::status_of(&inner, &self.master_peer) != mesh::UNREACHABLE
        {
            return;
        }
        let mut best = self.node_id as i32;
        for id in peer_ids {
            if Self::status_of(&inner, id) == mesh::UNREACHABLE {
                continue;
            }
            if let Ok(pid) = id.parse::<i32>() {
                if pid < best {
                    best = pid;
                }
            }
        }
        let prev = inner.master;
        let changed = prev != best as u16;
        inner.master = best as u16;
        if best == self.node_id as i32 {
            inner.role = mesh::MASTER;
        }
        drop(inner);
        if changed {
            log::info!(
                "mesh: master {} unreachable -> local re-election, new master {}",
                prev,
                best
            );
        }
    }

    /// Emit the periodic mesh-status line.
    fn log_status(&self, node: &DcfNode, peer_ids: &[String]) {
        let inner = self.inner.lock();
        let parts: Vec<String> = peer_ids
            .iter()
            .map(|id| {
                let st = Self::status_of(&inner, id);
                let grp = mesh::group_of(node.peer_rtt_ms(id), self.group_thr, st);
                format!("{}:{}:g{}", id, status_name(st), grp)
            })
            .collect();
        let (role, master) = (inner.role, inner.master);
        drop(inner);
        log::info!(
            "mesh-status node={} mode={} role={} master={} peers=[{}]",
            self.node_id,
            self.mode,
            role_name(role),
            master,
            parts.join(" ")
        );
    }

    /// The mesh loop: one tick per second until the node stops.
    pub async fn run(self: Arc<Self>, node: Arc<DcfNode>) {
        let mut ticker = tokio::time::interval(Duration::from_secs(1));
        let mut tick: u64 = 0;
        loop {
            ticker.tick().await;
            if !node.is_running() {
                return;
            }
            let peers = node.list_peers();
            self.health_tick(&peers);
            for id in &peers {
                let _ = node.send_ping(id);
            }
            match self.mode.as_str() {
                "auto" => {
                    self.send_report(&node, &peers);
                    self.check_master_failover(&peers);
                }
                "master" => self.run_election(&node),
                _ => {} // p2p: health FSM only
            }
            tick += 1;
            if tick % 3 == 0 {
                self.log_status(&node, &peers);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fsm_window_unreachable_then_recovers() {
        let m = MeshRuntime::new("auto", 1, "0", 50);
        let peers = vec!["2".to_string()];
        // 3 consecutive timeouts -> Unreachable.
        for _ in 0..3 {
            m.health_tick(&peers);
        }
        assert_eq!(MeshRuntime::status_of(&m.inner.lock(), "2"), mesh::UNREACHABLE);
        // 2 consecutive PONGs -> recover to Healthy.
        for _ in 0..2 {
            m.mark_answered("2");
            m.health_tick(&peers);
        }
        assert_eq!(MeshRuntime::status_of(&m.inner.lock(), "2"), mesh::HEALTHY);
    }

    #[test]
    fn degraded_after_single_timeout() {
        let m = MeshRuntime::new("auto", 1, "0", 50);
        let peers = vec!["2".to_string()];
        m.mark_answered("2");
        m.health_tick(&peers); // ok
        m.health_tick(&peers); // one timeout
        assert_eq!(MeshRuntime::status_of(&m.inner.lock(), "2"), mesh::DEGRADED);
    }

    #[test]
    fn role_adoption_via_control() {
        let m = MeshRuntime::new("auto", 7, "0", 50);
        let payload = mesh::pack_role(7, mesh::RELAY, 3);
        m.handle_control(&payload, "127.0.0.1:9000".parse().unwrap());
        let inner = m.inner.lock();
        assert_eq!(inner.role, mesh::RELAY);
        assert_eq!(inner.master, 3);
    }
}
