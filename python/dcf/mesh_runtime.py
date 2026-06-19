# SPDX-License-Identifier: LGPL-3.0-only
"""Self-healing mesh runtime for the Python SDK.

The live counterpart of the certified algorithms in ``python/MCP/meshlab_core.py``,
and a direct port of ``go/node/mesh_runtime.go`` / ``rust/src/mesh_runtime.rs``. It is
byte-compatible on the wire (DCF-Mesh REPORT/ROLE carried as a ``MSG_MESH``
ProtoMessage payload), so a Python node meshes with the Go/C/Rust nodes.

Per ~1s tick: fold each peer's PONG/timeout into a sliding health window
(``meshlab_core.peer_status``), PING every peer, then by mode:
  - ``auto``   — REPORT our peer view to the master + decentralized failover.
  - ``master`` — aggregate reporters' topology, ``meshlab_core.elect``, broadcast ROLE.
  - ``p2p``    — health FSM only.
"""

import logging
import os
import sys
import threading
import time

# The certified primitives live in python/MCP — pure functions, no deps.
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))
import meshlab_core as mesh  # noqa: E402

log = logging.getLogger("dcf.mesh")

# Health FSM tuning — identical to the Go/C/Rust runtimes.
WINDOW_CAP = 5
FAIL_THR = 3        # consecutive timeouts -> UNREACHABLE
OK_THR = 2          # consecutive PONGs -> recover from UNREACHABLE
RELAY_MIN_DEGREE = 2
TICK_SECONDS = 1.0


def _status_name(s):
    return {mesh.HEALTHY: "healthy", mesh.DEGRADED: "degraded"}.get(s, "unreachable")


def _role_name(r):
    return {mesh.MASTER: "master", mesh.RELAY: "relay"}.get(r, "leaf")


class _PeerHealth:
    __slots__ = ("answered", "window", "status")

    def __init__(self):
        self.answered = False
        self.window = []
        self.status = mesh.HEALTHY


class MeshRuntime:
    """Drives the certified mesh algorithms from live PING/PONG + REPORT/ROLE."""

    def __init__(self, mode, node_id, master_peer="", group_thr=50):
        self.mode = mode                       # "p2p" | "auto" | "master"
        self.node_id = int(node_id)
        self.master_peer = str(master_peer)
        self.group_thr = max(1, int(group_thr))
        self.role = mesh.MASTER if mode == "master" else mesh.LEAF
        self.master = self.node_id if mode == "master" else 0

        self._lock = threading.Lock()
        self._health = {}                      # peer_id (str) -> _PeerHealth
        self._reports = {}                     # reporter node id -> [[peer_id,status,rtt],..]
        self._report_addr = {}                 # reporter node id -> source addr
        self._thread = None
        self._running = False

    # ── status helpers ───────────────────────────────────────────────────────
    def _status_of(self, peer_id):
        h = self._health.get(str(peer_id))
        return h.status if h else mesh.HEALTHY

    # ── inbound hooks (called from the node's rx thread) ─────────────────────--
    def mark_answered(self, peer_id):
        with self._lock:
            self._health.setdefault(str(peer_id), _PeerHealth()).answered = True

    def handle_control(self, payload, addr):
        """Dispatch an incoming DCF-Mesh control message (REPORT or ROLE)."""
        mtype = mesh.mesh_msg_type(payload)
        if mtype == mesh.MESH_REPORT:
            try:
                nid, peers = mesh.unpack_report(payload)
            except ValueError:
                return
            with self._lock:
                self._reports[nid] = peers
                self._report_addr[nid] = addr
        elif mtype == mesh.MESH_ROLE:
            try:
                nid, role, master = mesh.unpack_role(payload)
            except ValueError:
                return
            if nid != self.node_id:
                return
            with self._lock:
                changed = self.role != role or self.master != master
                self.role, self.master = role, master
            if changed:
                log.info("mesh: role assigned -> %s (master %d)", _role_name(role), master)

    # ── per-tick steps ────────────────────────────────────────────────────────
    def _health_tick(self, peer_ids):
        with self._lock:
            for pid in peer_ids:
                h = self._health.setdefault(pid, _PeerHealth())
                h.window.append(1 if h.answered else 0)
                h.answered = False
                if len(h.window) > WINDOW_CAP:
                    h.window = h.window[-WINDOW_CAP:]
                h.status = mesh.peer_status(h.window, FAIL_THR, OK_THR)

    def _send_report(self, node, peer_ids):
        if not self.master_peer:
            return
        with self._lock:
            peers = []
            for pid in peer_ids:
                try:
                    pint = int(pid)
                except ValueError:
                    continue
                peers.append((pint, self._status_of(pid), node.peer_rtt_ms(pid)))
        payload = mesh.pack_report(self.node_id, peers)
        info = node.peer_info(self.master_peer)
        if info:
            node.send_mesh(payload, info[0], info[1])

    def _run_election(self, node):
        # 1. node-id set + deduplicated, canonicalized edges.
        with self._lock:
            idset = {self.node_id}
            dedup = {}
            for reporter, peers in self._reports.items():
                idset.add(reporter)
                for peer_id, status, rtt in peers:
                    idset.add(peer_id)
                    if status == mesh.UNREACHABLE:
                        continue
                    a, b = (reporter, peer_id) if reporter <= peer_id else (peer_id, reporter)
                    if (a, b) not in dedup or rtt < dedup[(a, b)]:
                        dedup[(a, b)] = rtt
            addrs = dict(self._report_addr)

        # 2. dense 0..N-1 index (sorted, deterministic).
        ids = sorted(idset)
        index = {nid: i for i, nid in enumerate(ids)}
        edges = [[index[a], index[b], w] for (a, b), w in dedup.items()]

        # 3. elect.
        master_idx, roles = mesh.elect(len(ids), edges, RELAY_MIN_DEGREE)
        master_id = ids[master_idx]

        # 4. adopt our own role + master.
        with self._lock:
            if self.node_id in index:
                self.role = roles[index[self.node_id]]
            self.master = master_id

        # 5. unicast each reporter its assigned role.
        for reporter, addr in addrs.items():
            if reporter in index:
                payload = mesh.pack_role(reporter, roles[index[reporter]], master_id)
                node.send_mesh(payload, addr[0], addr[1])

    def _check_master_failover(self, node, peer_ids):
        with self._lock:
            if not self.master_peer or self._status_of(self.master_peer) != mesh.UNREACHABLE:
                return
            best = self.node_id
            for pid in peer_ids:
                if self._status_of(pid) == mesh.UNREACHABLE:
                    continue
                try:
                    pint = int(pid)
                except ValueError:
                    continue
                if pint < best:
                    best = pint
            prev = self.master
            changed = prev != best
            self.master = best
            if best == self.node_id:
                self.role = mesh.MASTER
        if changed:
            log.info("mesh: master %d unreachable -> local re-election, new master %d", prev, best)

    def _log_status(self, node, peer_ids):
        with self._lock:
            parts = []
            for pid in peer_ids:
                st = self._status_of(pid)
                grp = mesh.group_of(node.peer_rtt_ms(pid), self.group_thr, st)
                parts.append("%s:%s:g%d" % (pid, _status_name(st), grp))
            role, master = self.role, self.master
        log.info("mesh-status node=%d mode=%s role=%s master=%d peers=[%s]",
                 self.node_id, self.mode, _role_name(role), master, " ".join(parts))

    # ── loop lifecycle (started by the node) ────────────────────────────────--
    def start(self, node):
        if self._running:
            return
        self._running = True
        self._thread = threading.Thread(target=self._run, args=(node,), name="dcf-mesh", daemon=True)
        self._thread.start()

    def stop(self):
        self._running = False
        if self._thread:
            self._thread.join(timeout=2)

    def _run(self, node):
        tick = 0
        while self._running and node.is_running():
            time.sleep(TICK_SECONDS)
            peers = node.list_peers()
            self._health_tick(peers)
            for pid in peers:
                node.send_ping(pid)
            if self.mode == "auto":
                self._send_report(node, peers)
                self._check_master_failover(node, peers)
            elif self.mode == "master":
                self._run_election(node)
            tick += 1
            if tick % 3 == 0:
                self._log_status(node, peers)
