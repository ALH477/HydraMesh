// SPDX-License-Identifier: LGPL-3.0-only

// Package mesh implements the DCF self-healing mesh algorithms — the certified,
// deterministic layer, value-identical to python/MCP/meshlab_core.py,
// codec/demod_mesh.h, and codec/src/mesh.rs. Pure integer functions over small-int
// node ids (0..n-1); the DcfNode runtime drives them from live PING/PONG + the
// DCF-Mesh control adapter. Pinned by Documentation/mesh_vectors.json.
package mesh

import "sort"

// Peer status (matches DCFPeerStatus intent).
const (
	Healthy     = 0
	Degraded    = 1
	Unreachable = 2
)

// Roles (AUTO mode).
const (
	Leaf   = 0
	Relay  = 1
	Master = 2
)

const (
	Inf      = 0x3FFFFFFF
	NoHop    = -1
	Isolated = -1
)

// PeerStatus folds a health-check sequence (1 = ok, 0 = timeout) into a status.
// Sticky Unreachable until okThreshold consecutive successes recover it.
func PeerStatus(events []byte, failThreshold, okThreshold int) int {
	status, cf, cs := Healthy, 0, 0
	for _, e := range events {
		if e != 0 {
			cs++
			cf = 0
		} else {
			cf++
			cs = 0
		}
		switch {
		case cf >= failThreshold:
			status = Unreachable
		case status == Unreachable:
			if cs >= okThreshold {
				status = Healthy
			}
		case cf >= 1:
			status = Degraded
		default:
			status = Healthy
		}
	}
	return status
}

// GroupOf returns a peer's RTT group: floor(rtt/threshold), or Isolated if unreachable.
func GroupOf(rttMs, thresholdMs, status int) int {
	if status == Unreachable || rttMs >= Inf {
		return Isolated
	}
	return rttMs / thresholdMs
}

// Dijkstra computes RTT-weighted shortest paths from source. edges are undirected
// {u, v, w}. Returns dist and nextHop (first hop from source; NoHop for
// source/unreachable). Deterministic tie-break: smaller predecessor id.
func Dijkstra(n int, edges [][3]int, source int) (dist, nextHop []int) {
	dist = make([]int, n)
	prev := make([]int, n)
	done := make([]bool, n)
	for i := range dist {
		dist[i] = Inf
		prev[i] = NoHop
	}
	dist[source] = 0
	for it := 0; it < n; it++ {
		u := NoHop
		for i := 0; i < n; i++ {
			if !done[i] && dist[i] < Inf && (u == NoHop || dist[i] < dist[u]) {
				u = i
			}
		}
		if u == NoHop {
			break
		}
		done[u] = true
		for _, e := range edges {
			a, b, w := e[0], e[1], e[2]
			var v int
			if a == u {
				v = b
			} else if b == u {
				v = a
			} else {
				continue
			}
			nd := dist[u] + w
			if nd < dist[v] || (nd == dist[v] && u < prev[v]) {
				dist[v] = nd
				prev[v] = u
			}
		}
	}
	nextHop = make([]int, n)
	for v := 0; v < n; v++ {
		nextHop[v] = NoHop
		if v == source || dist[v] >= Inf {
			continue
		}
		hop := v
		for prev[hop] != source {
			hop = prev[hop]
		}
		nextHop[v] = hop
	}
	return dist, nextHop
}

// SelectRoutes orders candidate routes {routeID, status, rtt} into a failover list:
// drops Unreachable, then (status, rtt, routeID) ascending. Returns the route ids.
func SelectRoutes(cand [][3]int) []int {
	usable := make([][3]int, 0, len(cand))
	for _, c := range cand {
		if c[1] != Unreachable {
			usable = append(usable, c)
		}
	}
	sort.Slice(usable, func(i, j int) bool {
		a, b := usable[i], usable[j]
		if a[1] != b[1] {
			return a[1] < b[1]
		}
		if a[2] != b[2] {
			return a[2] < b[2]
		}
		return a[0] < b[0]
	})
	out := make([]int, len(usable))
	for i, c := range usable {
		out[i] = c[0]
	}
	return out
}

// Elect performs deterministic master election + role assignment over the healthy
// topology (edges {u, v, w}). Master = max healthy degree, tie-break min avg RTT,
// then min id. Returns the master id and roles for each node.
func Elect(n int, edges [][3]int, relayMinDegree int) (master int, roles []int) {
	deg := make([]int, n)
	sum := make([]int, n)
	for _, e := range edges {
		u, v, w := e[0], e[1], e[2]
		deg[u]++
		sum[u] += w
		deg[v]++
		sum[v] += w
	}
	avg := make([]int, n)
	for i := 0; i < n; i++ {
		if deg[i] != 0 {
			avg[i] = sum[i] / deg[i]
		} else {
			avg[i] = Inf
		}
	}
	master = 0
	for i := 1; i < n; i++ {
		if deg[i] > deg[master] || (deg[i] == deg[master] && avg[i] < avg[master]) {
			master = i
		}
	}
	roles = make([]int, n)
	for i := 0; i < n; i++ {
		if deg[i] >= relayMinDegree {
			roles[i] = Relay
		} else {
			roles[i] = Leaf
		}
	}
	roles[master] = Master
	return master, roles
}

// ── DCF-Mesh control adapter (REPORT / ROLE) ──────────────────────────────────
// Carried as the payload of a ProtoMessage MsgMesh (=11). Big-endian, byte-exact.
const (
	MeshReport  = 0
	MeshRole    = 1
	MeshVersion = 1
	MsgMesh     = 11 // ProtoMessage type carrying a DCF-Mesh control message
)

// PackReport serialises a REPORT. peers are {peerID, status, rtt}.
func PackReport(nodeID int, peers [][3]int) []byte {
	out := []byte{MeshReport, MeshVersion, byte(nodeID >> 8), byte(nodeID), byte(len(peers))}
	for _, p := range peers {
		out = append(out, byte(p[0]>>8), byte(p[0]), byte(p[1]), byte(p[2]>>8), byte(p[2]))
	}
	return out
}

// UnpackReport parses a REPORT into (nodeID, peers). ok is false on bad input.
func UnpackReport(buf []byte) (nodeID int, peers [][3]int, ok bool) {
	if len(buf) < 5 || buf[0] != MeshReport || buf[1] != MeshVersion {
		return 0, nil, false
	}
	nodeID = int(buf[2])<<8 | int(buf[3])
	n := int(buf[4])
	if len(buf) < 5+5*n {
		return 0, nil, false
	}
	peers = make([][3]int, n)
	off := 5
	for i := 0; i < n; i++ {
		peers[i] = [3]int{int(buf[off])<<8 | int(buf[off+1]), int(buf[off+2]), int(buf[off+3])<<8 | int(buf[off+4])}
		off += 5
	}
	return nodeID, peers, true
}

// PackRole serialises a ROLE (7 bytes).
func PackRole(nodeID, role, masterID int) []byte {
	return []byte{MeshRole, MeshVersion, byte(nodeID >> 8), byte(nodeID), byte(role), byte(masterID >> 8), byte(masterID)}
}

// UnpackRole parses a ROLE into (nodeID, role, masterID). ok is false on bad input.
func UnpackRole(buf []byte) (nodeID, role, masterID int, ok bool) {
	if len(buf) < 7 || buf[0] != MeshRole || buf[1] != MeshVersion {
		return 0, 0, 0, false
	}
	return int(buf[2])<<8 | int(buf[3]), int(buf[4]), int(buf[5])<<8 | int(buf[6]), true
}

// MeshMsgType returns the control message type byte (-1 if empty).
func MeshMsgType(buf []byte) int {
	if len(buf) == 0 {
		return -1
	}
	return int(buf[0])
}
