// SPDX-License-Identifier: LGPL-3.0-only

package node

import "math"

// rttAlpha is the EWMA smoothing factor for RTT/jitter (DEFAULT_RTT_ALPHA in the Rust SDK).
const rttAlpha = 0.125

// NetworkStats holds counters plus a smoothed RTT/jitter estimate, mirroring the Rust SDK's
// NetworkStats. Counters are plain fields; callers serialise access (the endpoint guards its
// own stats with a mutex).
type NetworkStats struct {
	PacketsSent     uint64  `json:"packets_sent"`
	PacketsReceived uint64  `json:"packets_received"`
	BytesSent       uint64  `json:"bytes_sent"`
	BytesReceived   uint64  `json:"bytes_received"`
	PacketsLost     uint64  `json:"packets_lost"`
	Retransmits     uint64  `json:"retransmits"`
	LastRTT         float64 `json:"last_rtt"`
	AvgRTT          float64 `json:"avg_rtt"`
	Jitter          float64 `json:"jitter"`
}

// UpdateRTT folds a fresh RTT sample (milliseconds) into the smoothed averages using the
// same EWMA as the Rust SDK: AvgRTT = a*rtt + (1-a)*AvgRTT and Jitter = a*|rtt-LastRTT| +
// (1-a)*Jitter, then LastRTT = rtt.
func (s *NetworkStats) UpdateRTT(rtt float64) {
	prev := s.LastRTT
	s.AvgRTT = rttAlpha*rtt + (1.0-rttAlpha)*s.AvgRTT
	jitter := math.Abs(rtt - prev)
	s.Jitter = rttAlpha*jitter + (1.0-rttAlpha)*s.Jitter
	s.LastRTT = rtt
}

// PeerDetail is a peer with its address and latest per-peer network stats — for UI/recorder
// consumption (mirrors the Rust SDK's PeerDetail).
type PeerDetail struct {
	ID    string       `json:"id"`
	Host  string       `json:"host"`
	Port  uint16       `json:"port"`
	Stats NetworkStats `json:"stats"`
}
