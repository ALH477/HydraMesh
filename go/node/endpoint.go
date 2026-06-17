// SPDX-License-Identifier: LGPL-3.0-only

package node

import (
	"encoding/binary"
	"fmt"
	"net"
	"sync"
	"time"
)

// reliablePacket tracks an in-flight reliable ProtoMessage so the reliable handler can
// retransmit it until ACKed or retries are exhausted. Mirrors the Rust ReliablePacket.
type reliablePacket struct {
	msg      ProtoMessage
	host     string
	port     uint16
	attempts uint32
	sentTime time.Time
}

// UdpEndpoint owns the UDP socket and the reliable-delivery bookkeeping. It mirrors the Rust
// UdpEndpoint: a socket, smoothed NetworkStats, a sequence->reliablePacket table, and an
// ack-received set, all guarded for concurrent access.
type UdpEndpoint struct {
	conn *net.UDPConn
	port uint16

	statsMu sync.Mutex
	stats   NetworkStats

	relMu    sync.Mutex
	reliable map[uint32]*reliablePacket
	acked    map[uint32]bool

	reliableTimeout time.Duration
	maxRetries      uint32
}

// NewUdpEndpoint binds a UDP socket on host:port (host "" => all interfaces). A port of 0
// asks the OS for an ephemeral port; read it back with Port().
func NewUdpEndpoint(host string, port uint16) (*UdpEndpoint, error) {
	ip := net.IPv4zero
	if host != "" {
		if parsed := net.ParseIP(host); parsed != nil {
			ip = parsed
		}
	}
	conn, err := net.ListenUDP("udp", &net.UDPAddr{IP: ip, Port: int(port)})
	if err != nil {
		return nil, fmt.Errorf("node: udp listen failed: %w", err)
	}
	actual := uint16(conn.LocalAddr().(*net.UDPAddr).Port)
	return &UdpEndpoint{
		conn:            conn,
		port:            actual,
		reliable:        make(map[uint32]*reliablePacket),
		acked:           make(map[uint32]bool),
		reliableTimeout: time.Duration(DefaultReliableTimeoutMs) * time.Millisecond,
		maxRetries:      DefaultMaxRetries,
	}, nil
}

// Port returns the actual bound UDP port (resolved if 0 was requested).
func (e *UdpEndpoint) Port() uint16 { return e.port }

// Close closes the underlying socket.
func (e *UdpEndpoint) Close() error { return e.conn.Close() }

// SendRaw sends pre-serialised bytes to addr and folds the send into the TX stats.
func (e *UdpEndpoint) SendRaw(data []byte, addr *net.UDPAddr) error {
	if _, err := e.conn.WriteToUDP(data, addr); err != nil {
		return fmt.Errorf("node: udp send failed: %w", err)
	}
	e.statsMu.Lock()
	e.stats.PacketsSent++
	e.stats.BytesSent += uint64(len(data))
	e.statsMu.Unlock()
	return nil
}

// SendMessage resolves host:port and sends a serialised ProtoMessage. When reliable, the
// message is recorded (keyed by its sequence) before sending so the reliable handler can
// retransmit it until an ACK arrives.
func (e *UdpEndpoint) SendMessage(msg *ProtoMessage, host string, port uint16, reliable bool) error {
	addr, err := net.ResolveUDPAddr("udp", fmt.Sprintf("%s:%d", host, port))
	if err != nil {
		return fmt.Errorf("node: invalid address %s:%d: %w", host, port, err)
	}
	if reliable {
		e.relMu.Lock()
		e.reliable[msg.Sequence] = &reliablePacket{
			msg:      *msg,
			host:     host,
			port:     port,
			attempts: 1,
			sentTime: time.Now(),
		}
		e.relMu.Unlock()
	}
	return e.SendRaw(msg.Serialize(), addr)
}

// SendACK sends a MsgAck whose payload is the big-endian acked sequence (mirrors Rust
// send_ack).
func (e *UdpEndpoint) SendACK(seq uint32, addr *net.UDPAddr) error {
	payload := make([]byte, 4)
	binary.BigEndian.PutUint32(payload, seq)
	msg := NewProtoMessage(MsgAck, 0, payload)
	return e.SendRaw(msg.Serialize(), addr)
}

// SendPong echoes a ping's sequence and originating timestamp back as a MsgPong (mirrors
// Rust send_pong); the original timestamp lets the pinger compute RTT.
func (e *UdpEndpoint) SendPong(seq uint32, ts uint64, addr *net.UDPAddr) error {
	msg := &ProtoMessage{MsgType: MsgPong, Sequence: seq, Timestamp: ts, Payload: nil}
	return e.SendRaw(msg.Serialize(), addr)
}

// Stats returns a snapshot of the endpoint's network statistics.
func (e *UdpEndpoint) Stats() NetworkStats {
	e.statsMu.Lock()
	defer e.statsMu.Unlock()
	return e.stats
}

// markAcked records an ACK and clears any matching in-flight reliable packet.
func (e *UdpEndpoint) markAcked(seq uint32) {
	e.relMu.Lock()
	e.acked[seq] = true
	delete(e.reliable, seq)
	e.relMu.Unlock()
}

// addRecvStats folds a received datagram into the RX stats (the Rust receiver counts
// 17 + payload bytes per message).
func (e *UdpEndpoint) addRecvStats(payloadLen int) {
	e.statsMu.Lock()
	e.stats.PacketsReceived++
	e.stats.BytesReceived += uint64(protoHeaderLen + payloadLen)
	e.statsMu.Unlock()
}

// recordRTT folds an RTT sample (milliseconds) into the endpoint-wide smoothed stats.
func (e *UdpEndpoint) recordRTT(rttMs float64) {
	e.statsMu.Lock()
	e.stats.UpdateRTT(rttMs)
	e.statsMu.Unlock()
}
