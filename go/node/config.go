// SPDX-License-Identifier: LGPL-3.0-only

package node

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

// Defaults mirror the Rust SDK constants (rust/src/lib.rs).
const (
	DefaultUDPPort           uint16 = 7777
	DefaultUDPMTU            int    = 1400
	DefaultReliableTimeoutMs uint64 = 500
	DefaultMaxRetries        uint32 = 3
	DefaultRTTThreshold      uint64 = 50
)

// DcfConfig is the working-subset node configuration (the UDP transport fields the Go node
// actually uses). It mirrors the corresponding fields of the Rust DcfConfig; gRPC/StreamDB
// fields are intentionally omitted as out of scope for this stdlib-only UDP node.
type DcfConfig struct {
	Transport          string   `json:"transport"`
	Host               string   `json:"host"`
	UDPPort            uint16   `json:"udp_port"`
	Mode               string   `json:"mode"`
	NodeID             string   `json:"node_id,omitempty"`
	Peers              []string `json:"peers,omitempty"`
	GroupRTTThreshold  uint64   `json:"group_rtt_threshold"`
	RetryMax           uint32   `json:"retry_max"`
	UDPMtu             int      `json:"udp_mtu"`
	UDPReliableTimeout uint64   `json:"udp_reliable_timeout"`
}

// DefaultConfig returns a DcfConfig with the Rust SDK defaults (UDP transport, p2p mode,
// bind 0.0.0.0:7777, 500 ms reliable timeout, 3 retries, 50 ms group RTT threshold).
func DefaultConfig() DcfConfig {
	return DcfConfig{
		Transport:          "UDP",
		Host:               "0.0.0.0",
		UDPPort:            DefaultUDPPort,
		Mode:               "p2p",
		Peers:              nil,
		GroupRTTThreshold:  DefaultRTTThreshold,
		RetryMax:           DefaultMaxRetries,
		UDPMtu:             DefaultUDPMTU,
		UDPReliableTimeout: DefaultReliableTimeoutMs,
	}
}

// InitFromConfigFile loads a DcfConfig from a JSON file, starting from DefaultConfig() so
// omitted keys keep their defaults. Mirrors init_from_config_file in the Rust SDK for the
// .json path. TOML is unsupported (a stdlib-only build has no TOML parser).
func InitFromConfigFile(path string) (*DcfConfig, error) {
	if strings.HasSuffix(path, ".toml") {
		return nil, fmt.Errorf("node: unsupported config format (TOML needs a non-stdlib dep, out of scope); use .json")
	}
	if !strings.HasSuffix(path, ".json") {
		return nil, fmt.Errorf("node: unknown config format %q; use .json", path)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("node: failed to read config: %w", err)
	}
	cfg := DefaultConfig()
	if err := json.Unmarshal(data, &cfg); err != nil {
		return nil, fmt.Errorf("node: JSON parse error: %w", err)
	}
	return &cfg, nil
}
