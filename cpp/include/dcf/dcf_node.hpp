// SPDX-License-Identifier: LGPL-3.0-only
#ifndef DCF_NODE_HPP
#define DCF_NODE_HPP

// The C++ "supercharged gRPC" DCF node. Replaces the old non-building
// dcf_client.h stub. Carries the certified 17-byte DeModFrame quantum (and
// SuperPacks, and the game/audio/text adapters as frame bursts) over a gRPC
// DCFService (cpp/proto/dcf.proto): unary SendFrame, server-streaming Subscribe,
// bidirectional MeshStream, and Ping/RTT — with health + reflection enabled.
//
//   dcfcpp serve   [--port 50051]
//   dcfcpp connect --peer host:port      (ping + frame + superpack over MeshStream)
//   dcfcpp send-frame --peer host:port [--hex ..]
//   dcfcpp bench    --peer host:port [--count 100]

#include <string>

namespace dcf {

// Run the gRPC server until terminated. Returns a process exit code.
int run_server(const std::string& listen_addr, const std::string& node_id);

// Connect to a peer and exercise the mesh (ping + a frame + a superpack over the
// bidi stream). Returns a process exit code.
int run_client(const std::string& peer, const std::string& node_id);

// Send a single frame (unary SendFrame) to a peer. `hex` empty => the golden frame.
int send_frame(const std::string& peer, const std::string& hex);

// Ping a peer `count` times and report RTT.
int bench(const std::string& peer, int count);

}  // namespace dcf

#endif  // DCF_NODE_HPP
