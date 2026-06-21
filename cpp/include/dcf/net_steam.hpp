// SPDX-License-Identifier: LGPL-3.0-only
#ifndef DCF_NET_STEAM_HPP
#define DCF_NET_STEAM_HPP

// SteamNet — a DCF transport over Valve's ISteamNetworkingSockets API.
//
// The SAME source compiles against two backends, selected at build time:
//   • DCF_CPP_GNS   (default, open, hermetic): Valve's GameNetworkingSockets.
//                   Dedicated server via CreateListenSocketIP; client via
//                   ConnectByIPAddress. P2P NAT-traversal needs the WebRTC/ICE GNS
//                   build or the Steam backend below.
//   • DCF_CPP_STEAM (opt-in, developer-supplied Steamworks SDK): adds SDR relay
//                   (CreateListenSocketP2P / ConnectP2P), lobbies, server browser.
//
// Steam handles (HSteamListenSocket / HSteamNetConnection / HSteamNetPollGroup) are
// uint32 typedefs; we store them as plain integers so this header never needs the
// Steam SDK headers. See Documentation/DCF_STEAM_SPEC.md.

#include "dcf/transport.hpp"

#include <string>
#include <vector>

namespace dcf {

class SteamNet : public ITransport {
public:
    explicit SteamNet(std::string identity = "dcf-node");
    ~SteamNet() override;

    SteamNet(const SteamNet&) = delete;
    SteamNet& operator=(const SteamNet&) = delete;

    bool listen(std::uint16_t port) override;
    ConnId connect(const std::string& host, std::uint16_t port) override;
    bool send(ConnId c, const void* data, std::size_t len, bool reliable) override;
    int broadcast_except(ConnId except, const void* data, std::size_t len, bool reliable) override;
    void poll(std::vector<NetMessage>& out) override;
    std::vector<ConnId> connections() const override { return conns_; }
    void close(ConnId c) override;

    bool ok() const { return ok_; }
    bool is_server() const { return server_; }
    // Client side: the single outbound connection, and whether it has connected.
    ConnId client_conn() const { return client_conn_; }
    bool client_connected() const { return client_connected_; }

    // Invoked by the static connection-status trampoline; arg is a
    // SteamNetConnectionStatusChangedCallback_t* (opaque here).
    void on_conn_status(void* info);

private:
    void drain(std::vector<NetMessage>& out);

    std::string identity_;
    bool ok_ = false;
    bool server_ = false;
    std::uint32_t listen_sock_ = 0;   // HSteamListenSocket  (0 == invalid)
    std::uint32_t poll_group_ = 0;    // HSteamNetPollGroup
    ConnId client_conn_ = 0;
    bool client_connected_ = false;
    std::vector<ConnId> conns_;       // accepted (server) or outbound (client)
};

}  // namespace dcf

#endif  // DCF_NET_STEAM_HPP
