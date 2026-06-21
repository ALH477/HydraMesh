// SPDX-License-Identifier: LGPL-3.0-only
//
// SteamNet — DCF transport over Valve's ISteamNetworkingSockets. One source, two
// backends (DCF_CPP_GNS open / DCF_CPP_STEAM proprietary). See net_steam.hpp and
// Documentation/DCF_STEAM_SPEC.md.

#include "dcf/net_steam.hpp"

#include <algorithm>
#include <cstdio>
#include <cstring>

#if defined(DCF_CPP_STEAM)
// Proprietary Steamworks SDK (developer-supplied; see DCF_STEAM_SPEC.md). The
// SteamAPI/SteamGameServer context is initialised by the gameserver/lobby modules;
// here we only use the networking sockets + relay.
#include <steam/steam_api.h>
#include <steam/isteamnetworkingsockets.h>
#include <steam/isteamnetworkingutils.h>
#include <steam/steamnetworkingtypes.h>
#else
// Open GameNetworkingSockets (BSD-3; nixpkgs `gamenetworkingsockets`).
#include <steam/steamnetworkingsockets.h>
#include <steam/isteamnetworkingutils.h>
#endif

namespace dcf {

namespace {

// One node per process → a single active instance drives the status callback.
SteamNet* g_self = nullptr;
int g_init_count = 0;

void on_status_trampoline(SteamNetConnectionStatusChangedCallback_t* info) {
    if (g_self) g_self->on_conn_status(info);
}

bool backend_init(const char* identity_str, std::string& err) {
#if defined(DCF_CPP_STEAM)
    // Steamworks init (SteamAPI_Init / SteamGameServer_Init) is the caller's job;
    // turn on the Steam Datagram Relay so P2P + dedicated SDR sockets work.
    if (SteamNetworkingUtils()) SteamNetworkingUtils()->InitRelayNetworkAccess();
    (void)identity_str;
    (void)err;
    return true;
#else
    SteamNetworkingErrMsg m;
    SteamNetworkingIdentity id;
    id.Clear();
    if (identity_str && *identity_str) id.SetGenericString(identity_str);
    if (!GameNetworkingSockets_Init(id.IsInvalid() ? nullptr : &id, m)) {
        err = m;
        return false;
    }
    return true;
#endif
}

void backend_kill() {
#if !defined(DCF_CPP_STEAM)
    GameNetworkingSockets_Kill();
#endif
}

// Build the per-socket config that routes connection-status callbacks to us.
SteamNetworkingConfigValue_t status_cb_opt() {
    SteamNetworkingConfigValue_t opt;
    opt.SetPtr(k_ESteamNetworkingConfig_Callback_ConnectionStatusChanged,
               reinterpret_cast<void*>(on_status_trampoline));
    return opt;
}

void push_msg(std::vector<NetMessage>& out, SteamNetworkingMessage_t* m) {
    const char* d = static_cast<const char*>(m->m_pData);
    const int n = m->m_cbSize;
    NetMessage nm;
    nm.from = m->m_conn;
    if (n > 0) {
        nm.reliable = (d[0] & td::FLAG_RELIABLE) != 0;
        nm.data.assign(d + 1, static_cast<std::size_t>(n - 1));
    }
    out.push_back(std::move(nm));
}

}  // namespace

SteamNet::SteamNet(std::string identity) : identity_(std::move(identity)) {
    std::string err;
    if (g_init_count == 0) {
        if (!backend_init(identity_.c_str(), err)) {
            std::fprintf(stderr, "[steamnet] init failed: %s\n", err.c_str());
            return;
        }
    }
    ++g_init_count;
    g_self = this;
    ok_ = true;
}

SteamNet::~SteamNet() {
    if (!ok_) return;
    ISteamNetworkingSockets* s = SteamNetworkingSockets();
    if (s) {
        for (ConnId c : conns_) s->CloseConnection(c, 0, "shutdown", false);
        if (poll_group_) s->DestroyPollGroup(poll_group_);
        if (listen_sock_) s->CloseListenSocket(listen_sock_);
    }
    if (g_self == this) g_self = nullptr;
    if (--g_init_count == 0) backend_kill();
}

bool SteamNet::listen(std::uint16_t port) {
    if (!ok_) return false;
    server_ = true;
    ISteamNetworkingSockets* s = SteamNetworkingSockets();
    SteamNetworkingConfigValue_t opt = status_cb_opt();
#if defined(DCF_CPP_STEAM)
    // Dedicated server over the Steam Datagram Relay: a virtual port, IP hidden.
    listen_sock_ = s->CreateListenSocketP2P(static_cast<int>(port), 1, &opt);
#else
    SteamNetworkingIPAddr addr;
    addr.Clear();
    addr.SetIPv4(0, port);  // 0.0.0.0:port
    listen_sock_ = s->CreateListenSocketIP(addr, 1, &opt);
#endif
    poll_group_ = s->CreatePollGroup();
    return listen_sock_ != k_HSteamListenSocket_Invalid;
}

ConnId SteamNet::connect(const std::string& host, std::uint16_t port) {
    if (!ok_) return 0;
    ISteamNetworkingSockets* s = SteamNetworkingSockets();
    SteamNetworkingConfigValue_t opt = status_cb_opt();
#if defined(DCF_CPP_STEAM)
    // P2P over SDR: `host` is the peer identity (generic string / SteamID string).
    SteamNetworkingIdentity id;
    id.Clear();
    id.SetGenericString(host.c_str());
    client_conn_ = s->ConnectP2P(id, static_cast<int>(port), 1, &opt);
#else
    SteamNetworkingIPAddr addr;
    addr.Clear();
    const std::string hp = host + ":" + std::to_string(port);
    if (!addr.ParseString(hp.c_str())) {
        std::fprintf(stderr, "[steamnet] bad address %s\n", hp.c_str());
        return 0;
    }
    client_conn_ = s->ConnectByIPAddress(addr, 1, &opt);
#endif
    if (client_conn_ != k_HSteamNetConnection_Invalid) conns_.push_back(client_conn_);
    return client_conn_;
}

bool SteamNet::send(ConnId c, const void* data, std::size_t len, bool reliable) {
    if (!ok_ || c == 0) return false;
    const std::string dg = td::wrap(data, len, reliable);
    const int flags = reliable ? k_nSteamNetworkingSend_Reliable : k_nSteamNetworkingSend_Unreliable;
    const EResult r = SteamNetworkingSockets()->SendMessageToConnection(
        c, dg.data(), static_cast<std::uint32_t>(dg.size()), flags, nullptr);
    return r == k_EResultOK;
}

int SteamNet::broadcast_except(ConnId except, const void* data, std::size_t len, bool reliable) {
    int sent = 0;
    for (ConnId c : conns_)
        if (c != except && send(c, data, len, reliable)) ++sent;
    return sent;
}

void SteamNet::poll(std::vector<NetMessage>& out) {
    if (!ok_) return;
    g_self = this;
    SteamNetworkingSockets()->RunCallbacks();
    drain(out);
}

void SteamNet::drain(std::vector<NetMessage>& out) {
    ISteamNetworkingSockets* s = SteamNetworkingSockets();
    SteamNetworkingMessage_t* msgs[32];
    if (server_) {
        const int n = s->ReceiveMessagesOnPollGroup(poll_group_, msgs, 32);
        for (int i = 0; i < n; ++i) {
            push_msg(out, msgs[i]);
            msgs[i]->Release();
        }
    } else {
        for (ConnId c : conns_) {
            const int n = s->ReceiveMessagesOnConnection(c, msgs, 32);
            for (int i = 0; i < n; ++i) {
                push_msg(out, msgs[i]);
                msgs[i]->Release();
            }
        }
    }
}

void SteamNet::close(ConnId c) {
    if (!ok_ || c == 0) return;
    SteamNetworkingSockets()->CloseConnection(c, 0, "close", false);
    conns_.erase(std::remove(conns_.begin(), conns_.end(), c), conns_.end());
}

void SteamNet::on_conn_status(void* p) {
    auto* info = static_cast<SteamNetConnectionStatusChangedCallback_t*>(p);
    ISteamNetworkingSockets* s = SteamNetworkingSockets();
    switch (info->m_info.m_eState) {
        case k_ESteamNetworkingConnectionState_Connecting:
            if (server_) {
                if (s->AcceptConnection(info->m_hConn) == k_EResultOK) {
                    s->SetConnectionPollGroup(info->m_hConn, poll_group_);
                    conns_.push_back(info->m_hConn);
                } else {
                    s->CloseConnection(info->m_hConn, 0, "accept failed", false);
                }
            }
            break;
        case k_ESteamNetworkingConnectionState_Connected:
            if (!server_ && info->m_hConn == client_conn_) client_connected_ = true;
            break;
        case k_ESteamNetworkingConnectionState_ClosedByPeer:
        case k_ESteamNetworkingConnectionState_ProblemDetectedLocally:
            s->CloseConnection(info->m_hConn, 0, "peer closed", false);
            conns_.erase(std::remove(conns_.begin(), conns_.end(), info->m_hConn), conns_.end());
            if (info->m_hConn == client_conn_) {
                client_connected_ = false;
                client_conn_ = 0;
            }
            break;
        default:
            break;
    }
}

}  // namespace dcf
