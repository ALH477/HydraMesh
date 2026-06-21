// SPDX-License-Identifier: LGPL-3.0-only
#ifndef DCF_TRANSPORT_HPP
#define DCF_TRANSPORT_HPP

// A minimal transport seam for the DCF C++ node. The wire (17-byte DeModFrame and
// its adapters) is unchanged; a transport just carries those bytes between peers.
// One implementation, SteamNet (net_steam.hpp), targets Valve's
// ISteamNetworkingSockets API — built against the open GameNetworkingSockets
// (DCF_CPP_GNS) or, opt-in, the proprietary Steamworks SDK (DCF_CPP_STEAM) for SDR
// relay / lobbies. See Documentation/DCF_STEAM_SPEC.md.

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace dcf {

// A connection handle (HSteamNetConnection under the hood; 0 == invalid).
using ConnId = std::uint32_t;

// A payload received from a peer, with the reliability it was sent with.
struct NetMessage {
    ConnId from = 0;
    std::string data;        // DCF bytes (frame / superpack / adapter), prefix stripped
    bool reliable = false;
};

// ── DCF-Steam transport datagram ─────────────────────────────────────────────
// GNS does not expose, on receive, whether a message arrived reliably — so a
// forwarding hub could not preserve the sender's intent. We prepend one flags
// byte: [flags u8][payload...], flags bit0 = RELIABLE. This is a transport-internal
// envelope (documented in DCF_STEAM_SPEC.md); the payload is the unmodified DCF wire.
namespace td {
constexpr std::uint8_t FLAG_RELIABLE = 0x01;

inline std::string wrap(const void* p, std::size_t n, bool reliable) {
    std::string s;
    s.reserve(n + 1);
    s.push_back(reliable ? static_cast<char>(FLAG_RELIABLE) : 0);
    s.append(reinterpret_cast<const char*>(p), n);
    return s;
}
}  // namespace td

class ITransport {
public:
    virtual ~ITransport() = default;

    // Dedicated server / hub: listen for inbound connections on a port (IP port for
    // GNS; SDR virtual port for Steam).
    virtual bool listen(std::uint16_t port) = 0;

    // Client: connect to host:port (GNS) or peer identity@virtual-port (Steam).
    virtual ConnId connect(const std::string& host, std::uint16_t port) = 0;

    virtual bool send(ConnId c, const void* data, std::size_t len, bool reliable) = 0;

    // Hub fan-out: send to every connection except `except`. Returns count sent.
    virtual int broadcast_except(ConnId except, const void* data, std::size_t len, bool reliable) = 0;

    // Pump transport callbacks and drain any received messages into `out`.
    virtual void poll(std::vector<NetMessage>& out) = 0;

    virtual std::vector<ConnId> connections() const = 0;
    virtual void close(ConnId c) = 0;
};

}  // namespace dcf

#endif  // DCF_TRANSPORT_HPP
