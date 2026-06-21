// SPDX-License-Identifier: LGPL-3.0-only
#ifndef DCF_STEAM_EXT_HPP
#define DCF_STEAM_EXT_HPP

// Steamworks-only extensions (dedicated game server + matchmaking lobbies). These
// are compiled ONLY when DCF_CPP_STEAM is defined and a developer-supplied
// Steamworks SDK is on the include/link path (Valve's SDK is proprietary and not
// redistributable — see Documentation/DCF_STEAM_SPEC.md). The hermetic GNS build
// never includes this. The networking transport itself lives in net_steam.cpp; the
// SAME ISteamNetworkingSockets code links against either backend.
//
// Mapping: a Steam lobby ↔ a DCF dst channel; a SteamID ↔ a per-player identity.

#if defined(DCF_CPP_STEAM)

#include <cstdint>

namespace dcf {

// Dedicated game server: anonymous (or GSLT, via $DCF_STEAM_GSLT) login + master-
// server heartbeats so the node appears in the Steam server browser. Call before
// listen(); pump steam_server_run_callbacks() each tick. Needs steam_appid.txt.
bool steam_server_register(const char* node_id, std::uint16_t game_port);
void steam_server_run_callbacks();
void steam_server_shutdown();

// Matchmaking lobby helpers (results arrive via Steam callbacks; pump them).
std::uint64_t steam_lobby_create(int max_members);
bool steam_lobby_join(std::uint64_t lobby_id);
void steam_lobby_set_data(std::uint64_t lobby_id, const char* key, const char* val);

}  // namespace dcf

#endif  // DCF_CPP_STEAM
#endif  // DCF_STEAM_EXT_HPP
