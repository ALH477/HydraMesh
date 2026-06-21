// SPDX-License-Identifier: LGPL-3.0-only
//
// Steamworks dedicated game server — anonymous/GSLT login + master-server heartbeats
// so a dockerized DCF hub appears in the Steam server browser. Compiled only with
// DCF_CPP_STEAM against the developer-supplied Steamworks SDK; UNVERIFIED in CI (the
// SDK is proprietary). See Documentation/DCF_STEAM_SPEC.md.

#if defined(DCF_CPP_STEAM)

#include "dcf/steam_ext.hpp"

#include <steam/steam_gameserver.h>

#include <cstdio>
#include <cstdlib>

namespace dcf {

namespace {
bool g_gs_up = false;
}

bool steam_server_register(const char* node_id, std::uint16_t game_port) {
    // App ID is taken from steam_appid.txt (dev) or the depot at runtime. The query
    // port carries A2S/server-browser traffic; keep it adjacent to the game port.
    const std::uint16_t query_port = static_cast<std::uint16_t>(game_port + 1);
    if (!SteamGameServer_Init(0 /*INADDR_ANY*/, game_port, query_port,
                              eServerModeAuthentication, "0.3.0.0")) {
        std::fprintf(stderr, "[steam] SteamGameServer_Init failed "
                             "(missing steam_appid.txt / App ID / steamclient.so?)\n");
        return false;
    }
    ISteamGameServer* gs = SteamGameServer();
    gs->SetProduct("dcf");
    gs->SetGameDescription("DeMoD Communication Framework — mesh/game node");
    gs->SetModDir("dcf");
    gs->SetDedicatedServer(true);
    gs->SetServerName(node_id && *node_id ? node_id : "dcf-hub");
    gs->SetMaxPlayerCount(64);

    const char* gslt = std::getenv("DCF_STEAM_GSLT");
    if (gslt && *gslt) gs->LogOn(gslt);   // persistent, listed token
    else gs->LogOnAnonymous();            // anonymous account (fine for Docker)

    // Heartbeats -> Steam master server -> server browser. (Older SDKs:
    // EnableHeartbeats(true).)
    gs->SetAdvertiseServerActive(true);

    g_gs_up = true;
    std::printf("[steam] dedicated server registered (game=%u query=%u)\n", game_port, query_port);
    return true;
}

void steam_server_run_callbacks() {
    if (g_gs_up) SteamGameServer_RunCallbacks();
}

void steam_server_shutdown() {
    if (!g_gs_up) return;
    if (ISteamGameServer* gs = SteamGameServer()) {
        gs->SetAdvertiseServerActive(false);
        gs->LogOff();
    }
    SteamGameServer_Shutdown();
    g_gs_up = false;
}

}  // namespace dcf

#endif  // DCF_CPP_STEAM
