// SPDX-License-Identifier: LGPL-3.0-only
//
// Steamworks matchmaking lobbies — the discovery/coordination layer that maps onto a
// DCF dst channel (lobby ↔ channel; SteamID ↔ player identity). Compiled only with
// DCF_CPP_STEAM against the developer-supplied Steamworks SDK; UNVERIFIED in CI.
// Lobby operations complete via Steam callbacks — pump SteamAPI_RunCallbacks().
// See Documentation/DCF_STEAM_SPEC.md.

#if defined(DCF_CPP_STEAM)

#include "dcf/steam_ext.hpp"

#include <steam/steam_api.h>

#include <cstdio>

namespace dcf {

namespace {

// Tracks the in-flight CreateLobby/JoinLobby calls (async → CCallResult).
struct LobbyOps {
    CSteamID lobby;
    bool ready = false;
    CCallResult<LobbyOps, LobbyCreated_t> on_create;
    CCallResult<LobbyOps, LobbyEnter_t> on_enter;

    void create(int max_members) {
        SteamAPICall_t h = SteamMatchmaking()->CreateLobby(k_ELobbyTypePublic, max_members);
        on_create.Set(h, this, &LobbyOps::Created);
    }
    void Created(LobbyCreated_t* r, bool io_fail) {
        if (io_fail || r->m_eResult != k_EResultOK) {
            std::fprintf(stderr, "[steam] CreateLobby failed (%d)\n", r ? r->m_eResult : -1);
            return;
        }
        lobby = CSteamID(r->m_ulSteamIDLobby);
        ready = true;
        std::printf("[steam] lobby created: %llu\n", (unsigned long long)lobby.ConvertToUint64());
    }

    void join(std::uint64_t id) {
        SteamAPICall_t h = SteamMatchmaking()->JoinLobby(CSteamID(id));
        on_enter.Set(h, this, &LobbyOps::Entered);
    }
    void Entered(LobbyEnter_t* r, bool io_fail) {
        if (io_fail) {
            std::fprintf(stderr, "[steam] JoinLobby failed\n");
            return;
        }
        lobby = CSteamID(r->m_ulSteamIDLobby);
        ready = true;
        std::printf("[steam] lobby joined: %llu (members=%d)\n",
                    (unsigned long long)lobby.ConvertToUint64(),
                    SteamMatchmaking()->GetNumLobbyMembers(lobby));
    }
};

LobbyOps g_lobby;

}  // namespace

std::uint64_t steam_lobby_create(int max_members) {
    g_lobby.create(max_members);
    return g_lobby.lobby.ConvertToUint64();  // 0 until the LobbyCreated_t callback fires
}

bool steam_lobby_join(std::uint64_t lobby_id) {
    g_lobby.join(lobby_id);
    return true;
}

void steam_lobby_set_data(std::uint64_t lobby_id, const char* key, const char* val) {
    SteamMatchmaking()->SetLobbyData(CSteamID(lobby_id), key, val);
}

}  // namespace dcf

#endif  // DCF_CPP_STEAM
