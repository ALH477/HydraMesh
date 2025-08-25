#include "dcf_interface.h"
#include <cjson/cJSON.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ncurses.h>
#include <time.h>

static const char* DCF_VERSION = "5.0.0";

DCFError dcf_interface_execute(DCFClient* client, DCFCmd cmd, const char** args, int arg_count, bool json_output, char** output) {
    if (!client || !output) return DCF_ERR_NULL_PTR;
    *output = NULL;
    cJSON* json = json_output ? cJSON_CreateObject() : NULL;
    char* result = calloc(1, 4096);  // Large buffer for output
    if (!result) {
        if (json) cJSON_Delete(json);
        return DCF_ERR_MALLOC_FAIL;
    }
    DCFError err = DCF_SUCCESS;
    switch (cmd) {
        case DCF_CMD_INIT:
            if (arg_count < 1) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            err = dcf_client_initialize(client, args[0]);
            if (err == DCF_SUCCESS) {
                strcpy(result, "Initialized successfully");
                if (json) cJSON_AddStringToObject(json, "status", "initialized");
            }
            break;
        case DCF_CMD_START:
            err = dcf_client_start(client);
            if (err == DCF_SUCCESS) {
                strcpy(result, "Started DCF instance");
                if (json) cJSON_AddStringToObject(json, "status", "started");
            }
            break;
        case DCF_CMD_STOP:
            err = dcf_client_stop(client);
            if (err == DCF_SUCCESS) {
                strcpy(result, "Stopped DCF instance");
                if (json) cJSON_AddStringToObject(json, "status", "stopped");
            }
            break;
        case DCF_CMD_STATUS: {
            char** peers;
            size_t peer_count;
            if (dcf_config_get_peers(client->config, &peers, &peer_count) == DCF_SUCCESS) {
                char* mode_str;
                dcf_config_get_mode(client->config, &client->config->mode);
                snprintf(result, 4096, "Running: %s\nMode: %s\nPeers: %zu", client->running ? "Yes" : "No",
                         client->config->mode == CLIENT_MODE ? "client" : client->config->mode == SERVER_MODE ? "server" : client->config->mode == P2P_MODE ? "p2p" : "auto", peer_count);
                if (json) {
                    cJSON_AddBoolToObject(json, "running", client->running);
                    cJSON_AddStringToObject(json, "mode", client->config->mode == CLIENT_MODE ? "client" : client->config->mode == SERVER_MODE ? "server" : client->config->mode == P2P_MODE ? "p2p" : "auto");
                    cJSON_AddNumberToObject(json, "peer_count", peer_count);
                }
                for (size_t i = 0; i < peer_count; i++) free(peers[i]);
                free(peers);
            } else {
                err = DCF_ERR_CONFIG_INVALID;
            }
            break;
        }
        case DCF_CMD_SEND:
            if (arg_count < 2) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            err = dcf_client_send_message(client, args[0], args[1], &result);
            if (err == DCF_SUCCESS) {
                snprintf(result, 4096, "Sent message: %s to %s", args[0], args[1]);
                if (json) {
                    cJSON_AddStringToObject(json, "message", args[0]);
                    cJSON_AddStringToObject(json, "recipient", args[1]);
                }
            }
            break;
        case DCF_CMD_RECEIVE: {
            char* message, *sender;
            err = dcf_client_receive_message(client, &message, &sender);
            if (err == DCF_SUCCESS) {
                snprintf(result, 4096, "Received from %s: %s", sender, message);
                if (json) {
                    cJSON_AddStringToObject(json, "message", message);
                    cJSON_AddStringToObject(json, "sender", sender);
                }
                free(message);
                free(sender);
            }
            break;
        }
        case DCF_CMD_HEALTH_CHECK:
            if (arg_count < 1) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            int rtt;
            err = dcf_redundancy_health_check(client->redundancy, args[0], &rtt);
            if (err == DCF_SUCCESS) {
                snprintf(result, 4096, "Peer %s RTT: %d ms", args[0], rtt);
                if (json) {
                    cJSON_AddStringToObject(json, "peer", args[0]);
                    cJSON_AddNumberToObject(json, "rtt", rtt);
                }
            }
            break;
        case DCF_CMD_LIST_PEERS: {
            char** peers;
            size_t peer_count;
            if (dcf_config_get_peers(client->config, &peers, &peer_count) == DCF_SUCCESS) {
                char* ptr = result;
                ptr += snprintf(ptr, 4096, "Peers (%zu):\n", peer_count);
                if (json) cJSON_AddArrayToObject(json, "peers");
                for (size_t i = 0; i < peer_count; i++) {
                    int rtt;
                    dcf_redundancy_health_check(client->redundancy, peers[i], &rtt);
                    ptr += snprintf(ptr, 4096 - (ptr - result), "%s (RTT: %d ms, Group: %s)\n", peers[i], rtt, client->redundancy->groups[i]);
                    if (json) {
                        cJSON* peer = cJSON_CreateObject();
                        cJSON_AddStringToObject(peer, "address", peers[i]);
                        cJSON_AddNumberToObject(peer, "rtt", rtt);
                        cJSON_AddStringToObject(peer, "group", client->redundancy->groups[i]);
                        cJSON_AddItemToArray(cJSON_GetObjectItem(json, "peers"), peer);
                    }
                    free(peers[i]);
                }
                free(peers);
            } else {
                err = DCF_ERR_CONFIG_INVALID;
            }
            break;
        }
        case DCF_CMD_HEAL:
            if (arg_count < 1) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            err = dcf_redundancy_simulate_failure(client->redundancy, args[0]);
            if (err == DCF_SUCCESS) {
                err = dcf_redundancy_group_peers(client->redundancy);
                if (err == DCF_SUCCESS) {
                    strcpy(result, "Healed network for peer");
                    if (json) cJSON_AddStringToObject(json, "status", "healed");
                }
            }
            break;
        case DCF_CMD_VERSION:
            snprintf(result, 4096, "DCF Version: %s (C SDK)", DCF_VERSION);
            if (json) cJSON_AddStringToObject(json, "version", DCF_VERSION);
            break;
        case DCF_CMD_BENCHMARK:
            if (arg_count < 1) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            clock_t start = clock();
            int rtt;
            err = dcf_redundancy_health_check(client->redundancy, args[0], &rtt);
            clock_t end = clock();
            if (err == DCF_SUCCESS) {
                double ms = ((double)(end - start) * 1000) / CLOCKS_PER_SEC;
                snprintf(result, 4096, "Benchmark RTT to %s: %d ms, Execution: %.2f ms", args[0], rtt, ms);
                if (json) {
                    cJSON_AddStringToObject(json, "peer", args[0]);
                    cJSON_AddNumberToObject(json, "rtt", rtt);
                    cJSON_AddNumberToObject(json, "execution_ms", ms);
                }
            }
            break;
        case DCF_CMD_GROUP_PEERS:
            err = dcf_redundancy_group_peers(client->redundancy);
            if (err == DCF_SUCCESS) {
                strcpy(result, "Regrouped peers");
                if (json) cJSON_AddStringToObject(json, "status", "regrouped");
            }
            break;
        case DCF_CMD_SIMULATE_FAILURE:
            if (arg_count < 1) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            err = dcf_redundancy_simulate_failure(client->redundancy, args[0]);
            if (err == DCF_SUCCESS) {
                snprintf(result, 4096, "Simulated failure for %s", args[0]);
                if (json) cJSON_AddStringToObject(json, "peer", args[0]);
            }
            break;
        case DCF_CMD_LOG_LEVEL:
            if (arg_count < 1) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            int level = atoi(args[0]);
            err = dcf_client_set_log_level(client, level);
            if (err == DCF_SUCCESS) {
                snprintf(result, 4096, "Log level set to %d", level);
                if (json) cJSON_AddNumberToObject(json, "log_level", level);
            }
            break;
        case DCF_CMD_LOAD_PLUGIN:
            if (arg_count < 1) {
                err = DCF_ERR_INVALID_ARG;
                break;
            }
            err = dcf_plugin_manager_load(client->plugin_mgr, client->config);
            if (err == DCF_SUCCESS) {
                strcpy(result, "Plugin loaded successfully");
                if (json) cJSON_AddStringToObject(json, "status", "plugin_loaded");
            }
            break;
        case DCF_CMD_TUI:
            err = dcf_interface_tui_start(client);
            if (err == DCF_SUCCESS) {
                strcpy(result, "TUI started");
                if (json) cJSON_AddStringToObject(json, "status", "tui_started");
            }
            break;
        default:
            err = DCF_ERR_UNKNOWN;
            break;
    }
    if (err != DCF_SUCCESS) {
        snprintf(result, 4096, "Error: %s", dcf_error_str(err));
        if (json) cJSON_AddStringToObject(json, "error", dcf_error_str(err));
    }
    if (json) {
        free(result);
        *output = cJSON_PrintUnformatted(json);
        cJSON_Delete(json);
    } else {
        *output = result;
    }
    return err;
}

DCFError dcf_interface_tui_start(DCFClient* client) {
    if (!client) return DCF_ERR_NULL_PTR;
    initscr();
    raw();
    keypad(stdscr, TRUE);
    noecho();
    printw("DCF Text User Interface (Version %s)\n", DCF_VERSION);
    printw("Commands: init, start, stop, status, send, receive, health-check, list-peers, heal, version, benchmark, group-peers, simulate-failure, log-level, load-plugin, quit\n");
    printw("Enter command (or 'quit' to exit): ");
    refresh();
    char input[256];
    char* output;
    while (1) {
        getstr(input);
        if (strcmp(input, "quit") == 0) break;
        DCFCmd cmd = DCF_CMD_UNKNOWN;
        // Parse input to cmd (simplified; parse full line in production)
        if (strcmp(input, "init") == 0) cmd = DCF_CMD_INIT;
        // ... (add all commands as in dcf_cli.c)
        if (cmd == DCF_CMD_UNKNOWN) {
            printw("Unknown command: %s\n", input);
        } else {
            char* args[2] = {"config.json", "peer1"};  // Simplified for TUI
            DCFError err = dcf_interface_execute(client, cmd, (const char**)args, cmd == DCF_CMD_SEND ? 2 : 1, false, &output);
            printw("%s\n", output);
            free(output);
        }
        printw("Enter command: ");
        refresh();
    }
    endwin();
    return DCF_SUCCESS;
}
