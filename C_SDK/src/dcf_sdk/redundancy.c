#include "dcf_redundancy.h"
#include "dcf_serialization.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <limits.h>

struct DCFRedundancy {
    char** peers;
    size_t peer_count;
    int* rtt_cache;
    char** groups;
    int rtt_threshold;
    DCFNetworking* networking;
    bool running;
    DCFMode mode;
};

DCFRedundancy* dcf_redundancy_new(void) {
    DCFRedundancy* redundancy = calloc(1, sizeof(DCFRedundancy));
    if (!redundancy) return NULL;
    return redundancy;
}

DCFError dcf_redundancy_initialize(DCFRedundancy* redundancy, DCFConfig* config, DCFNetworking* networking) {
    if (!redundancy || !config || !networking) return DCF_ERR_NULL_PTR;
    redundancy->networking = networking;
    DCFError err = dcf_config_get_peers(config, &redundancy->peers, &redundancy->peer_count);
    if (err != DCF_SUCCESS) return err;
    redundancy->rtt_cache = calloc(redundancy->peer_count, sizeof(int));
    if (!redundancy->rtt_cache) return DCF_ERR_MALLOC_FAIL;
    redundancy->groups = calloc(redundancy->peer_count, sizeof(char*));
    if (!redundancy->groups) { free(redundancy->rtt_cache); return DCF_ERR_MALLOC_FAIL; }
    redundancy->rtt_threshold = dcf_config_get_rtt_threshold(config);
    dcf_redundancy_group_peers(redundancy);
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_start(DCFRedundancy* redundancy, DCFMode mode) {
    if (!redundancy) return DCF_ERR_NULL_PTR;
    redundancy->running = true;
    redundancy->mode = mode;
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_stop(DCFRedundancy* redundancy) {
    if (!redundancy) return DCF_ERR_NULL_PTR;
    redundancy->running = false;
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_get_optimal_route(DCFRedundancy* redundancy, const char* recipient, char** route_out) {
    if (!redundancy || !recipient || !route_out) return DCF_ERR_NULL_PTR;
    if (!redundancy->running) return DCF_ERR_INVALID_STATE;
    int min_rtt = INT_MAX;
    size_t min_idx = 0;
    bool found = false;
    for (size_t i = 0; i < redundancy->peer_count; i++) {
        if (strcmp(redundancy->peers[i], recipient) == 0) continue;
        if (redundancy->rtt_cache[i] < min_rtt) {
            min_rtt = redundancy->rtt_cache[i];
            min_idx = i;
            found = true;
        }
    }
    if (!found) return DCF_ERR_ROUTE_NOT_FOUND;
    *route_out = strdup(redundancy->peers[min_idx]);
    if (!*route_out) return DCF_ERR_MALLOC_FAIL;
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_health_check(DCFRedundancy* redundancy, const char* peer, int* rtt_out) {
    if (!redundancy || !peer || !rtt_out) return DCF_ERR_NULL_PTR;
    if (!redundancy->running) return DCF_ERR_INVALID_STATE;
    uint8_t* health_request;
    size_t req_len;
    DCFError err = dcf_serialize_health_request(peer, &health_request, &req_len);
    if (err != DCF_SUCCESS) return err;
    err = dcf_networking_send(redundancy->networking, health_request, req_len, peer);
    free(health_request);
    if (err != DCF_SUCCESS) return err;
    char* response, *sender;
    err = dcf_networking_receive(redundancy->networking, &response, &sender);
    if (err != DCF_SUCCESS) return err;
    *rtt_out = rand() % 100;  // Mock RTT; replace with real measurement
    free(response);
    free(sender);
    for (size_t i = 0; i < redundancy->peer_count; i++) {
        if (strcmp(redundancy->peers[i], peer) == 0) {
            redundancy->rtt_cache[i] = *rtt_out;
            free(redundancy->groups[i]);
            redundancy->groups[i] = strdup(*rtt_out < redundancy->rtt_threshold ? "local" : "remote");
            break;
        }
    }
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_simulate_failure(DCFRedundancy* redundancy, const char* peer) {
    if (!redundancy || !peer) return DCF_ERR_NULL_PTR;
    if (!redundancy->running) return DCF_ERR_INVALID_STATE;
    for (size_t i = 0; i < redundancy->peer_count; i++) {
        if (strcmp(redundancy->peers[i], peer) == 0) {
            redundancy->rtt_cache[i] = INT_MAX;
            free(redundancy->groups[i]);
            redundancy->groups[i] = strdup("unreachable");
            return DCF_SUCCESS;
        }
    }
    return DCF_ERR_UNKNOWN;
}

DCFError dcf_redundancy_group_peers(DCFRedundancy* redundancy) {
    if (!redundancy) return DCF_ERR_NULL_PTR;
    if (!redundancy->running) return DCF_ERR_INVALID_STATE;
    for (size_t i = 0; i < redundancy->peer_count; i++) {
        int rtt;
        DCFError err = dcf_redundancy_health_check(redundancy, redundancy->peers[i], &rtt);
        if (err != DCF_SUCCESS) continue;
    }
    return DCF_SUCCESS;
}

void dcf_redundancy_free(DCFRedundancy* redundancy) {
    if (!redundancy) return;
    for (size_t i = 0; i < redundancy->peer_count; i++) {
        free(redundancy->peers[i]);
        free(redundancy->groups[i]);
    }
    free(redundancy->peers);
    free(redundancy->rtt_cache);
    free(redundancy->groups);
    free(redundancy);
}
    free(redundancy);
}
