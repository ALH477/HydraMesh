#include "dcf_redundancy.h"
#include "dcf_config.h"
#include "dcf_networking.h"
#include <stdio.h>
#include <string.h>

int main() {
    DCFConfig* config = dcf_config_load("config.json");
    if (!config) {
        printf("Config load failed\n");
        return 1;
    }
    DCFNetworking* net = dcf_networking_new();
    if (!net || dcf_networking_initialize(net, config) != DCF_SUCCESS) {
        printf("Networking init failed\n");
        dcf_config_free(config);
        return 1;
    }
    DCFRedundancy* redundancy = dcf_redundancy_new();
    if (!redundancy || dcf_redundancy_initialize(redundancy, config, net) != DCF_SUCCESS) {
        printf("Redundancy init failed\n");
        dcf_networking_free(net);
        dcf_config_free(config);
        return 1;
    }
    if (dcf_redundancy_start(redundancy) != DCF_SUCCESS) {
        printf("Redundancy start failed\n");
        dcf_redundancy_free(redundancy);
        dcf_networking_free(net);
        dcf_config_free(config);
        return 1;
    }
    char* peers;
    size_t peer_count;
    if (dcf_config_get_peers(config, &peers, &peer_count) != DCF_SUCCESS || peer_count == 0) {
        printf("No peers found\n");
        dcf_redundancy_stop(redundancy);
        dcf_redundancy_free(redundancy);
        dcf_networking_free(net);
        dcf_config_free(config);
        return 1;
    }
    int rtt;
    DCFError err = dcf_redundancy_health_check(redundancy, peers[0], &rtt);
    if (err != DCF_SUCCESS) {
        printf("Health check failed: %s\n", dcf_error_str(err));
        for (size_t i = 0; i < peer_count; i++) free(peers[i]);
        free(peers);
        dcf_redundancy_stop(redundancy);
        dcf_redundancy_free(redundancy);
        dcf_networking_free(net);
        dcf_config_free(config);
        return 1;
    }
    printf("RTT to %s: %d ms\n", peers[0], rtt);
    char* route;
    err = dcf_redundancy_get_optimal_route(redundancy, peers[0], &route);
    if (err == DCF_SUCCESS) {
        printf("Optimal route: %s\n", route);
        free(route);
    }
    for (size_t i = 0; i < peer_count; i++) free(peers[i]);
    free(peers);
    dcf_redundancy_stop(redundancy);
    dcf_redundancy_free(redundancy);
    dcf_networking_free(net);
    dcf_config_free(config);
    printf("Redundancy tests passed\n");
    return 0;
}
