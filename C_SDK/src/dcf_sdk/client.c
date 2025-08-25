#include "dcf_client.h"
#include "dcf_serialization.h"
#include <stdlib.h>
#include <string.h>
#include <uuid/uuid.h>

struct DCFClient {
    DCFConfig* config;
    DCFNetworking* networking;
    DCFRedundancy* redundancy;
    DCFPluginManager* plugin_mgr;
    bool running;
    int log_level;  // Default: 1 (info)
    DCFMode current_mode;  // For AUTO mode adjustments
};

DCFClient* dcf_client_new(void) {
    DCFClient* client = calloc(1, sizeof(DCFClient));
    if (!client) return NULL;
    client->log_level = 1;  // Default: info
    client->current_mode = AUTO_MODE;  // Default to AUTO
    return client;
}

DCFError dcf_client_initialize(DCFClient* client, const char* config_path) {
    if (!client || !config_path) return DCF_ERR_NULL_PTR;
    client->config = dcf_config_load(config_path);
    if (!client->config) return DCF_ERR_CONFIG_INVALID;
    client->networking = dcf_networking_new();
    if (!client->networking) return DCF_ERR_MALLOC_FAIL;
    DCFError err = dcf_networking_initialize(client->networking, client->config);
    if (err != DCF_SUCCESS) return err;
    client->plugin_mgr = dcf_plugin_manager_new();
    if (!client->plugin_mgr) return DCF_ERR_MALLOC_FAIL;
    err = dcf_plugin_manager_load(client->plugin_mgr, client->config);
    if (err != DCF_SUCCESS) return err;
    client->redundancy = dcf_redundancy_new();
    if (!client->redundancy) return DCF_ERR_MALLOC_FAIL;
    err = dcf_redundancy_initialize(client->redundancy, client->config, client->networking);
    if (err != DCF_SUCCESS) return err;
    // For AUTO mode, listen for master assignments
    if (client->config->mode == AUTO_MODE) {
        dcf_client_set_mode(client, AUTO_MODE);  // Set initial mode
    }
    return DCF_SUCCESS;
}

DCFError dcf_client_start(DCFClient* client) {
    if (!client) return DCF_ERR_NULL_PTR;
    if (client->running) return DCF_ERR_INVALID_STATE;
    client->running = true;
    DCFError err = dcf_networking_start(client->networking, client->current_mode);
    if (err != DCF_SUCCESS) return err;
    err = dcf_redundancy_start(client->redundancy, client->current_mode);
    if (err != DCF_SUCCESS) return err;
    return DCF_SUCCESS;
}

DCFError dcf_client_stop(DCFClient* client) {
    if (!client) return DCF_ERR_NULL_PTR;
    if (!client->running) return DCF_ERR_INVALID_STATE;
    client->running = false;
    DCFError err = dcf_networking_stop(client->networking);
    if (err != DCF_SUCCESS) return err;
    err = dcf_redundancy_stop(client->redundancy);
    if (err != DCF_SUCCESS) return err;
    return DCF_SUCCESS;
}

DCFError dcf_client_send_message(DCFClient* client, const char* data, const char* recipient, char** response_out) {
    if (!client || !data || !recipient || !response_out) return DCF_ERR_NULL_PTR;
    if (!client->running) return DCF_ERR_INVALID_STATE;
    char* node_id;
    DCFError err = dcf_config_get_node_id(client->config, &node_id);
    if (err != DCF_SUCCESS) return err;
    uint8_t* serialized;
    size_t serialized_len;
    err = dcf_serialize_message(data, node_id, recipient, &serialized, &serialized_len);
    free(node_id);
    if (err != DCF_SUCCESS) return err;
    char* target = (char*)recipient;
    if (client->current_mode == P2P_MODE || client->current_mode == AUTO_MODE) {
        err = dcf_redundancy_get_optimal_route(client->redundancy, recipient, &target);
        if (err != DCF_SUCCESS) { free(serialized); return err; }
    }
    ITransport* transport = dcf_plugin_manager_get_transport(client->plugin_mgr);
    if (transport) {
        if (!transport->send(transport, serialized, serialized_len, target)) {
            free(serialized);
            if (target != recipient) free(target);
            return DCF_ERR_NETWORK_FAIL;
        }
        size_t response_len;
        uint8_t* response_data = transport->receive(transport, &response_len);
        if (response_data) {
            char* sender;
            err = dcf_deserialize_message(response_data, response_len, response_out, &sender);
            free(response_data);
            free(sender);
        } else {
            err = DCF_ERR_NETWORK_FAIL;
        }
    } else {
        err = dcf_networking_send(client->networking, serialized, serialized_len, target);
        if (err == DCF_SUCCESS) {
            err = dcf_networking_receive(client->networking, response_out, NULL);
        }
    }
    free(serialized);
    if (target != recipient) free(target);
    return err;
}

DCFError dcf_client_receive_message(DCFClient* client, char** message_out, char** sender_out) {
    if (!client || !message_out || !sender_out) return DCF_ERR_NULL_PTR;
    if (!client->running) return DCF_ERR_INVALID_STATE;
    ITransport* transport = dcf_plugin_manager_get_transport(client->plugin_mgr);
    if (transport) {
        size_t response_len;
        uint8_t* response_data = transport->receive(transport, &response_len);
        if (!response_data) return DCF_ERR_NETWORK_FAIL;
        DCFError err = dcf_deserialize_message(response_data, response_len, message_out, sender_out);
        free(response_data);
        return err;
    }
    return dcf_networking_receive(client->networking, message_out, sender_out);
}

DCFError dcf_client_set_mode(DCFClient* client, DCFMode mode) {
    if (!client) return DCF_ERR_NULL_PTR;
    client->current_mode = mode;
    // Reconfigure networking and redundancy for new mode (placeholder)
    return DCF_SUCCESS;
}

DCFError dcf_client_set_log_level(DCFClient* client, int level) {
    if (!client) return DCF_ERR_NULL_PTR;
    client->log_level = level;
    // Apply log level (placeholder)
    return DCF_SUCCESS;
}

void dcf_client_free(DCFClient* client) {
    if (!client) return;
    dcf_config_free(client->config);
    dcf_networking_free(client->networking);
    dcf_redundancy_free(client->redundancy);
    dcf_plugin_manager_free(client->plugin_mgr);
    free(client);
}
