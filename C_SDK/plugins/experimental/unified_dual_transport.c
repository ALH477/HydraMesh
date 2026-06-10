#include <dcf_sdk/dcf_plugin_manager.h>
#include <dcf_sdk/dcf_config.h>
#include <dcf_sdk/dcf_error.h>
#include <dcf_sdk/dcf_logging.h>
#include <cjson/cJSON.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>  // For usleep

// Plugin private data
typedef struct {
    DCFConfig* config;                // Primary DCF config
    char* secondary_config_path;      // Path to secondary config (from config_unifier)
    cJSON* secondary_config;          // Parsed secondary config
    void* primary_transport;          // Primary transport instance (e.g., gRPC)
    void* secondary_transport;        // Secondary transport instance (e.g., WebSocket)
    ITransport* primary_iface;        // Primary transport interface
    ITransport* secondary_iface;      // Secondary transport interface
    pthread_t listener_thread;        // Thread for async receiving
    int running;                     // Flag to control listener thread
} UnifiedDualTransport;

// Forward declaration
void* listener_thread_func(void* arg);
DCFError merge_configs(UnifiedDualTransport* self, DCFConfig* primary);

// Initialize transport from config
static DCFError init_transport(const char* transport_name, const char* host, int port, void** transport_out, ITransport** iface_out) {
    *transport_out = dcf_networking_create_transport(transport_name, host, port);
    if (!*transport_out) {
        dcf_log_error("Failed to create transport: %s", transport_name);
        return DCF_ERR_TRANSPORT_INIT;
    }
    *iface_out = dcf_networking_get_transport_iface(*transport_out);
    if (!*iface_out) {
        dcf_log_error("Failed to get transport interface: %s", transport_name);
        return DCF_ERR_TRANSPORT_INIT;
    }
    return DCF_SUCCESS;
}

// Setup: Merge config first, then init transports
DCFError setup(void* self, const char* host, int port) {
    UnifiedDualTransport* data = (UnifiedDualTransport*)self;
    if (!data->config || !data->config->json_config) {
        dcf_log_error("NULL config in setup");
        return DCF_ERR_NULL_PTR;
    }

    // Step 1: Load and merge secondary config (from config_unifier logic)
    if (data->secondary_config_path) {
        FILE* fp = fopen(data->secondary_config_path, "r");
        if (!fp) {
            dcf_log_error("Failed to open secondary config: %s", data->secondary_config_path);
            return DCF_ERR_FILE_IO;
        }
        fseek(fp, 0, SEEK_END);
        long size = ftell(fp);
        if (size <= 0) {
            fclose(fp);
            dcf_log_error("Invalid file size for secondary config");
            return DCF_ERR_FILE_IO;
        }
        fseek(fp, 0, SEEK_SET);
        char* buffer = calloc(size + 1, 1);
        if (!buffer) {
            fclose(fp);
            dcf_log_error("Memory allocation failed for buffer");
            return DCF_ERR_MEMORY_ALLOC;
        }
        if (fread(buffer, 1, size, fp) != (size_t)size) {
            free(buffer);
            fclose(fp);
            dcf_log_error("Failed to read secondary config");
            return DCF_ERR_FILE_IO;
        }
        fclose(fp);

        data->secondary_config = cJSON_Parse(buffer);
        free(buffer);
        if (!data->secondary_config) {
            dcf_log_error("JSON parse failed for secondary config");
            return DCF_ERR_JSON_PARSE;
        }

        DCFError merge_err = merge_configs(data, data->config);
        if (merge_err != DCF_SUCCESS) {
            dcf_log_error("Config merge failed: %s", dcf_error_str(merge_err));
            cJSON_Delete(data->secondary_config);
            data->secondary_config = NULL;
            return merge_err;
        }
    }

    // Step 2: Parse and init dual transports
    cJSON* plugins = cJSON_GetObjectItem(data->config->json_config, "plugins");
    if (!plugins) {
        dcf_log_error("No plugins config found");
        return DCF_ERR_CONFIG_MISSING;
    }

    cJSON* primary_transport = cJSON_GetObjectItem(plugins, "primary_transport");
    cJSON* primary_host = cJSON_GetObjectItem(plugins, "primary_host");
    cJSON* primary_port = cJSON_GetObjectItem(plugins, "primary_port");
    if (!primary_transport || !cJSON_IsString(primary_transport) ||
        !primary_host || !cJSON_IsString(primary_host) ||
        !primary_port || !cJSON_IsNumber(primary_port)) {
        dcf_log_error("Invalid primary transport config");
        return DCF_ERR_CONFIG_INVALID;
    }

    DCFError err = init_transport(primary_transport->valuestring, primary_host->valuestring,
                                  (int)primary_port->valuedouble, &data->primary_transport,
                                  &data->primary_iface);
    if (err != DCF_SUCCESS) return err;

    cJSON* secondary_transport = cJSON_GetObjectItem(plugins, "secondary_transport");
    cJSON* secondary_host = cJSON_GetObjectItem(plugins, "secondary_host");
    cJSON* secondary_port = cJSON_GetObjectItem(plugins, "secondary_port");
    if (!secondary_transport || !cJSON_IsString(secondary_transport) ||
        !secondary_host || !cJSON_IsString(secondary_host) ||
        !secondary_port || !cJSON_IsNumber(secondary_port)) {
        dcf_log_error("Invalid secondary transport config");
        data->primary_iface->destroy(data->primary_transport);
        return DCF_ERR_CONFIG_INVALID;
    }

    err = init_transport(secondary_transport->valuestring, secondary_host->valuestring,
                         (int)secondary_port->valuedouble, &data->secondary_transport,
                         &data->secondary_iface);
    if (err != DCF_SUCCESS) {
        dcf_log_error("Secondary transport init failed: %s", dcf_error_str(err));
        data->primary_iface->destroy(data->primary_transport);
        return err;
    }

    // Step 3: Start listener thread
    data->running = 1;
    int thread_err = pthread_create(&data->listener_thread, NULL, listener_thread_func, data);
    if (thread_err != 0) {
        dcf_log_error("Failed to create listener thread: %d", thread_err);
        data->running = 0;
        data->primary_iface->destroy(data->primary_transport);
        data->secondary_iface->destroy(data->secondary_transport);
        return DCF_ERR_THREAD_INIT;
    }

    return DCF_SUCCESS;
}

// Merge configs (from config_unifier)
static DCFError merge_configs(UnifiedDualTransport* self, DCFConfig* primary) {
    if (!self->secondary_config || !primary) {
        dcf_log_error("NULL args in merge_configs");
        return DCF_ERR_NULL_PTR;
    }

    cJSON* sec = self->secondary_config;
    
    cJSON* transport = cJSON_GetObjectItem(sec, "transport");
    if (transport && cJSON_IsString(transport)) {
        if (strlen(transport->valuestring) >= DCF_CONFIG_STR_LEN) {
            dcf_log_error("Transport string too long in secondary config");
            return DCF_ERR_CONFIG_INVALID;
        }
        strncpy(primary->transport, transport->valuestring, DCF_CONFIG_STR_LEN);
    }

    cJSON* host = cJSON_GetObjectItem(sec, "host");
    if (host && cJSON_IsString(host)) {
        if (strlen(host->valuestring) >= DCF_CONFIG_STR_LEN) {
            dcf_log_error("Host string too long in secondary config");
            return DCF_ERR_CONFIG_INVALID;
        }
        strncpy(primary->host, host->valuestring, DCF_CONFIG_STR_LEN);
    }

    cJSON* port = cJSON_GetObjectItem(sec, "port");
    if (port && cJSON_IsNumber(port)) {
        if (port->valuedouble < 1 || port->valuedouble > 65535) {
            dcf_log_error("Invalid port in secondary config: %f", port->valuedouble);
            return DCF_ERR_CONFIG_INVALID;
        }
        primary->port = (int)port->valuedouble;
    }

    cJSON* mode = cJSON_GetObjectItem(sec, "mode");
    if (mode && cJSON_IsString(mode)) {
        if (strcmp(mode->valuestring, "client") == 0) primary->mode = CLIENT_MODE;
        else if (strcmp(mode->valuestring, "server") == 0) primary->mode = SERVER_MODE;
        else if (strcmp(mode->valuestring, "p2p") == 0) primary->mode = P2P_MODE;
        else if (strcmp(mode->valuestring, "auto") == 0) primary->mode = AUTO_MODE;
        else if (strcmp(mode->valuestring, "master") == 0) primary->mode = MASTER_MODE;
        else {
            dcf_log_error("Invalid mode in secondary config: %s", mode->valuestring);
            return DCF_ERR_CONFIG_INVALID;
        }
    }

    cJSON* peers = cJSON_GetObjectItem(sec, "peers");
    if (peers && cJSON_IsArray(peers)) {
        cJSON* peer;
        cJSON_ArrayForEach(peer, peers) {
            if (cJSON_IsString(peer) && primary->peer_count < DCF_MAX_PEERS) {
                if (strlen(peer->valuestring) >= DCF_CONFIG_STR_LEN) {
                    dcf_log_error("Peer string too long: %s", peer->valuestring);
                    continue;
                }
                strncpy(primary->peers[primary->peer_count], peer->valuestring, DCF_CONFIG_STR_LEN);
                primary->peer_count++;
            }
        }
    }

    cJSON* rtt = cJSON_GetObjectItem(sec, "group_rtt_threshold");
    if (rtt && cJSON_IsNumber(rtt)) {
        if (rtt->valuedouble < 0) {
            dcf_log_error("Invalid RTT threshold in secondary config: %f", rtt->valuedouble);
            return DCF_ERR_CONFIG_INVALID;
        }
        primary->group_rtt_threshold = (int)rtt->valuedouble;
    }

    return DCF_SUCCESS;
}

// Send: Route to appropriate transport
DCFError send(void* self, const uint8_t* data, size_t size, const char* target) {
    UnifiedDualTransport* data = (UnifiedDualTransport*)self;
    if (!data->primary_transport || !data->secondary_transport) {
        dcf_log_error("NULL transports in send");
        return DCF_ERR_NULL_PTR;
    }

    cJSON* plugins = cJSON_GetObjectItem(data->config->json_config, "plugins");
    if (!plugins) {
        dcf_log_error("No plugins in send");
        return DCF_ERR_CONFIG_MISSING;
    }
    cJSON* secondary_host = cJSON_GetObjectItem(plugins, "secondary_host");
    cJSON* secondary_port = cJSON_GetObjectItem(plugins, "secondary_port");
    char secondary_target[DCF_CONFIG_STR_LEN];
    snprintf(secondary_target, DCF_CONFIG_STR_LEN, "%s:%d", secondary_host->valuestring, (int)secondary_port->valuedouble);

    DCFError err;
    if (strstr(target, secondary_target)) {
        err = data->secondary_iface->send(data->secondary_transport, data, size, target);
    } else {
        err = data->primary_iface->send(data->primary_transport, data, size, target);
    }
    if (err != DCF_SUCCESS) {
        dcf_log_error("Send failed to %s: %s", target, dcf_error_str(err));
    }
    return err;
}

// Receive: Stub; actual in listener
uint8_t* receive(void* self, size_t* size) {
    if (size) *size = 0;
    return NULL;
}

// Listener thread: Poll and forward
void* listener_thread_func(void* arg) {
    UnifiedDualTransport* data = (UnifiedDualTransport*)arg;
    while (data->running) {
        size_t size;
        uint8_t* msg = data->primary_iface->receive(data->primary_transport, &size);
        if (msg && size > 0) {
            cJSON* plugins = cJSON_GetObjectItem(data->config->json_config, "plugins");
            if (!plugins) {
                dcf_log_error("No plugins in listener (primary)");
                free(msg);
                continue;
            }
            cJSON* secondary_host = cJSON_GetObjectItem(plugins, "secondary_host");
            cJSON* secondary_port = cJSON_GetObjectItem(plugins, "secondary_port");
            char target[DCF_CONFIG_STR_LEN];
            snprintf(target, DCF_CONFIG_STR_LEN, "%s:%d", secondary_host->valuestring, (int)secondary_port->valuedouble);
            DCFError err = data->secondary_iface->send(data->secondary_transport, msg, size, target);
            if (err != DCF_SUCCESS) {
                dcf_log_error("Forward from primary failed: %s", dcf_error_str(err));
            }
            free(msg);
        }

        msg = data->secondary_iface->receive(data->secondary_transport, &size);
        if (msg && size > 0) {
            cJSON* plugins = cJSON_GetObjectItem(data->config->json_config, "plugins");
            if (!plugins) {
                dcf_log_error("No plugins in listener (secondary)");
                free(msg);
                continue;
            }
            cJSON* primary_host = cJSON_GetObjectItem(plugins, "primary_host");
            cJSON* primary_port = cJSON_GetObjectItem(plugins, "primary_port");
            char target[DCF_CONFIG_STR_LEN];
            snprintf(target, DCF_CONFIG_STR_LEN, "%s:%d", primary_host->valuestring, (int)primary_port->valuedouble);
            DCFError err = data->primary_iface->send(data->primary_transport, msg, size, target);
            if (err != DCF_SUCCESS) {
                dcf_log_error("Forward from secondary failed: %s", dcf_error_str(err));
            }
            free(msg);
        }

        usleep(1000);  // 1ms sleep to avoid busy-wait CPU spike
    }
    return NULL;
}

// Destroy: Cleanup
void destroy(void* self) {
    UnifiedDualTransport* data = (UnifiedDualTransport*)self;
    data->running = 0;
    void* thread_res;
    int join_err = pthread_join(data->listener_thread, &thread_res);
    if (join_err != 0) {
        dcf_log_error("Failed to join listener thread: %d", join_err);
    }
    if (data->primary_transport && data->primary_iface)
        data->primary_iface->destroy(data->primary_transport);
    if (data->secondary_transport && data->secondary_iface)
        data->secondary_iface->destroy(data->secondary_transport);
    if (data->secondary_config) cJSON_Delete(data->secondary_config);
    if (data->secondary_config_path) free(data->secondary_config_path);
    free(data);
}

// Plugin interface
ITransport iface = { (bool (*)(void*, const char*, int))setup,
                     (bool (*)(void*, const uint8_t*, size_t, const char*))send,
                     receive, destroy };

void* create_plugin(DCFConfig* config) {
    if (!config || !config->json_config) {
        dcf_log_error("NULL config in create_plugin");
        return NULL;
    }
    UnifiedDualTransport* data = calloc(1, sizeof(UnifiedDualTransport));
    if (!data) {
        dcf_log_error("Memory allocation failed for UnifiedDualTransport");
        return NULL;
    }
    data->config = config;

    cJSON* plugins = cJSON_GetObjectItem(config->json_config, "plugins");
    if (!plugins) {
        dcf_log_error("No plugins in create_plugin");
        free(data);
        return NULL;
    }
    cJSON* config_path = cJSON_GetObjectItem(plugins, "secondary_config");
    if (config_path && cJSON_IsString(config_path)) {
        data->secondary_config_path = strdup(config_path->valuestring);
        if (!data->secondary_config_path) {
            dcf_log_error("strdup failed for secondary_config_path");
            free(data);
            return NULL;
        }
    }

    return data;
}

const char* get_plugin_version() { return "1.0.0"; }
