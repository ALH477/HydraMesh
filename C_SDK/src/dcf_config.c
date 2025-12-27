/**
 * @file dcf_config.c
 * @brief Configuration management implementation
 */

#include "dcf_config.h"
#include <cjson/cJSON.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <pthread.h>
#include <errno.h>

/* ============================================================================
 * Internal Structure
 * ============================================================================ */

#define MAX_CONFIG_CALLBACKS 16

typedef struct ConfigCallback {
    DCFConfigChangeCallback callback;
    void* user_data;
    int id;
    bool active;
} ConfigCallback;

struct DCFConfig {
    /* Core settings */
    DCFMode mode;
    char* node_id;
    char** peers;
    size_t peer_count;
    size_t peer_capacity;
    char* host;
    int port;
    int rtt_threshold;
    char* plugin_path;
    
    /* TLS settings */
    bool tls_enabled;
    char* tls_cert_path;
    char* tls_key_path;
    char* tls_ca_path;
    
    /* Thread safety */
    pthread_rwlock_t lock;
    bool lock_initialized;
    
    /* Change callbacks */
    ConfigCallback callbacks[MAX_CONFIG_CALLBACKS];
    int next_callback_id;
};

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static char* safe_strdup(const char* str) {
    if (!str) return NULL;
    char* dup = strdup(str);
    if (!dup) {
        DCF_LOG_ERROR("Failed to duplicate string: %s", strerror(errno));
    }
    return dup;
}

static void notify_change(DCFConfig* config, const char* key,
                          const char* old_value, const char* new_value) {
    for (int i = 0; i < MAX_CONFIG_CALLBACKS; i++) {
        if (config->callbacks[i].active && config->callbacks[i].callback) {
            config->callbacks[i].callback(key, old_value, new_value,
                                          config->callbacks[i].user_data);
        }
    }
}

static DCFError parse_mode(const char* mode_str, DCFMode* mode_out) {
    if (!mode_str || !mode_out) return DCF_ERR_NULL_PTR;
    
    if (strcmp(mode_str, "client") == 0) {
        *mode_out = DCF_MODE_CLIENT;
    } else if (strcmp(mode_str, "server") == 0) {
        *mode_out = DCF_MODE_SERVER;
    } else if (strcmp(mode_str, "p2p") == 0) {
        *mode_out = DCF_MODE_P2P;
    } else if (strcmp(mode_str, "auto") == 0) {
        *mode_out = DCF_MODE_AUTO;
    } else if (strcmp(mode_str, "hybrid") == 0) {
        *mode_out = DCF_MODE_HYBRID;
    } else {
        return DCF_ERR_INVALID_ARG;
    }
    return DCF_SUCCESS;
}

static const char* mode_to_string(DCFMode mode) {
    switch (mode) {
        case DCF_MODE_CLIENT: return "client";
        case DCF_MODE_SERVER: return "server";
        case DCF_MODE_P2P:    return "p2p";
        case DCF_MODE_AUTO:   return "auto";
        case DCF_MODE_HYBRID: return "hybrid";
        default:              return "unknown";
    }
}

/* ============================================================================
 * Lifecycle Functions
 * ============================================================================ */

DCFConfig* dcf_config_create_default(void) {
    DCFConfig* config = calloc(1, sizeof(DCFConfig));
    if (!config) {
        DCF_LOG_ERROR("Failed to allocate config");
        return NULL;
    }
    
    /* Initialize lock */
    if (pthread_rwlock_init(&config->lock, NULL) != 0) {
        DCF_LOG_ERROR("Failed to initialize config lock");
        free(config);
        return NULL;
    }
    config->lock_initialized = true;
    
    /* Set defaults */
    config->mode = DCF_MODE_AUTO;
    config->port = DCF_DEFAULT_PORT;
    config->rtt_threshold = DCF_DEFAULT_RTT_THRESHOLD;
    config->host = safe_strdup("localhost");
    config->peer_capacity = 16;
    config->peers = calloc(config->peer_capacity, sizeof(char*));
    
    if (!config->host || !config->peers) {
        dcf_config_free(config);
        return NULL;
    }
    
    DCF_LOG_DEBUG("Created default configuration");
    return config;
}

DCFConfig* dcf_config_load(const char* path) {
    DCF_RETURN_IF_NULL(path, "config path is NULL");
    
    DCF_LOG_INFO("Loading configuration from: %s", path);
    
    FILE* fp = fopen(path, "r");
    if (!fp) {
        DCF_LOG_AND_SET_ERROR(DCF_ERR_CONFIG_NOT_FOUND, 
                              "Cannot open config file: %s (%s)", path, strerror(errno));
        return NULL;
    }
    
    /* Read file */
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    if (size <= 0 || size > 1024 * 1024) {  /* Max 1MB config */
        DCF_LOG_ERROR("Invalid config file size: %ld", size);
        fclose(fp);
        return NULL;
    }
    fseek(fp, 0, SEEK_SET);
    
    char* buffer = calloc(1, (size_t)size + 1);
    if (!buffer) {
        DCF_LOG_ERROR("Failed to allocate buffer for config");
        fclose(fp);
        return NULL;
    }
    
    size_t read_size = fread(buffer, 1, (size_t)size, fp);
    fclose(fp);
    
    if (read_size != (size_t)size) {
        DCF_LOG_ERROR("Failed to read entire config file");
        free(buffer);
        return NULL;
    }
    
    DCFConfig* config = dcf_config_load_from_string(buffer);
    free(buffer);
    return config;
}

DCFConfig* dcf_config_load_from_string(const char* json_str) {
    DCF_RETURN_IF_NULL(json_str, "JSON string is NULL");
    
    cJSON* json = cJSON_Parse(json_str);
    if (!json) {
        const char* error_ptr = cJSON_GetErrorPtr();
        DCF_LOG_AND_SET_ERROR(DCF_ERR_CONFIG_PARSE_FAIL,
                              "JSON parse error near: %.20s", 
                              error_ptr ? error_ptr : "unknown");
        return NULL;
    }
    
    DCFConfig* config = dcf_config_create_default();
    if (!config) {
        cJSON_Delete(json);
        return NULL;
    }
    
    /* Parse mode */
    cJSON* mode = cJSON_GetObjectItem(json, "mode");
    if (cJSON_IsString(mode)) {
        if (parse_mode(mode->valuestring, &config->mode) != DCF_SUCCESS) {
            DCF_LOG_WARN("Invalid mode '%s', using default", mode->valuestring);
        }
    }
    
    /* Parse node_id */
    cJSON* node_id = cJSON_GetObjectItem(json, "node_id");
    if (cJSON_IsString(node_id) && node_id->valuestring[0]) {
        free(config->node_id);
        config->node_id = safe_strdup(node_id->valuestring);
    }
    
    /* Parse host */
    cJSON* host = cJSON_GetObjectItem(json, "host");
    if (cJSON_IsString(host) && host->valuestring[0]) {
        free(config->host);
        config->host = safe_strdup(host->valuestring);
    }
    
    /* Parse port */
    cJSON* port = cJSON_GetObjectItem(json, "port");
    if (cJSON_IsNumber(port)) {
        int port_val = port->valueint;
        if (port_val > 0 && port_val <= 65535) {
            config->port = port_val;
        } else {
            DCF_LOG_WARN("Invalid port %d, using default %d", port_val, config->port);
        }
    }
    
    /* Parse rtt_threshold */
    cJSON* rtt = cJSON_GetObjectItem(json, "rtt_threshold");
    if (cJSON_IsNumber(rtt) && rtt->valueint > 0) {
        config->rtt_threshold = rtt->valueint;
    }
    
    /* Parse peers array */
    cJSON* peers = cJSON_GetObjectItem(json, "peers");
    if (cJSON_IsArray(peers)) {
        size_t peer_count = (size_t)cJSON_GetArraySize(peers);
        if (peer_count > 0) {
            /* Ensure capacity */
            if (peer_count > config->peer_capacity) {
                char** new_peers = realloc(config->peers, peer_count * sizeof(char*));
                if (new_peers) {
                    config->peers = new_peers;
                    config->peer_capacity = peer_count;
                }
            }
            
            for (size_t i = 0; i < peer_count && i < config->peer_capacity; i++) {
                cJSON* peer = cJSON_GetArrayItem(peers, (int)i);
                if (cJSON_IsString(peer) && peer->valuestring[0]) {
                    config->peers[config->peer_count++] = safe_strdup(peer->valuestring);
                }
            }
        }
    }
    
    /* Parse plugin path */
    cJSON* plugins = cJSON_GetObjectItem(json, "plugins");
    if (!plugins) plugins = cJSON_GetObjectItem(json, "plugin_path");
    if (cJSON_IsString(plugins) && plugins->valuestring[0]) {
        config->plugin_path = safe_strdup(plugins->valuestring);
    }
    
    /* Parse TLS settings */
    cJSON* tls = cJSON_GetObjectItem(json, "tls");
    if (cJSON_IsObject(tls)) {
        cJSON* enabled = cJSON_GetObjectItem(tls, "enabled");
        config->tls_enabled = cJSON_IsTrue(enabled);
        
        cJSON* cert = cJSON_GetObjectItem(tls, "cert_path");
        if (cJSON_IsString(cert)) {
            config->tls_cert_path = safe_strdup(cert->valuestring);
        }
        
        cJSON* key = cJSON_GetObjectItem(tls, "key_path");
        if (cJSON_IsString(key)) {
            config->tls_key_path = safe_strdup(key->valuestring);
        }
        
        cJSON* ca = cJSON_GetObjectItem(tls, "ca_path");
        if (cJSON_IsString(ca)) {
            config->tls_ca_path = safe_strdup(ca->valuestring);
        }
    }
    
    cJSON_Delete(json);
    
    DCF_LOG_INFO("Configuration loaded: mode=%s, host=%s, port=%d, peers=%zu",
                 mode_to_string(config->mode), config->host, config->port, 
                 config->peer_count);
    
    return config;
}

DCFConfig* dcf_config_clone(const DCFConfig* config) {
    if (!config) return NULL;
    
    DCFConfig* clone = dcf_config_create_default();
    if (!clone) return NULL;
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    
    clone->mode = config->mode;
    clone->port = config->port;
    clone->rtt_threshold = config->rtt_threshold;
    clone->tls_enabled = config->tls_enabled;
    
    free(clone->node_id);
    clone->node_id = config->node_id ? safe_strdup(config->node_id) : NULL;
    
    free(clone->host);
    clone->host = config->host ? safe_strdup(config->host) : NULL;
    
    clone->plugin_path = config->plugin_path ? safe_strdup(config->plugin_path) : NULL;
    clone->tls_cert_path = config->tls_cert_path ? safe_strdup(config->tls_cert_path) : NULL;
    clone->tls_key_path = config->tls_key_path ? safe_strdup(config->tls_key_path) : NULL;
    clone->tls_ca_path = config->tls_ca_path ? safe_strdup(config->tls_ca_path) : NULL;
    
    /* Clone peers */
    for (size_t i = 0; i < config->peer_count; i++) {
        dcf_config_add_peer(clone, config->peers[i]);
    }
    
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return clone;
}

DCFError dcf_config_save(const DCFConfig* config, const char* path) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(path, "path is NULL");
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    
    cJSON* json = cJSON_CreateObject();
    if (!json) {
        pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
        return DCF_ERR_MALLOC_FAIL;
    }
    
    cJSON_AddStringToObject(json, "mode", mode_to_string(config->mode));
    if (config->node_id) cJSON_AddStringToObject(json, "node_id", config->node_id);
    if (config->host) cJSON_AddStringToObject(json, "host", config->host);
    cJSON_AddNumberToObject(json, "port", config->port);
    cJSON_AddNumberToObject(json, "rtt_threshold", config->rtt_threshold);
    if (config->plugin_path) cJSON_AddStringToObject(json, "plugin_path", config->plugin_path);
    
    if (config->peer_count > 0) {
        cJSON* peers = cJSON_AddArrayToObject(json, "peers");
        for (size_t i = 0; i < config->peer_count; i++) {
            cJSON_AddItemToArray(peers, cJSON_CreateString(config->peers[i]));
        }
    }
    
    if (config->tls_enabled) {
        cJSON* tls = cJSON_AddObjectToObject(json, "tls");
        cJSON_AddBoolToObject(tls, "enabled", config->tls_enabled);
        if (config->tls_cert_path) cJSON_AddStringToObject(tls, "cert_path", config->tls_cert_path);
        if (config->tls_key_path) cJSON_AddStringToObject(tls, "key_path", config->tls_key_path);
        if (config->tls_ca_path) cJSON_AddStringToObject(tls, "ca_path", config->tls_ca_path);
    }
    
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    char* json_str = cJSON_Print(json);
    cJSON_Delete(json);
    
    if (!json_str) {
        return DCF_ERR_MALLOC_FAIL;
    }
    
    FILE* fp = fopen(path, "w");
    if (!fp) {
        free(json_str);
        return DCF_ERR_CONFIG_UPDATE_FAIL;
    }
    
    fprintf(fp, "%s\n", json_str);
    fclose(fp);
    free(json_str);
    
    DCF_LOG_INFO("Configuration saved to: %s", path);
    return DCF_SUCCESS;
}

void dcf_config_free(DCFConfig* config) {
    if (!config) return;
    
    DCF_LOG_DEBUG("Freeing configuration");
    
    if (config->lock_initialized) {
        pthread_rwlock_destroy(&config->lock);
    }
    
    free(config->node_id);
    free(config->host);
    free(config->plugin_path);
    free(config->tls_cert_path);
    free(config->tls_key_path);
    free(config->tls_ca_path);
    
    for (size_t i = 0; i < config->peer_count; i++) {
        free(config->peers[i]);
    }
    free(config->peers);
    
    free(config);
}

/* ============================================================================
 * Validation
 * ============================================================================ */

DCFError dcf_config_validate(const DCFConfig* config, DCFConfigValidation* result) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(result, "result is NULL");
    
    memset(result, 0, sizeof(*result));
    result->valid = true;
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    
    /* Validate port */
    if (config->port <= 0 || config->port > 65535) {
        result->valid = false;
        result->error_count++;
        snprintf(result->error_message, sizeof(result->error_message),
                 "Invalid port: %d", config->port);
    }
    
    /* Validate host */
    if (!config->host || config->host[0] == '\0') {
        result->valid = false;
        result->error_count++;
        if (result->error_message[0] == '\0') {
            strncpy(result->error_message, "Host is required", sizeof(result->error_message));
        }
    }
    
    /* Validate TLS */
    if (config->tls_enabled) {
        if (!config->tls_cert_path || !config->tls_key_path) {
            result->valid = false;
            result->error_count++;
            if (result->error_message[0] == '\0') {
                strncpy(result->error_message, "TLS cert and key paths required when TLS enabled",
                        sizeof(result->error_message));
            }
        }
    }
    
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return result->valid ? DCF_SUCCESS : DCF_ERR_CONFIG_VALIDATION_FAIL;
}

/* ============================================================================
 * Getters (Thread-Safe)
 * ============================================================================ */

DCFError dcf_config_get_mode(const DCFConfig* config, DCFMode* mode_out) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(mode_out, "mode_out is NULL");
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    *mode_out = config->mode;
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return DCF_SUCCESS;
}

DCFError dcf_config_get_node_id(const DCFConfig* config, char** node_id_out) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(node_id_out, "node_id_out is NULL");
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    *node_id_out = config->node_id ? safe_strdup(config->node_id) : NULL;
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return *node_id_out ? DCF_SUCCESS : DCF_ERR_MALLOC_FAIL;
}

DCFError dcf_config_get_host(const DCFConfig* config, char** host_out) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(host_out, "host_out is NULL");
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    *host_out = config->host ? safe_strdup(config->host) : NULL;
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return *host_out ? DCF_SUCCESS : DCF_ERR_MALLOC_FAIL;
}

int dcf_config_get_port(const DCFConfig* config) {
    if (!config) return DCF_DEFAULT_PORT;
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    int port = config->port;
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return port;
}

int dcf_config_get_rtt_threshold(const DCFConfig* config) {
    if (!config) return DCF_DEFAULT_RTT_THRESHOLD;
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    int threshold = config->rtt_threshold;
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return threshold;
}

DCFError dcf_config_get_peers(const DCFConfig* config, char*** peers_out, size_t* count_out) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(peers_out, "peers_out is NULL");
    DCF_RETURN_IF_NULL(count_out, "count_out is NULL");
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    
    *count_out = config->peer_count;
    if (config->peer_count == 0) {
        *peers_out = NULL;
        pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
        return DCF_SUCCESS;
    }
    
    *peers_out = calloc(config->peer_count, sizeof(char*));
    if (!*peers_out) {
        pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
        return DCF_ERR_MALLOC_FAIL;
    }
    
    for (size_t i = 0; i < config->peer_count; i++) {
        (*peers_out)[i] = safe_strdup(config->peers[i]);
        if (!(*peers_out)[i]) {
            /* Cleanup on failure */
            for (size_t j = 0; j < i; j++) {
                free((*peers_out)[j]);
            }
            free(*peers_out);
            *peers_out = NULL;
            *count_out = 0;
            pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
            return DCF_ERR_MALLOC_FAIL;
        }
    }
    
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    return DCF_SUCCESS;
}

DCFError dcf_config_get_plugin_path(const DCFConfig* config, char** path_out) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(path_out, "path_out is NULL");
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    *path_out = config->plugin_path ? safe_strdup(config->plugin_path) : NULL;
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return DCF_SUCCESS;
}

bool dcf_config_get_tls_enabled(const DCFConfig* config) {
    if (!config) return false;
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&config->lock);
    bool enabled = config->tls_enabled;
    pthread_rwlock_unlock((pthread_rwlock_t*)&config->lock);
    
    return enabled;
}

/* ============================================================================
 * Setters (Thread-Safe)
 * ============================================================================ */

DCFError dcf_config_update(DCFConfig* config, const char* key, const char* value) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(key, "key is NULL");
    DCF_RETURN_IF_NULL(value, "value is NULL");
    
    DCFError err = DCF_SUCCESS;
    
    if (strcmp(key, "mode") == 0) {
        DCFMode mode;
        err = parse_mode(value, &mode);
        if (err == DCF_SUCCESS) {
            err = dcf_config_set_mode(config, mode);
        }
    } else if (strcmp(key, "node_id") == 0) {
        err = dcf_config_set_node_id(config, value);
    } else if (strcmp(key, "host") == 0) {
        err = dcf_config_set_host(config, value);
    } else if (strcmp(key, "port") == 0) {
        int port = atoi(value);
        err = dcf_config_set_port(config, port);
    } else if (strcmp(key, "rtt_threshold") == 0) {
        pthread_rwlock_wrlock(&config->lock);
        config->rtt_threshold = atoi(value);
        pthread_rwlock_unlock(&config->lock);
    } else if (strcmp(key, "plugin_path") == 0) {
        pthread_rwlock_wrlock(&config->lock);
        free(config->plugin_path);
        config->plugin_path = safe_strdup(value);
        pthread_rwlock_unlock(&config->lock);
    } else {
        err = DCF_ERR_INVALID_ARG;
    }
    
    return err;
}

DCFError dcf_config_set_mode(DCFConfig* config, DCFMode mode) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    
    pthread_rwlock_wrlock(&config->lock);
    const char* old = mode_to_string(config->mode);
    config->mode = mode;
    const char* new = mode_to_string(mode);
    notify_change(config, "mode", old, new);
    pthread_rwlock_unlock(&config->lock);
    
    DCF_LOG_DEBUG("Mode changed to: %s", new);
    return DCF_SUCCESS;
}

DCFError dcf_config_set_node_id(DCFConfig* config, const char* node_id) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(node_id, "node_id is NULL");
    
    char* new_id = safe_strdup(node_id);
    if (!new_id) return DCF_ERR_MALLOC_FAIL;
    
    pthread_rwlock_wrlock(&config->lock);
    char* old = config->node_id;
    config->node_id = new_id;
    notify_change(config, "node_id", old, new_id);
    free(old);
    pthread_rwlock_unlock(&config->lock);
    
    return DCF_SUCCESS;
}

DCFError dcf_config_set_host(DCFConfig* config, const char* host) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(host, "host is NULL");
    
    char* new_host = safe_strdup(host);
    if (!new_host) return DCF_ERR_MALLOC_FAIL;
    
    pthread_rwlock_wrlock(&config->lock);
    char* old = config->host;
    config->host = new_host;
    notify_change(config, "host", old, new_host);
    free(old);
    pthread_rwlock_unlock(&config->lock);
    
    return DCF_SUCCESS;
}

DCFError dcf_config_set_port(DCFConfig* config, int port) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    
    if (port <= 0 || port > 65535) {
        DCF_SET_ERROR(DCF_ERR_ARG_OUT_OF_RANGE, "Port must be 1-65535, got %d", port);
        return DCF_ERR_ARG_OUT_OF_RANGE;
    }
    
    pthread_rwlock_wrlock(&config->lock);
    config->port = port;
    pthread_rwlock_unlock(&config->lock);
    
    return DCF_SUCCESS;
}

DCFError dcf_config_add_peer(DCFConfig* config, const char* peer) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(peer, "peer is NULL");
    
    pthread_rwlock_wrlock(&config->lock);
    
    /* Check for duplicate */
    for (size_t i = 0; i < config->peer_count; i++) {
        if (strcmp(config->peers[i], peer) == 0) {
            pthread_rwlock_unlock(&config->lock);
            return DCF_SUCCESS;  /* Already exists */
        }
    }
    
    /* Expand if needed */
    if (config->peer_count >= config->peer_capacity) {
        size_t new_cap = config->peer_capacity * 2;
        char** new_peers = realloc(config->peers, new_cap * sizeof(char*));
        if (!new_peers) {
            pthread_rwlock_unlock(&config->lock);
            return DCF_ERR_MALLOC_FAIL;
        }
        config->peers = new_peers;
        config->peer_capacity = new_cap;
    }
    
    config->peers[config->peer_count] = safe_strdup(peer);
    if (!config->peers[config->peer_count]) {
        pthread_rwlock_unlock(&config->lock);
        return DCF_ERR_MALLOC_FAIL;
    }
    config->peer_count++;
    
    pthread_rwlock_unlock(&config->lock);
    
    DCF_LOG_DEBUG("Added peer: %s (total: %zu)", peer, config->peer_count);
    return DCF_SUCCESS;
}

DCFError dcf_config_remove_peer(DCFConfig* config, const char* peer) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(peer, "peer is NULL");
    
    pthread_rwlock_wrlock(&config->lock);
    
    for (size_t i = 0; i < config->peer_count; i++) {
        if (strcmp(config->peers[i], peer) == 0) {
            free(config->peers[i]);
            /* Shift remaining peers */
            for (size_t j = i; j < config->peer_count - 1; j++) {
                config->peers[j] = config->peers[j + 1];
            }
            config->peer_count--;
            pthread_rwlock_unlock(&config->lock);
            
            DCF_LOG_DEBUG("Removed peer: %s (remaining: %zu)", peer, config->peer_count);
            return DCF_SUCCESS;
        }
    }
    
    pthread_rwlock_unlock(&config->lock);
    return DCF_ERR_INVALID_ARG;  /* Peer not found */
}

/* ============================================================================
 * Callback Management
 * ============================================================================ */

int dcf_config_register_callback(DCFConfig* config, DCFConfigChangeCallback callback,
                                  void* user_data) {
    if (!config || !callback) return -1;
    
    pthread_rwlock_wrlock(&config->lock);
    
    for (int i = 0; i < MAX_CONFIG_CALLBACKS; i++) {
        if (!config->callbacks[i].active) {
            config->callbacks[i].callback = callback;
            config->callbacks[i].user_data = user_data;
            config->callbacks[i].id = config->next_callback_id++;
            config->callbacks[i].active = true;
            
            int id = config->callbacks[i].id;
            pthread_rwlock_unlock(&config->lock);
            return id;
        }
    }
    
    pthread_rwlock_unlock(&config->lock);
    return -1;  /* No slots available */
}

DCFError dcf_config_unregister_callback(DCFConfig* config, int callback_id) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    
    pthread_rwlock_wrlock(&config->lock);
    
    for (int i = 0; i < MAX_CONFIG_CALLBACKS; i++) {
        if (config->callbacks[i].active && config->callbacks[i].id == callback_id) {
            config->callbacks[i].active = false;
            config->callbacks[i].callback = NULL;
            config->callbacks[i].user_data = NULL;
            pthread_rwlock_unlock(&config->lock);
            return DCF_SUCCESS;
        }
    }
    
    pthread_rwlock_unlock(&config->lock);
    return DCF_ERR_INVALID_ARG;
}

/* ============================================================================
 * Environment Variables
 * ============================================================================ */

DCFError dcf_config_apply_env_overrides(DCFConfig* config) {
    DCF_RETURN_IF_NULL(config, "config is NULL");
    
    const char* env;
    
    if ((env = getenv("DCF_MODE"))) {
        dcf_config_update(config, "mode", env);
    }
    if ((env = getenv("DCF_HOST"))) {
        dcf_config_set_host(config, env);
    }
    if ((env = getenv("DCF_PORT"))) {
        int port = atoi(env);
        if (port > 0) dcf_config_set_port(config, port);
    }
    if ((env = getenv("DCF_NODE_ID"))) {
        dcf_config_set_node_id(config, env);
    }
    if ((env = getenv("DCF_RTT_THRESHOLD"))) {
        dcf_config_update(config, "rtt_threshold", env);
    }
    
    return DCF_SUCCESS;
}
