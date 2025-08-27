#include <dcf_sdk/dcf_plugin_manager.h>
#include <dcf_sdk/dcf_config.h>
#include <dcf_sdk/dcf_error.h>  // Assume this for DCFError
#include <dcf_sdk/dcf_logging.h>  // Assume SDK logging
#include <cjson/cJSON.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// Plugin private data
typedef struct {
    char* secondary_config_path;
    cJSON* secondary_config;
    DCFConfig* primary_config;
} ConfigUnifier;

// Function to merge configurations
static DCFError merge_configs(ConfigUnifier* self, DCFConfig* primary) {
    if (!self->secondary_config || !primary) return DCF_ERR_NULL_PTR;

    cJSON* sec = self->secondary_config;
    
    cJSON* transport = cJSON_GetObjectItem(sec, "transport");
    if (transport && cJSON_IsString(transport)) {
        if (strlen(transport->valuestring) >= DCF_CONFIG_STR_LEN) return DCF_ERR_CONFIG_INVALID;
        strncpy(primary->transport, transport->valuestring, DCF_CONFIG_STR_LEN);
    }

    cJSON* host = cJSON_GetObjectItem(sec, "host");
    if (host && cJSON_IsString(host)) {
        if (strlen(host->valuestring) >= DCF_CONFIG_STR_LEN) return DCF_ERR_CONFIG_INVALID;
        strncpy(primary->host, host->valuestring, DCF_CONFIG_STR_LEN);
    }

    cJSON* port = cJSON_GetObjectItem(sec, "port");
    if (port && cJSON_IsNumber(port)) {
        if (port->valuedouble < 1 || port->valuedouble > 65535) return DCF_ERR_CONFIG_INVALID;
        primary->port = (int)port->valuedouble;
    }

    cJSON* mode = cJSON_GetObjectItem(sec, "mode");
    if (mode && cJSON_IsString(mode)) {
        if (strcmp(mode->valuestring, "client") == 0) primary->mode = CLIENT_MODE;
        else if (strcmp(mode->valuestring, "server") == 0) primary->mode = SERVER_MODE;
        else if (strcmp(mode->valuestring, "p2p") == 0) primary->mode = P2P_MODE;
        else if (strcmp(mode->valuestring, "auto") == 0) primary->mode = AUTO_MODE;
        else if (strcmp(mode->valuestring, "master") == 0) primary->mode = MASTER_MODE;
        else return DCF_ERR_CONFIG_INVALID;
    }

    cJSON* peers = cJSON_GetObjectItem(sec, "peers");
    if (peers && cJSON_IsArray(peers)) {
        cJSON* peer;
        cJSON_ArrayForEach(peer, peers) {
            if (cJSON_IsString(peer) && primary->peer_count < DCF_MAX_PEERS) {
                if (strlen(peer->valuestring) >= DCF_CONFIG_STR_LEN) continue;  // Skip invalid
                strncpy(primary->peers[primary->peer_count], peer->valuestring, DCF_CONFIG_STR_LEN);
                primary->peer_count++;
            }
        }
    }

    cJSON* rtt = cJSON_GetObjectItem(sec, "group_rtt_threshold");
    if (rtt && cJSON_IsNumber(rtt)) {
        if (rtt->valuedouble < 0) return DCF_ERR_CONFIG_INVALID;
        primary->group_rtt_threshold = (int)rtt->valuedouble;
    }

    return DCF_SUCCESS;
}

// ITransport interface (returns DCFError instead of bool for better handling)
DCFError setup(void* self, const char* host, int port) {
    ConfigUnifier* unifier = (ConfigUnifier*)self;
    if (!unifier->secondary_config_path) {
        dcf_log_error("No secondary config path");
        return DCF_ERR_CONFIG_MISSING;
    }

    FILE* fp = fopen(unifier->secondary_config_path, "r");
    if (!fp) {
        dcf_log_error("Failed to open secondary config: %s", unifier->secondary_config_path);
        return DCF_ERR_FILE_IO;
    }

    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    if (size <= 0) {
        fclose(fp);
        return DCF_ERR_FILE_IO;
    }
    fseek(fp, 0, SEEK_SET);
    char* buffer = calloc(size + 1, 1);
    if (!buffer) {
        fclose(fp);
        dcf_log_error("Memory allocation failed");
        return DCF_ERR_MEMORY_ALLOC;
    }
    if (fread(buffer, 1, size, fp) != (size_t)size) {
        free(buffer);
        fclose(fp);
        return DCF_ERR_FILE_IO;
    }
    fclose(fp);

    unifier->secondary_config = cJSON_Parse(buffer);
    free(buffer);
    if (!unifier->secondary_config) {
        dcf_log_error("JSON parse failed for secondary config");
        return DCF_ERR_JSON_PARSE;
    }

    DCFError err = merge_configs(unifier, unifier->primary_config);
    if (err != DCF_SUCCESS) {
        dcf_log_error("Config merge failed: %s", dcf_error_str(err));
    }
    return err;
}

DCFError send(void* self, const uint8_t* data, size_t size, const char* target) {
    ConfigUnifier* unifier = (ConfigUnifier*)self;
    if (!unifier->primary_config || !unifier->primary_config->transport_iface) return DCF_ERR_NULL_PTR;
    return unifier->primary_config->transport_iface->send(
        unifier->primary_config->transport_instance, data, size, target);
}

uint8_t* receive(void* self, size_t* size) {  // Assume returns NULL on error; add error out-param if needed
    ConfigUnifier* unifier = (ConfigUnifier*)self;
    if (!unifier->primary_config || !unifier->primary_config->transport_iface) {
        if (size) *size = 0;
        return NULL;
    }
    return unifier->primary_config->transport_iface->receive(
        unifier->primary_config->transport_instance, size);
}

void destroy(void* self) {
    ConfigUnifier* unifier = (ConfigUnifier*)self;
    if (unifier->secondary_config) cJSON_Delete(unifier->secondary_config);
    if (unifier->secondary_config_path) free(unifier->secondary_config_path);
    free(unifier);
}

// Plugin interface
ITransport iface = { (bool (*)(void*, const char*, int))setup,  // Cast to match bool, but internally DCFError
                     (bool (*)(void*, const uint8_t*, size_t, const char*))send,
                     receive, destroy };

void* create_plugin(DCFConfig* config) {
    if (!config || !config->json_config) return NULL;
    ConfigUnifier* unifier = calloc(1, sizeof(ConfigUnifier));
    if (!unifier) return NULL;
    unifier->primary_config = config;

    cJSON* plugins = cJSON_GetObjectItem(config->json_config, "plugins");
    if (!plugins) {
        free(unifier);
        return NULL;
    }
    cJSON* config_path = cJSON_GetObjectItem(plugins, "secondary_config");
    if (config_path && cJSON_IsString(config_path)) {
        unifier->secondary_config_path = strdup(config_path->valuestring);
        if (!unifier->secondary_config_path) {
            free(unifier);
            return NULL;
        }
    } else {
        free(unifier);
        return NULL;
    }
    return unifier;
}

const char* get_plugin_version() { return "1.0.0"; }
