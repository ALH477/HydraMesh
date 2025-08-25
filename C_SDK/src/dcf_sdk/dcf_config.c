#include "dcf_config.h"
#include <cjson/cJSON.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

struct DCFConfig {
    DCFMode mode;
    char* node_id;
    char** peers;
    size_t peer_count;
    char* host;
    int port;
    int rtt_threshold;
    char* plugin_path;
};

DCFConfig* dcf_config_load(const char* path) {
    if (!path) return NULL;
    FILE* fp = fopen(path, "r");
    if (!fp) return NULL;
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    char* buffer = calloc(1, size + 1);
    if (!buffer) { fclose(fp); return NULL; }
    fread(buffer, 1, size, fp);
    fclose(fp);
    cJSON* json = cJSON_Parse(buffer);
    free(buffer);
    if (!json) return NULL;
    DCFConfig* config = calloc(1, sizeof(DCFConfig));
    if (!config) { cJSON_Delete(json); return NULL; }
    cJSON* mode = cJSON_GetObjectItem(json, "mode");
    if (cJSON_IsString(mode)) {
        if (strcmp(mode->valuestring, "client") == 0) config->mode = CLIENT_MODE;
        else if (strcmp(mode->valuestring, "server") == 0) config->mode = SERVER_MODE;
        else if (strcmp(mode->valuestring, "p2p") == 0) config->mode = P2P_MODE;
        else if (strcmp(mode->valuestring, "auto") == 0) config->mode = AUTO_MODE;
    }
    cJSON* node_id = cJSON_GetObjectItem(json, "node_id");
    if (cJSON_IsString(node_id)) config->node_id = strdup(node_id->valuestring);
    cJSON* peers = cJSON_GetObjectItem(json, "peers");
    if (cJSON_IsArray(peers)) {
        config->peer_count = cJSON_GetArraySize(peers);
        config->peers = calloc(config->peer_count, sizeof(char*));
        if (!config->peers) { cJSON_Delete(json); dcf_config_free(config); return NULL; }
        for (size_t i = 0; i < config->peer_count; i++) {
            cJSON* peer = cJSON_GetArrayItem(peers, i);
            if (cJSON_IsString(peer)) config->peers[i] = strdup(peer->valuestring);
        }
    }
    cJSON* host = cJSON_GetObjectItem(json, "host");
    if (cJSON_IsString(host)) config->host = strdup(host->valuestring);
    cJSON* port = cJSON_GetObjectItem(json, "port");
    if (cJSON_IsNumber(port)) config->port = port->valueint;
    cJSON* rtt = cJSON_GetObjectItem(json, "rtt_threshold");
    if (cJSON_IsNumber(rtt)) config->rtt_threshold = rtt->valueint;
    cJSON* plugins = cJSON_GetObjectItem(json, "plugins");
    if (cJSON_IsString(plugins)) config->plugin_path = strdup(plugins->valuestring);
    cJSON_Delete(json);
    return config;
}

DCFError dcf_config_update(DCFConfig* config, const char* key, const char* value) {
    if (!config || !key || !value) return DCF_ERR_NULL_PTR;
    if (strcmp(key, "mode") == 0) {
        if (strcmp(value, "client") == 0) config->mode = CLIENT_MODE;
        else if (strcmp(value, "server") == 0) config->mode = SERVER_MODE;
        else if (strcmp(value, "p2p") == 0) config->mode = P2P_MODE;
        else if (strcmp(value, "auto") == 0) config->mode = AUTO_MODE;
        else return DCF_ERR_INVALID_ARG;
    } else if (strcmp(key, "node_id") == 0) {
        free(config->node_id);
        config->node_id = strdup(value);
    } else if (strcmp(key, "host") == 0) {
        free(config->host);
        config->host = strdup(value);
    } else if (strcmp(key, "port") == 0) {
        config->port = atoi(value);
    } else if (strcmp(key, "rtt_threshold") == 0) {
        config->rtt_threshold = atoi(value);
    } else if (strcmp(key, "plugin_path") == 0) {
        free(config->plugin_path);
        config->plugin_path = strdup(value);
    } else {
        return DCF_ERR_INVALID_ARG;
    }
    return DCF_SUCCESS;
}

void dcf_config_free(DCFConfig* config) {
    if (!config) return;
    free(config->node_id);
    free(config->host);
    free(config->plugin_path);
    for (size_t i = 0; i < config->peer_count; i++) free(config->peers[i]);
    free(config->peers);
    free(config);
}
