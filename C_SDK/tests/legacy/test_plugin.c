#include "dcf_plugin_manager.h"
#include "dcf_config.h"
#include <stdio.h>

int main() {
    DCFConfig* config = dcf_config_load("config.json");
    if (!config) {
        printf("Config load failed\n");
        return 1;
    }
    DCFPluginManager* manager = dcf_plugin_manager_new();
    if (!manager) {
        printf("Plugin manager creation failed\n");
        dcf_config_free(config);
        return 1;
    }
    DCFError err = dcf_plugin_manager_load(manager, config);
    if (err != DCF_SUCCESS) {
        printf("Plugin load failed: %s\n", dcf_error_str(err));
        dcf_plugin_manager_free(manager);
        dcf_config_free(config);
        return 1;
    }
    ITransport* transport = dcf_plugin_manager_get_transport(manager);
    if (!transport) {
        printf("Transport retrieval failed\n");
        dcf_plugin_manager_free(manager);
        dcf_config_free(config);
        return 1;
    }
    char* host;
    dcf_config_get_host(config, &host);
    int port = dcf_config_get_port(config);
    if (!transport->setup(transport, host, port)) {
        printf("Transport setup failed\n");
        free(host);
        dcf_plugin_manager_free(manager);
        dcf_config_free(config);
        return 1;
    }
    uint8_t* data = (uint8_t*)"Test";
    if (!transport->send(transport, data, strlen((char*)data), "localhost:50052")) {
        printf("Transport send failed\n");
        free(host);
        dcf_plugin_manager_free(manager);
        dcf_config_free(config);
        return 1;
    }
    size_t len;
    uint8_t* response = transport->receive(transport, &len);
    if (response) {
        printf("Received: %.*s\n", (int)len, response);
        free(response);
    }
    free(host);
    dcf_plugin_manager_free(manager);
    dcf_config_free(config);
    printf("Plugin tests passed\n");
    return 0;
}
