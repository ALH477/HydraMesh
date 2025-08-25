#include "dcf_plugin_manager.h"
#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>

struct DCFPluginManager {
    void* handle;
    ITransport* transport;
};

DCFPluginManager* dcf_plugin_manager_new(void) {
    DCFPluginManager* manager = calloc(1, sizeof(DCFPluginManager));
    if (!manager) return NULL;
    return manager;
}

DCFError dcf_plugin_manager_load(DCFPluginManager* manager, DCFConfig* config) {
    if (!manager || !config) return DCF_ERR_NULL_PTR;
    char* path;
    DCFError err = dcf_config_get_plugin_path(config, &path);
    if (err != DCF_SUCCESS || !path) return DCF_ERR_PLUGIN_FAIL;
    manager->handle = dlopen(path, RTLD_LAZY);
    free(path);
    if (!manager->handle) return DCF_ERR_PLUGIN_FAIL;
    typedef ITransport* (*create_fn)(void);
    typedef const char* (*version_fn)(void);
    create_fn create = (create_fn)dlsym(manager->handle, "create_plugin");
    version_fn get_version = (version_fn)dlsym(manager->handle, "get_plugin_version");
    if (!create || !get_version || strcmp(get_version(), "1.0") != 0) {
        dlclose(manager->handle);
        return DCF_ERR_PLUGIN_FAIL;
    }
    manager->transport = create();
    if (!manager->transport) {
        dlclose(manager->handle);
        return DCF_ERR_PLUGIN_FAIL;
    }
    char* host;
    err = dcf_config_get_host(config, &host);
    if (err != DCF_SUCCESS) return err;
    int port = dcf_config_get_port(config);
    if (!manager->transport->setup(manager->transport, host, port)) {
        free(host);
        return DCF_ERR_PLUGIN_FAIL;
    }
    free(host);
    return DCF_SUCCESS;
}

ITransport* dcf_plugin_manager_get_transport(DCFPluginManager* manager) {
    if (!manager) return NULL;
    return manager->transport;
}

void dcf_plugin_manager_free(DCFPluginManager* manager) {
    if (!manager) return;
    if (manager->transport && manager->transport->destroy) manager->transport->destroy(manager->transport);
    if (manager->handle) dlclose(manager->handle);
    free(manager);
}
