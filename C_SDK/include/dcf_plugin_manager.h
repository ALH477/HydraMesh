#ifndef DCF_PLUGIN_MANAGER_H
#define DCF_PLUGIN_MANAGER_H
#include "dcf_config.h"
#include "dcf_error.h"

typedef struct {
    bool (*setup)(void* self, const char* host, int port);
    bool (*send)(void* self, const uint8_t* data, size_t size, const char* target);
    uint8_t* (*receive)(void* self, size_t* size);
    void (*destroy)(void* self);
} ITransport;

typedef struct DCFPluginManager DCFPluginManager;

DCFPluginManager* dcf_plugin_manager_new(void);
DCFError dcf_plugin_manager_load(DCFPluginManager* manager, DCFConfig* config);
ITransport* dcf_plugin_manager_get_transport(DCFPluginManager* manager);
void dcf_plugin_manager_free(DCFPluginManager* manager);
#endif
