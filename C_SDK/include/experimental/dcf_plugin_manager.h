// SPDX-License-Identifier: LGPL-3.0-only
//
// EXPERIMENTAL — declaration only, no implementation ships. This header is a
// design reference for a future plugin loader; none of the functions below
// exist in the built library. It lives in include/experimental/ (with
// dcf_config.h, which it depends on) and is not installed. The one piece
// plugins actually need — the DCFTransportV1 ABI — is real and lives in
// ../dcf_transport_v1.h.
#ifndef DCF_PLUGIN_MANAGER_H
#define DCF_PLUGIN_MANAGER_H
#include "dcf_config.h"
#include "dcf_error.h"
#include "dcf_types.h"
#include "../dcf_transport_v1.h"

typedef struct DCFPluginManager DCFPluginManager;

DCFPluginManager* dcf_plugin_manager_new(void);
DCFError dcf_plugin_manager_load(DCFPluginManager* manager, DCFConfig* config);
ITransport* dcf_plugin_manager_get_transport(DCFPluginManager* manager);
void dcf_plugin_manager_free(DCFPluginManager* manager);
#endif
