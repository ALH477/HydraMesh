/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * dcf_transport_v1.h — the v1 (legacy) transport plugin ABI.
 *
 * A v1 plugin is a shared object exporting:
 *     void*       create_plugin(void);        // allocate transport state
 *     const char* get_plugin_version(void);
 * and a file-scope `DCFTransportV1 iface` initialized with its four entry
 * points. The loader resolves the symbols with dlsym and calls through the
 * struct. This header is self-contained on purpose: the historical
 * <dcf_sdk/dcf_plugin_manager.h> include path never existed in this tree
 * (dcf_plugin_manager.h itself is a design reference in include/experimental/),
 * and plugins only ever needed this ABI.
 *
 * The rich 11-member ITransport in dcf_types.h is the v2 ABI; do not mix the
 * two in one plugin (that mismatch was review finding C2).
 */
#ifndef DCF_TRANSPORT_V1_H
#define DCF_TRANSPORT_V1_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct {
    bool (*setup)(void* self, const char* host, int port);
    bool (*send)(void* self, const uint8_t* data, size_t size, const char* target);
    uint8_t* (*receive)(void* self, size_t* size);
    void (*destroy)(void* self);
} DCFTransportV1;

#endif /* DCF_TRANSPORT_V1_H */
