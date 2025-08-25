#include "dcf_networking.h"
#include "grpc_wrapper.h"
#include <stdlib.h>
#include <string.h>

struct DCFNetworking {
    void* grpc_handle;
    char* host;
    int port;
    DCFMode mode;
};

DCFNetworking* dcf_networking_new(void) {
    DCFNetworking* net = calloc(1, sizeof(DCFNetworking));
    if (!net) return NULL;
    return net;
}

DCFError dcf_networking_initialize(DCFNetworking* net, DCFConfig* config) {
    if (!net || !config) return DCF_ERR_NULL_PTR;
    DCFError err = dcf_config_get_host(config, &net->host);
    if (err != DCF_SUCCESS) return err;
    net->port = dcf_config_get_port(config);
    err = dcf_config_get_mode(config, &net->mode);
    if (err != DCF_SUCCESS) { free(net->host); return err; }
    net->grpc_handle = grpc_wrapper_new(net->host, net->port);
    if (!net->grpc_handle) { free(net->host); return DCF_ERR_GRPC_FAIL; }
    return DCF_SUCCESS;
}

DCFError dcf_networking_start(DCFNetworking* net, DCFMode mode) {
    if (!net) return DCF_ERR_NULL_PTR;
    net->mode = mode;
    if (net->mode == SERVER_MODE) {
        if (!grpc_wrapper_start_server(net->grpc_handle)) return DCF_ERR_GRPC_FAIL;
    }
    return DCF_SUCCESS;
}

DCFError dcf_networking_stop(DCFNetworking* net) {
    if (!net) return DCF_ERR_NULL_PTR;
    if (net->mode == SERVER_MODE) {
        if (!grpc_wrapper_stop_server(net->grpc_handle)) return DCF_ERR_GRPC_FAIL;
    }
    return DCF_SUCCESS;
}

DCFError dcf_networking_send(DCFNetworking* net, const uint8_t* data, size_t len, const char* recipient) {
    if (!net || !data || !recipient) return DCF_ERR_NULL_PTR;
    char* response;
    if (!grpc_wrapper_send(net->grpc_handle, data, len, recipient, &response)) return DCF_ERR_GRPC_FAIL;
    free(response);
    return DCF_SUCCESS;
}

DCFError dcf_networking_receive(DCFNetworking* net, char** message_out, char** sender_out) {
    if (!net || !message_out || !sender_out) return DCF_ERR_NULL_PTR;
    size_t len;
    uint8_t* data;
    char* sender;
    if (!grpc_wrapper_receive(net->grpc_handle, &data, &len, &sender)) return DCF_ERR_GRPC_FAIL;
    DCFError err = dcf_deserialize_message(data, len, message_out, sender_out);
    free(data);
    free(sender);
    return err;
}

void dcf_networking_free(DCFNetworking* net) {
    if (!net) return;
    if (net->grpc_handle) grpc_wrapper_free(net->grpc_handle);
    free(net->host);
    free(net);
}
