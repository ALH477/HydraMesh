#include "dcf_error.h"

const char* dcf_error_str(DCFError err) {
    switch (err) {
        case DCF_SUCCESS: return "Success";
        case DCF_ERR_NULL_PTR: return "Null pointer error";
        case DCF_ERR_MALLOC_FAIL: return "Memory allocation failed";
        case DCF_ERR_CONFIG_NOT_FOUND: return "Configuration file not found";
        case DCF_ERR_CONFIG_INVALID: return "Invalid configuration";
        case DCF_ERR_NETWORK_FAIL: return "Network failure";
        case DCF_ERR_SERIALIZATION_FAIL: return "Serialization failure";
        case DCF_ERR_DESERIALIZATION_FAIL: return "Deserialization failure";
        case DCF_ERR_PLUGIN_FAIL: return "Plugin loading error";
        case DCF_ERR_INVALID_STATE: return "Invalid client state";
        case DCF_ERR_ROUTE_NOT_FOUND: return "No route found";
        case DCF_ERR_GRPC_FAIL: return "gRPC operation failed";
        case DCF_ERR_INVALID_ARG: return "Invalid argument";
        case DCF_ERR_CONFIG_UPDATE_FAIL: return "Configuration update failed";
        case DCF_ERR_UNKNOWN: return "Unknown error";
    }
    return "Unknown error";
}
