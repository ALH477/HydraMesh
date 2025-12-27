#ifndef DCF_INTERFACE_H
#define DCF_INTERFACE_H
#include "dcf_client.h"
#include "dcf_error.h"

typedef enum {
    DCF_CMD_INIT,
    DCF_CMD_START,
    DCF_CMD_STOP,
    DCF_CMD_STATUS,
    DCF_CMD_SEND,
    DCF_CMD_RECEIVE,
    DCF_CMD_HEALTH_CHECK,
    DCF_CMD_LIST_PEERS,
    DCF_CMD_HEAL,
    DCF_CMD_VERSION,
    DCF_CMD_BENCHMARK,
    DCF_CMD_GROUP_PEERS,
    DCF_CMD_SIMULATE_FAILURE,
    DCF_CMD_LOG_LEVEL,
    DCF_CMD_LOAD_PLUGIN,
    DCF_CMD_TUI,
    DCF_CMD_UNKNOWN
} DCFCmd;

DCFError dcf_interface_execute(DCFClient* client, DCFCmd cmd, const char** args, int arg_count, bool json_output, char** output);
DCFError dcf_interface_tui_start(DCFClient* client);
#endif
