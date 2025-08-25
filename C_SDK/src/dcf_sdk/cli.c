#include "dcf_client.h"
#include "dcf_interface.h"
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static struct option long_options[] = {
    {"json", no_argument, 0, 'j'},
    {0, 0, 0, 0}
};

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("Usage: dcf <command> [options]\n");
        printf("Commands:\n");
        printf("  init [config_path] - Initialize with config\n");
        printf("  start - Start DCF instance\n");
        printf("  stop - Stop DCF instance\n");
        printf("  status - Display status\n");
        printf("  send [data] [recipient] - Send message\n");
        printf("  receive - Receive message\n");
        printf("  health-check [peer] - Health check peer\n");
        printf("  list-peers - List peers\n");
        printf("  heal [peer] - Heal network for peer\n");
        printf("  version - Display version\n");
        printf("  benchmark [peer] - Benchmark peer\n");
        printf("  group-peers - Regroup peers\n");
        printf("  simulate-failure [peer] - Simulate failure\n");
        printf("  log-level [level] - Set log level (0=debug, 1=info, 2=error)\n");
        printf("  load-plugin [path] - Load plugin\n");
        printf("  tui - Start TUI\n");
        printf("Options:\n");
        printf("  -j, --json - Output in JSON format\n");
        return 1;
    }
    bool json_output = false;
    int opt;
    while ((opt = getopt_long(argc, argv, "j", long_options, NULL)) != -1) {
        switch (opt) {
            case 'j': json_output = true; break;
            default: return 1;
        }
    }
    DCFClient* client = dcf_client_new();
    if (!client) {
        printf("Error: %s\n", dcf_error_str(DCF_ERR_MALLOC_FAIL));
        return 1;
    }
    char* command = argv[optind];
    DCFCmd cmd = DCF_CMD_UNKNOWN;
    // Map command string to DCFCmd enum (as in prior)
    int cmd_arg_start = optind + 1;
    int cmd_arg_count = argc - cmd_arg_start;
    char* output;
    DCFError err = dcf_interface_execute(client, cmd, (const char**)argv + cmd_arg_start, cmd_arg_count, json_output, &output);
    if (err == DCF_SUCCESS) {
        printf("%s\n", output);
    } else {
        printf("Error: %s\n", dcf_error_str(err));
    }
    free(output);
    dcf_client_free(client);
    return err == DCF_SUCCESS ? 0 : 1;
}
