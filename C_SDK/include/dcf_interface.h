#ifndef DCF_INTERFACE_H
#define DCF_INTERFACE_H
#include "dcf_types.h"   /* canonical DCFCmd (DCF_CMD_UNKNOWN=0 ...) lives here */
#include "dcf_client.h"
#include "dcf_error.h"

/* DCFCmd is defined once, in dcf_types.h. The duplicate enum that used to live
 * here started at DCF_CMD_INIT=0 and silently disagreed with dcf_types.h on
 * every numeric value; any TU including both headers failed to compile, and
 * any that included only one got a different command<->int mapping. */

DCFError dcf_interface_execute(DCFClient* client, DCFCmd cmd, const char** args, int arg_count, bool json_output, char** output);
DCFError dcf_interface_tui_start(DCFClient* client);
#endif
