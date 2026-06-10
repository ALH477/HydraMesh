/**
 * @file dcf_networking.h
 * @brief DCF Networking - Thread-safe network communication layer
 * @version 5.1.0
 *
 * Provides gRPC-based network communication with proper lifecycle
 * management, timeout support, and thread-safe operations.
 */

#ifndef DCF_NETWORKING_H
#define DCF_NETWORKING_H

#include "dcf_types.h"
#include "dcf_error.h"
#include "dcf_config.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Lifecycle Functions
 * ============================================================================ */

/**
 * @brief Create a new networking instance
 * @return New networking instance or NULL on failure
 */
DCFNetworking* dcf_networking_new(void);

/**
 * @brief Initialize networking with configuration
 * @param net Networking instance
 * @param config Configuration to use
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_initialize(DCFNetworking* net, DCFConfig* config);

/**
 * @brief Start networking operations
 * @param net Networking instance
 * @param mode Operating mode (affects server startup)
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_start(DCFNetworking* net, DCFMode mode);

/**
 * @brief Stop networking operations
 * @param net Networking instance
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_stop(DCFNetworking* net);

/**
 * @brief Free networking instance and all resources
 * @param net Networking instance to free
 */
void dcf_networking_free(DCFNetworking* net);

/* ============================================================================
 * Send Operations
 * ============================================================================ */

/**
 * @brief Send data to a recipient
 * @param net Networking instance
 * @param data Data to send
 * @param len Length of data
 * @param recipient Target address
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_send(DCFNetworking* net, const uint8_t* data,
                              size_t len, const char* recipient);

/**
 * @brief Send data with timeout
 * @param net Networking instance
 * @param data Data to send
 * @param len Length of data
 * @param recipient Target address
 * @param timeout_ms Timeout in milliseconds
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_send_with_timeout(DCFNetworking* net, const uint8_t* data,
                                           size_t len, const char* recipient,
                                           int timeout_ms);

/* ============================================================================
 * Receive Operations
 * ============================================================================ */

/**
 * @brief Receive a message
 * @param net Networking instance
 * @param message_out Output for message data (allocated, caller frees)
 * @param sender_out Output for sender address (allocated, caller frees, may be NULL)
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_receive(DCFNetworking* net, char** message_out,
                                 char** sender_out);

/**
 * @brief Receive a message with timeout
 * @param net Networking instance
 * @param message_out Output for message data (allocated, caller frees)
 * @param sender_out Output for sender address (allocated, caller frees, may be NULL)
 * @param timeout_ms Timeout in milliseconds
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_receive_with_timeout(DCFNetworking* net, char** message_out,
                                              char** sender_out, int timeout_ms);

/* ============================================================================
 * Status and Statistics
 * ============================================================================ */

/**
 * @brief Check if networking is running
 * @param net Networking instance
 * @return true if running, false otherwise
 */
bool dcf_networking_is_running(DCFNetworking* net);

/**
 * @brief Get current operating mode
 * @param net Networking instance
 * @return Current mode
 */
DCFMode dcf_networking_get_mode(DCFNetworking* net);

/**
 * @brief Get networking statistics
 * @param net Networking instance
 * @param sent Output for messages sent (may be NULL)
 * @param received Output for messages received (may be NULL)
 * @param bytes_sent Output for bytes sent (may be NULL)
 * @param bytes_received Output for bytes received (may be NULL)
 * @param errors Output for total errors (may be NULL)
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_networking_get_stats(DCFNetworking* net,
                                   uint64_t* sent, uint64_t* received,
                                   uint64_t* bytes_sent, uint64_t* bytes_received,
                                   uint64_t* errors);

#ifdef __cplusplus
}
#endif

#endif /* DCF_NETWORKING_H */
