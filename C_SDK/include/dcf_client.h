/**
 * @file dcf_client.h
 * @brief Main DCF Client Interface
 * 
 * The DCFClient is the primary entry point for applications using DCF.
 * It manages:
 * - Configuration
 * - Networking
 * - Redundancy/failover
 * - Plugin management
 * - Message sending/receiving
 */

#ifndef DCF_CLIENT_H
#define DCF_CLIENT_H

#include "dcf_types.h"
#include "dcf_error.h"
#include "dcf_config.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Client State
 * ============================================================================ */

typedef enum DCFClientState {
    DCF_CLIENT_UNINITIALIZED = 0,
    DCF_CLIENT_INITIALIZED   = 1,
    DCF_CLIENT_STARTING      = 2,
    DCF_CLIENT_RUNNING       = 3,
    DCF_CLIENT_STOPPING      = 4,
    DCF_CLIENT_STOPPED       = 5,
    DCF_CLIENT_ERROR         = 6
} DCFClientState;

/* ============================================================================
 * Client Options
 * ============================================================================ */

typedef struct DCFClientOptions {
    bool enable_auto_reconnect;
    int reconnect_interval_ms;
    int max_reconnect_attempts;
    bool enable_health_monitoring;
    int health_check_interval_ms;
    size_t receive_buffer_size;
    size_t send_buffer_size;
    int connection_timeout_ms;
    int operation_timeout_ms;
} DCFClientOptions;

/**
 * @brief Get default client options
 */
DCF_API DCFClientOptions dcf_client_options_default(void);

/* ============================================================================
 * Lifecycle Functions
 * ============================================================================ */

/**
 * @brief Create new client instance
 * @return New client or NULL on allocation failure
 */
DCF_API DCFClient* dcf_client_new(void);

/**
 * @brief Create client with options
 * @param options Client options (NULL for defaults)
 * @return New client or NULL on failure
 */
DCF_API DCFClient* dcf_client_new_with_options(const DCFClientOptions* options);

/**
 * @brief Initialize client with configuration file
 * @param client Client instance
 * @param config_path Path to configuration file
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_initialize(DCFClient* client, const char* config_path);

/**
 * @brief Initialize client with configuration object
 * @param client Client instance
 * @param config Configuration (ownership transferred to client)
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_initialize_with_config(DCFClient* client, DCFConfig* config);

/**
 * @brief Start client operations
 * @param client Client instance
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_start(DCFClient* client);

/**
 * @brief Stop client operations gracefully
 * @param client Client instance
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_stop(DCFClient* client);

/**
 * @brief Shutdown client immediately
 * @param client Client instance
 */
DCF_API void dcf_client_shutdown(DCFClient* client);

/**
 * @brief Free client and all resources
 * @param client Client instance
 */
DCF_API void dcf_client_free(DCFClient* client);

/* ============================================================================
 * State Query
 * ============================================================================ */

/**
 * @brief Get current client state
 */
DCF_API DCFClientState dcf_client_get_state(const DCFClient* client);

/**
 * @brief Check if client is running
 */
DCF_API bool dcf_client_is_running(const DCFClient* client);

/**
 * @brief Get current operating mode
 */
DCF_API DCFMode dcf_client_get_mode(const DCFClient* client);

/**
 * @brief Get client configuration (read-only)
 */
DCF_API const DCFConfig* dcf_client_get_config(const DCFClient* client);

/* ============================================================================
 * Messaging
 * ============================================================================ */

/**
 * @brief Send message to recipient
 * @param client Client instance
 * @param data Message data
 * @param recipient Recipient identifier
 * @param response_out Response from recipient (caller must free, may be NULL)
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_send_message(DCFClient* client, const char* data,
                                          const char* recipient, char** response_out);

/**
 * @brief Send binary message
 * @param client Client instance
 * @param data Binary data
 * @param data_len Data length
 * @param recipient Recipient identifier
 * @param response_out Response data (caller must free)
 * @param response_len_out Response length
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_send_binary(DCFClient* client, const uint8_t* data,
                                         size_t data_len, const char* recipient,
                                         uint8_t** response_out, size_t* response_len_out);

/**
 * @brief Send message asynchronously
 * @param client Client instance
 * @param data Message data
 * @param recipient Recipient identifier
 * @return DCF_SUCCESS if message queued, error code otherwise
 */
DCF_API DCFError dcf_client_send_async(DCFClient* client, const char* data,
                                        const char* recipient);

/**
 * @brief Receive message (blocking)
 * @param client Client instance
 * @param message_out Received message (caller must free)
 * @param sender_out Sender identifier (caller must free)
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_receive_message(DCFClient* client, char** message_out,
                                             char** sender_out);

/**
 * @brief Receive message with timeout
 * @param client Client instance
 * @param message_out Received message (caller must free)
 * @param sender_out Sender identifier (caller must free)
 * @param timeout_ms Timeout in milliseconds (-1 for infinite)
 * @return DCF_SUCCESS, DCF_ERR_CONNECTION_TIMEOUT, or other error
 */
DCF_API DCFError dcf_client_receive_timeout(DCFClient* client, char** message_out,
                                             char** sender_out, int timeout_ms);

/**
 * @brief Register message callback for async receiving
 * @param client Client instance
 * @param callback Message callback function
 * @param user_data User context
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_set_message_callback(DCFClient* client,
                                                  DCFMessageCallback callback,
                                                  void* user_data);

/* ============================================================================
 * Mode Management
 * ============================================================================ */

/**
 * @brief Set operating mode
 * @param client Client instance
 * @param mode New mode
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_set_mode(DCFClient* client, DCFMode mode);

/* ============================================================================
 * Logging Configuration
 * ============================================================================ */

/**
 * @brief Set client log level
 * @param client Client instance
 * @param level Log level
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_client_set_log_level(DCFClient* client, DCFLogLevel level);

/**
 * @brief Get current log level
 */
DCF_API DCFLogLevel dcf_client_get_log_level(const DCFClient* client);

/* ============================================================================
 * Component Access (Advanced)
 * ============================================================================ */

/**
 * @brief Get redundancy manager (for direct health checks, etc.)
 * @note Returned pointer is owned by client; do not free
 */
DCF_API DCFRedundancy* dcf_client_get_redundancy(DCFClient* client);

/**
 * @brief Get plugin manager
 * @note Returned pointer is owned by client; do not free
 */
DCF_API DCFPluginManager* dcf_client_get_plugin_manager(DCFClient* client);

/**
 * @brief Get networking component
 * @note Returned pointer is owned by client; do not free
 */
DCF_API DCFNetworking* dcf_client_get_networking(DCFClient* client);

/* ============================================================================
 * Statistics
 * ============================================================================ */

typedef struct DCFClientStats {
    uint64_t messages_sent;
    uint64_t messages_received;
    uint64_t bytes_sent;
    uint64_t bytes_received;
    uint64_t errors;
    uint64_t reconnections;
    time_t start_time;
    time_t last_message_time;
} DCFClientStats;

/**
 * @brief Get client statistics
 */
DCF_API DCFError dcf_client_get_stats(const DCFClient* client, DCFClientStats* stats_out);

/**
 * @brief Reset client statistics
 */
DCF_API DCFError dcf_client_reset_stats(DCFClient* client);

#ifdef __cplusplus
}
#endif

#endif /* DCF_CLIENT_H */
