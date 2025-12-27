/**
 * @file dcf_config.h
 * @brief Configuration management for DCF
 * 
 * Features:
 * - Thread-safe configuration access
 * - Validation on load and update
 * - Configuration change notifications
 * - Environment variable overrides
 */

#ifndef DCF_CONFIG_H
#define DCF_CONFIG_H

#include "dcf_types.h"
#include "dcf_error.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Validation
 * ============================================================================ */

typedef struct DCFConfigValidation {
    bool valid;
    char error_message[256];
    int error_count;
} DCFConfigValidation;

/* ============================================================================
 * Configuration Change Callback
 * ============================================================================ */

typedef void (*DCFConfigChangeCallback)(const char* key, const char* old_value,
                                        const char* new_value, void* user_data);

/* ============================================================================
 * Lifecycle Functions
 * ============================================================================ */

/**
 * @brief Load configuration from JSON file
 * @param path Path to configuration file
 * @return Allocated config or NULL on failure
 */
DCF_API DCFConfig* dcf_config_load(const char* path);

/**
 * @brief Load configuration from JSON string
 * @param json_str JSON configuration string
 * @return Allocated config or NULL on failure
 */
DCF_API DCFConfig* dcf_config_load_from_string(const char* json_str);

/**
 * @brief Create default configuration
 * @return Allocated config with default values
 */
DCF_API DCFConfig* dcf_config_create_default(void);

/**
 * @brief Deep copy configuration
 * @param config Source configuration
 * @return New allocated copy or NULL on failure
 */
DCF_API DCFConfig* dcf_config_clone(const DCFConfig* config);

/**
 * @brief Save configuration to file
 * @param config Configuration to save
 * @param path Output file path
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_config_save(const DCFConfig* config, const char* path);

/**
 * @brief Free configuration resources
 * @param config Configuration to free
 */
DCF_API void dcf_config_free(DCFConfig* config);

/* ============================================================================
 * Validation
 * ============================================================================ */

/**
 * @brief Validate configuration
 * @param config Configuration to validate
 * @param result Validation result output
 * @return DCF_SUCCESS if valid, error code otherwise
 */
DCF_API DCFError dcf_config_validate(const DCFConfig* config, 
                                      DCFConfigValidation* result);

/* ============================================================================
 * Getters (Thread-Safe)
 * ============================================================================ */

/**
 * @brief Get operating mode
 */
DCF_API DCFError dcf_config_get_mode(const DCFConfig* config, DCFMode* mode_out);

/**
 * @brief Get node ID (caller must free result)
 */
DCF_API DCFError dcf_config_get_node_id(const DCFConfig* config, char** node_id_out);

/**
 * @brief Get host (caller must free result)
 */
DCF_API DCFError dcf_config_get_host(const DCFConfig* config, char** host_out);

/**
 * @brief Get port
 */
DCF_API int dcf_config_get_port(const DCFConfig* config);

/**
 * @brief Get RTT threshold in milliseconds
 */
DCF_API int dcf_config_get_rtt_threshold(const DCFConfig* config);

/**
 * @brief Get peer list (caller must free peers and each peer string)
 * @param config Configuration
 * @param peers_out Array of peer strings
 * @param count_out Number of peers
 */
DCF_API DCFError dcf_config_get_peers(const DCFConfig* config, 
                                       char*** peers_out, size_t* count_out);

/**
 * @brief Get plugin path (caller must free result)
 */
DCF_API DCFError dcf_config_get_plugin_path(const DCFConfig* config, char** path_out);

/**
 * @brief Get TLS enabled status
 */
DCF_API bool dcf_config_get_tls_enabled(const DCFConfig* config);

/**
 * @brief Get TLS certificate path (caller must free result)
 */
DCF_API DCFError dcf_config_get_tls_cert_path(const DCFConfig* config, char** path_out);

/**
 * @brief Get TLS key path (caller must free result)
 */
DCF_API DCFError dcf_config_get_tls_key_path(const DCFConfig* config, char** path_out);

/* ============================================================================
 * Setters (Thread-Safe)
 * ============================================================================ */

/**
 * @brief Update configuration value
 * @param config Configuration to update
 * @param key Configuration key
 * @param value New value as string
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_config_update(DCFConfig* config, const char* key, 
                                    const char* value);

/**
 * @brief Set operating mode
 */
DCF_API DCFError dcf_config_set_mode(DCFConfig* config, DCFMode mode);

/**
 * @brief Set node ID
 */
DCF_API DCFError dcf_config_set_node_id(DCFConfig* config, const char* node_id);

/**
 * @brief Set host
 */
DCF_API DCFError dcf_config_set_host(DCFConfig* config, const char* host);

/**
 * @brief Set port
 */
DCF_API DCFError dcf_config_set_port(DCFConfig* config, int port);

/**
 * @brief Add peer to configuration
 */
DCF_API DCFError dcf_config_add_peer(DCFConfig* config, const char* peer);

/**
 * @brief Remove peer from configuration
 */
DCF_API DCFError dcf_config_remove_peer(DCFConfig* config, const char* peer);

/* ============================================================================
 * Change Notifications
 * ============================================================================ */

/**
 * @brief Register configuration change callback
 * @param config Configuration
 * @param callback Callback function
 * @param user_data User context
 * @return Registration ID or -1 on failure
 */
DCF_API int dcf_config_register_callback(DCFConfig* config,
                                          DCFConfigChangeCallback callback,
                                          void* user_data);

/**
 * @brief Unregister configuration change callback
 */
DCF_API DCFError dcf_config_unregister_callback(DCFConfig* config, int callback_id);

/* ============================================================================
 * Environment Variable Support
 * ============================================================================ */

/**
 * @brief Apply environment variable overrides
 * 
 * Supported variables:
 *   DCF_MODE, DCF_HOST, DCF_PORT, DCF_NODE_ID, etc.
 */
DCF_API DCFError dcf_config_apply_env_overrides(DCFConfig* config);

#ifdef __cplusplus
}
#endif

#endif /* DCF_CONFIG_H */
