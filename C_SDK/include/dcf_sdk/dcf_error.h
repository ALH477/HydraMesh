/**
 * @file dcf_error.h
 * @brief DCF Error Handling and Logging Infrastructure
 * 
 * Production-grade error handling with:
 * - Thread-safe error context
 * - Structured logging with levels
 * - Error code categories
 * - Stack trace support (optional)
 */

#ifndef DCF_ERROR_H
#define DCF_ERROR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Error Code Definitions
 * ============================================================================ */

/**
 * @brief DCF Error codes organized by category
 * 
 * Error code format: 0xCCNN where CC = category, NN = specific error
 * Categories:
 *   0x00XX - Success/General
 *   0x01XX - Memory errors
 *   0x02XX - Configuration errors
 *   0x03XX - Network errors
 *   0x04XX - Serialization errors
 *   0x05XX - Plugin errors
 *   0x06XX - State errors
 *   0x07XX - Security errors
 */
typedef enum DCFError {
    /* Success */
    DCF_SUCCESS                     = 0x0000,
    
    /* Memory errors (0x01XX) */
    DCF_ERR_NULL_PTR                = 0x0100,
    DCF_ERR_MALLOC_FAIL             = 0x0101,
    DCF_ERR_BUFFER_OVERFLOW         = 0x0102,
    DCF_ERR_OUT_OF_MEMORY           = 0x0103,
    
    /* Configuration errors (0x02XX) */
    DCF_ERR_CONFIG_NOT_FOUND        = 0x0200,
    DCF_ERR_CONFIG_INVALID          = 0x0201,
    DCF_ERR_CONFIG_PARSE_FAIL       = 0x0202,
    DCF_ERR_CONFIG_UPDATE_FAIL      = 0x0203,
    DCF_ERR_CONFIG_VALIDATION_FAIL  = 0x0204,
    
    /* Network errors (0x03XX) */
    DCF_ERR_NETWORK_FAIL            = 0x0300,
    DCF_ERR_GRPC_FAIL               = 0x0301,
    DCF_ERR_CONNECTION_REFUSED      = 0x0302,
    DCF_ERR_CONNECTION_TIMEOUT      = 0x0303,
    DCF_ERR_ROUTE_NOT_FOUND         = 0x0304,
    DCF_ERR_PEER_UNREACHABLE        = 0x0305,
    DCF_ERR_NETWORK_INIT_FAIL       = 0x0306,
    
    /* Serialization errors (0x04XX) */
    DCF_ERR_SERIALIZATION_FAIL      = 0x0400,
    DCF_ERR_DESERIALIZATION_FAIL    = 0x0401,
    DCF_ERR_INVALID_MESSAGE         = 0x0402,
    DCF_ERR_MESSAGE_TOO_LARGE       = 0x0403,
    
    /* Plugin errors (0x05XX) */
    DCF_ERR_PLUGIN_FAIL             = 0x0500,
    DCF_ERR_PLUGIN_NOT_FOUND        = 0x0501,
    DCF_ERR_PLUGIN_VERSION_MISMATCH = 0x0502,
    DCF_ERR_PLUGIN_INIT_FAIL        = 0x0503,
    
    /* State errors (0x06XX) */
    DCF_ERR_INVALID_STATE           = 0x0600,
    DCF_ERR_NOT_INITIALIZED         = 0x0601,
    DCF_ERR_ALREADY_RUNNING         = 0x0602,
    DCF_ERR_NOT_RUNNING             = 0x0603,
    DCF_ERR_SHUTDOWN_IN_PROGRESS    = 0x0604,
    
    /* Security errors (0x07XX) */
    DCF_ERR_AUTH_FAIL               = 0x0700,
    DCF_ERR_PERMISSION_DENIED       = 0x0701,
    DCF_ERR_INVALID_CERTIFICATE     = 0x0702,
    
    /* Argument errors (0x08XX) */
    DCF_ERR_INVALID_ARG             = 0x0800,
    DCF_ERR_ARG_OUT_OF_RANGE        = 0x0801,
    
    /* Generic errors */
    DCF_ERR_UNKNOWN                 = 0xFFFF
} DCFError;

/* ============================================================================
 * Error Code Aliases (for API consistency)
 * ============================================================================ */

#define DCF_ERR_NULL_POINTER        DCF_ERR_NULL_PTR
#define DCF_ERR_ALLOC               DCF_ERR_MALLOC_FAIL
#define DCF_ERR_INVALID_PARAM       DCF_ERR_INVALID_ARG
#define DCF_ERR_DESERIALIZE         DCF_ERR_DESERIALIZATION_FAIL
#define DCF_ERR_PLUGIN_LOAD         DCF_ERR_PLUGIN_FAIL
#define DCF_ERR_PLUGIN_VERSION      DCF_ERR_PLUGIN_VERSION_MISMATCH
#define DCF_ERR_PLUGIN_INIT         DCF_ERR_PLUGIN_INIT_FAIL
#define DCF_ERR_GRPC_INIT           DCF_ERR_NETWORK_INIT_FAIL
#define DCF_ERR_GRPC_SERVER         DCF_ERR_GRPC_FAIL
#define DCF_ERR_GRPC_SEND           DCF_ERR_NETWORK_FAIL
#define DCF_ERR_GRPC_RECV           DCF_ERR_NETWORK_FAIL

/* ============================================================================
 * Log Level Definitions
 * ============================================================================ */

typedef enum DCFLogLevel {
    DCF_LOG_TRACE   = 0,
    DCF_LOG_DEBUG   = 1,
    DCF_LOG_INFO    = 2,
    DCF_LOG_WARN    = 3,
    DCF_LOG_ERROR   = 4,
    DCF_LOG_FATAL   = 5,
    DCF_LOG_OFF     = 6
} DCFLogLevel;

/* ============================================================================
 * Error Context Structure
 * ============================================================================ */

#define DCF_ERROR_MSG_MAX_LEN 256
#define DCF_ERROR_FILE_MAX_LEN 128
#define DCF_ERROR_FUNC_MAX_LEN 64

typedef struct DCFErrorContext {
    DCFError code;
    char message[DCF_ERROR_MSG_MAX_LEN];
    char file[DCF_ERROR_FILE_MAX_LEN];
    char function[DCF_ERROR_FUNC_MAX_LEN];
    int line;
    time_t timestamp;
    int os_errno;  /* Captured errno if applicable */
} DCFErrorContext;

/* ============================================================================
 * Logger Configuration
 * ============================================================================ */

typedef void (*DCFLogCallback)(DCFLogLevel level, const char* file, int line,
                               const char* func, const char* message, void* user_data);

typedef struct DCFLoggerConfig {
    DCFLogLevel min_level;
    bool log_to_stderr;
    bool log_to_file;
    char log_file_path[256];
    bool include_timestamp;
    bool include_source_location;
    DCFLogCallback callback;
    void* callback_user_data;
} DCFLoggerConfig;

/* ============================================================================
 * Error Functions
 * ============================================================================ */

/**
 * @brief Get human-readable error string
 */
const char* dcf_error_str(DCFError err);

/**
 * @brief Get error category name
 */
const char* dcf_error_category(DCFError err);

/**
 * @brief Check if error is in a specific category
 */
bool dcf_error_is_category(DCFError err, uint16_t category);

/**
 * @brief Set thread-local error context
 */
void dcf_set_error(DCFError code, const char* file, int line, 
                   const char* func, const char* fmt, ...);

/**
 * @brief Get last error context for current thread
 */
const DCFErrorContext* dcf_get_last_error(void);

/**
 * @brief Clear last error
 */
void dcf_clear_error(void);

/* ============================================================================
 * Logging Functions
 * ============================================================================ */

/**
 * @brief Initialize logging system
 */
DCFError dcf_log_init(const DCFLoggerConfig* config);

/**
 * @brief Shutdown logging system
 */
void dcf_log_shutdown(void);

/**
 * @brief Set minimum log level at runtime
 */
void dcf_log_set_level(DCFLogLevel level);

/**
 * @brief Get current log level
 */
DCFLogLevel dcf_log_get_level(void);

/**
 * @brief Core logging function (use macros instead)
 */
void dcf_log_write(DCFLogLevel level, const char* file, int line,
                   const char* func, const char* fmt, ...);

/* ============================================================================
 * Convenience Macros
 * ============================================================================ */

/* Error setting macro with automatic file/line/function capture */
#define DCF_SET_ERROR(code, ...) \
    dcf_set_error((code), __FILE__, __LINE__, __func__, __VA_ARGS__)

/* Return with error if condition is true */
#define DCF_RETURN_IF(cond, code, ...) \
    do { \
        if (cond) { \
            DCF_SET_ERROR(code, __VA_ARGS__); \
            return (code); \
        } \
    } while(0)

/* Return with error if pointer is NULL */
#define DCF_RETURN_IF_NULL(ptr, ...) \
    DCF_RETURN_IF((ptr) == NULL, DCF_ERR_NULL_PTR, __VA_ARGS__)

/* Logging macros with automatic context */
#define DCF_LOG_TRACE(...) \
    dcf_log_write(DCF_LOG_TRACE, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define DCF_LOG_DEBUG(...) \
    dcf_log_write(DCF_LOG_DEBUG, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define DCF_LOG_INFO(...) \
    dcf_log_write(DCF_LOG_INFO, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define DCF_LOG_WARN(...) \
    dcf_log_write(DCF_LOG_WARN, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define DCF_LOG_ERROR(...) \
    dcf_log_write(DCF_LOG_ERROR, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define DCF_LOG_FATAL(...) \
    dcf_log_write(DCF_LOG_FATAL, __FILE__, __LINE__, __func__, __VA_ARGS__)

/* Log error and set error context */
#define DCF_LOG_AND_SET_ERROR(code, ...) \
    do { \
        DCF_LOG_ERROR(__VA_ARGS__); \
        DCF_SET_ERROR(code, __VA_ARGS__); \
    } while(0)

#ifdef __cplusplus
}
#endif

#endif /* DCF_ERROR_H */
