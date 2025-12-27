/**
 * @file dcf_error.h
 * @brief Production Error Handling and Logging Infrastructure
 * @version 5.2.0
 *
 * Features:
 * - Thread-safe error context with stack traces
 * - Structured logging with rotation
 * - Error categorization and filtering
 * - Crash recovery hooks
 * - Performance metrics integration
 */

#ifndef DCF_ERROR_H
#define DCF_ERROR_H

#include "dcf_platform.h"
#include "dcf_types.h"
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
 *   0x08XX - Argument errors
 *   0x09XX - I/O errors
 *   0x0AXX - Timeout errors
 *   0x0BXX - Resource errors
 *   0x0CXX - Protocol errors
 */
typedef enum DCFError {
    /* Success (0x00XX) */
    DCF_SUCCESS                     = 0x0000,
    DCF_PENDING                     = 0x0001,  /**< Operation in progress */
    DCF_WOULD_BLOCK                 = 0x0002,  /**< Non-blocking op would block */
    DCF_CANCELLED                   = 0x0003,  /**< Operation was cancelled */
    DCF_EOF                         = 0x0004,  /**< End of stream/data */
    
    /* Memory errors (0x01XX) */
    DCF_ERR_NULL_PTR                = 0x0100,
    DCF_ERR_MALLOC_FAIL             = 0x0101,
    DCF_ERR_BUFFER_OVERFLOW         = 0x0102,
    DCF_ERR_OUT_OF_MEMORY           = 0x0103,
    DCF_ERR_BUFFER_TOO_SMALL        = 0x0104,
    DCF_ERR_ALIGNMENT               = 0x0105,
    DCF_ERR_DOUBLE_FREE             = 0x0106,
    DCF_ERR_USE_AFTER_FREE          = 0x0107,
    
    /* Configuration errors (0x02XX) */
    DCF_ERR_CONFIG_NOT_FOUND        = 0x0200,
    DCF_ERR_CONFIG_INVALID          = 0x0201,
    DCF_ERR_CONFIG_PARSE_FAIL       = 0x0202,
    DCF_ERR_CONFIG_UPDATE_FAIL      = 0x0203,
    DCF_ERR_CONFIG_VALIDATION_FAIL  = 0x0204,
    DCF_ERR_CONFIG_READ_ONLY        = 0x0205,
    DCF_ERR_CONFIG_TYPE_MISMATCH    = 0x0206,
    DCF_ERR_CONFIG_KEY_NOT_FOUND    = 0x0207,
    
    /* Network errors (0x03XX) */
    DCF_ERR_NETWORK_FAIL            = 0x0300,
    DCF_ERR_GRPC_FAIL               = 0x0301,
    DCF_ERR_CONNECTION_REFUSED      = 0x0302,
    DCF_ERR_CONNECTION_TIMEOUT      = 0x0303,
    DCF_ERR_ROUTE_NOT_FOUND         = 0x0304,
    DCF_ERR_PEER_UNREACHABLE        = 0x0305,
    DCF_ERR_NETWORK_INIT_FAIL       = 0x0306,
    DCF_ERR_DNS_FAIL                = 0x0307,
    DCF_ERR_BIND_FAIL               = 0x0308,
    DCF_ERR_LISTEN_FAIL             = 0x0309,
    DCF_ERR_ACCEPT_FAIL             = 0x030A,
    DCF_ERR_CONNECT_FAIL            = 0x030B,
    DCF_ERR_SOCKET_CLOSED           = 0x030C,
    DCF_ERR_NETWORK_RESET           = 0x030D,
    DCF_ERR_HOST_UNREACHABLE        = 0x030E,
    DCF_ERR_NETWORK_UNREACHABLE     = 0x030F,
    DCF_ERR_CONNECTION_ABORTED      = 0x0310,
    DCF_ERR_TOO_MANY_CONNECTIONS    = 0x0311,
    DCF_ERR_ADDRESS_IN_USE          = 0x0312,
    
    /* Serialization errors (0x04XX) */
    DCF_ERR_SERIALIZATION_FAIL      = 0x0400,
    DCF_ERR_DESERIALIZATION_FAIL    = 0x0401,
    DCF_ERR_INVALID_MESSAGE         = 0x0402,
    DCF_ERR_MESSAGE_TOO_LARGE       = 0x0403,
    DCF_ERR_MESSAGE_CORRUPTED       = 0x0404,
    DCF_ERR_CRC_MISMATCH            = 0x0405,
    DCF_ERR_VERSION_MISMATCH        = 0x0406,
    DCF_ERR_ENCODING_FAIL           = 0x0407,
    DCF_ERR_DECODING_FAIL           = 0x0408,
    DCF_ERR_TRUNCATED_MESSAGE       = 0x0409,
    
    /* Plugin errors (0x05XX) */
    DCF_ERR_PLUGIN_FAIL             = 0x0500,
    DCF_ERR_PLUGIN_NOT_FOUND        = 0x0501,
    DCF_ERR_PLUGIN_VERSION_MISMATCH = 0x0502,
    DCF_ERR_PLUGIN_INIT_FAIL        = 0x0503,
    DCF_ERR_PLUGIN_SYMBOL_NOT_FOUND = 0x0504,
    DCF_ERR_PLUGIN_ALREADY_LOADED   = 0x0505,
    DCF_ERR_PLUGIN_DEPENDENCY       = 0x0506,
    DCF_ERR_PLUGIN_INCOMPATIBLE     = 0x0507,
    
    /* State errors (0x06XX) */
    DCF_ERR_INVALID_STATE           = 0x0600,
    DCF_ERR_NOT_INITIALIZED         = 0x0601,
    DCF_ERR_ALREADY_RUNNING         = 0x0602,
    DCF_ERR_NOT_RUNNING             = 0x0603,
    DCF_ERR_SHUTDOWN_IN_PROGRESS    = 0x0604,
    DCF_ERR_ALREADY_INITIALIZED     = 0x0605,
    DCF_ERR_BUSY                    = 0x0606,
    DCF_ERR_DRAINING                = 0x0607,
    DCF_ERR_CIRCUIT_OPEN            = 0x0608,
    
    /* Security errors (0x07XX) */
    DCF_ERR_AUTH_FAIL               = 0x0700,
    DCF_ERR_PERMISSION_DENIED       = 0x0701,
    DCF_ERR_INVALID_CERTIFICATE     = 0x0702,
    DCF_ERR_CERTIFICATE_EXPIRED     = 0x0703,
    DCF_ERR_TLS_HANDSHAKE_FAIL      = 0x0704,
    DCF_ERR_ENCRYPTION_FAIL         = 0x0705,
    DCF_ERR_DECRYPTION_FAIL         = 0x0706,
    DCF_ERR_SIGNATURE_INVALID       = 0x0707,
    DCF_ERR_KEY_EXCHANGE_FAIL       = 0x0708,
    DCF_ERR_RATE_LIMITED            = 0x0709,
    DCF_ERR_BLACKLISTED             = 0x070A,
    
    /* Argument errors (0x08XX) */
    DCF_ERR_INVALID_ARG             = 0x0800,
    DCF_ERR_ARG_OUT_OF_RANGE        = 0x0801,
    DCF_ERR_INVALID_FORMAT          = 0x0802,
    DCF_ERR_INVALID_LENGTH          = 0x0803,
    DCF_ERR_INVALID_TYPE            = 0x0804,
    DCF_ERR_INVALID_VALUE           = 0x0805,
    DCF_ERR_MISSING_ARG             = 0x0806,
    
    /* I/O errors (0x09XX) */
    DCF_ERR_IO_FAIL                 = 0x0900,
    DCF_ERR_FILE_NOT_FOUND          = 0x0901,
    DCF_ERR_FILE_EXISTS             = 0x0902,
    DCF_ERR_FILE_TOO_LARGE          = 0x0903,
    DCF_ERR_DISK_FULL               = 0x0904,
    DCF_ERR_READ_FAIL               = 0x0905,
    DCF_ERR_WRITE_FAIL              = 0x0906,
    DCF_ERR_SEEK_FAIL               = 0x0907,
    DCF_ERR_PIPE_BROKEN             = 0x0908,
    
    /* Timeout errors (0x0AXX) */
    DCF_ERR_TIMEOUT                 = 0x0A00,
    DCF_ERR_CONNECT_TIMEOUT         = 0x0A01,
    DCF_ERR_READ_TIMEOUT            = 0x0A02,
    DCF_ERR_WRITE_TIMEOUT           = 0x0A03,
    DCF_ERR_OPERATION_TIMEOUT       = 0x0A04,
    DCF_ERR_LOCK_TIMEOUT            = 0x0A05,
    DCF_ERR_HEALTH_TIMEOUT          = 0x0A06,
    
    /* Resource errors (0x0BXX) */
    DCF_ERR_RESOURCE_EXHAUSTED      = 0x0B00,
    DCF_ERR_TOO_MANY_OPEN_FILES     = 0x0B01,
    DCF_ERR_QUEUE_FULL              = 0x0B02,
    DCF_ERR_POOL_EXHAUSTED          = 0x0B03,
    DCF_ERR_LIMIT_EXCEEDED          = 0x0B04,
    DCF_ERR_QUOTA_EXCEEDED          = 0x0B05,
    DCF_ERR_THREAD_CREATE_FAIL      = 0x0B06,
    
    /* Protocol errors (0x0CXX) */
    DCF_ERR_PROTOCOL                = 0x0C00,
    DCF_ERR_PROTOCOL_VERSION        = 0x0C01,
    DCF_ERR_UNEXPECTED_MESSAGE      = 0x0C02,
    DCF_ERR_SEQUENCE_ERROR          = 0x0C03,
    DCF_ERR_HANDSHAKE_FAIL          = 0x0C04,
    DCF_ERR_KEEPALIVE_FAIL          = 0x0C05,
    
    /* Generic/Unknown */
    DCF_ERR_UNKNOWN                 = 0xFFFF
} DCFError;

/* Error category extraction */
#define DCF_ERROR_CATEGORY(err)     (((err) >> 8) & 0xFF)
#define DCF_ERROR_SPECIFIC(err)     ((err) & 0xFF)
#define DCF_MAKE_ERROR(cat, spec)   (((cat) << 8) | (spec))

/* Category constants */
#define DCF_ERROR_CAT_SUCCESS       0x00
#define DCF_ERROR_CAT_MEMORY        0x01
#define DCF_ERROR_CAT_CONFIG        0x02
#define DCF_ERROR_CAT_NETWORK       0x03
#define DCF_ERROR_CAT_SERIAL        0x04
#define DCF_ERROR_CAT_PLUGIN        0x05
#define DCF_ERROR_CAT_STATE         0x06
#define DCF_ERROR_CAT_SECURITY      0x07
#define DCF_ERROR_CAT_ARGUMENT      0x08
#define DCF_ERROR_CAT_IO            0x09
#define DCF_ERROR_CAT_TIMEOUT       0x0A
#define DCF_ERROR_CAT_RESOURCE      0x0B
#define DCF_ERROR_CAT_PROTOCOL      0x0C

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

#define DCF_MAX_STACK_FRAMES    32
#define DCF_MAX_ERROR_CHAIN     8

typedef struct DCFStackFrame {
    const char* function;
    const char* file;
    int line;
    void* address;
} DCFStackFrame;

typedef struct DCFErrorContext {
    DCFError code;
    char message[DCF_MAX_ERROR_MSG_LEN];
    char file[128];
    char function[128];
    int line;
    uint64_t timestamp;
    int os_errno;
    dcf_thread_id_t thread_id;
    
    /* Stack trace */
    DCFStackFrame stack[DCF_MAX_STACK_FRAMES];
    int stack_depth;
    
    /* Error chain for wrapped errors */
    struct {
        DCFError code;
        char message[256];
    } chain[DCF_MAX_ERROR_CHAIN];
    int chain_depth;
} DCFErrorContext;

/* ============================================================================
 * Logger Configuration
 * ============================================================================ */

typedef void (*DCFLogCallback)(
    DCFLogLevel level, 
    const char* file, 
    int line,
    const char* func, 
    const char* message, 
    void* user_data
);

typedef struct DCFLoggerConfig {
    DCFLogLevel min_level;
    DCFLogLevel min_file_level;      /**< Minimum level for file logging */
    
    /* Output destinations */
    bool log_to_stderr;
    bool log_to_file;
    bool log_to_syslog;
    bool use_colors;                 /**< ANSI colors for terminal */
    
    /* File logging */
    char log_file_path[DCF_MAX_PATH_LEN];
    size_t max_file_size;            /**< Max size before rotation */
    int max_file_count;              /**< Number of rotated files to keep */
    bool async_file_write;           /**< Use background thread for I/O */
    
    /* Format options */
    bool include_timestamp;
    bool include_source_location;
    bool include_thread_id;
    bool include_level;
    bool include_function;
    char timestamp_format[64];       /**< strftime format string */
    
    /* Custom callback */
    DCFLogCallback callback;
    void* callback_user_data;
} DCFLoggerConfig;

#define DCF_LOGGER_CONFIG_DEFAULT {                          \
    .min_level = DCF_LOG_INFO,                               \
    .min_file_level = DCF_LOG_DEBUG,                         \
    .log_to_stderr = true,                                   \
    .log_to_file = false,                                    \
    .log_to_syslog = false,                                  \
    .use_colors = true,                                      \
    .log_file_path = "",                                     \
    .max_file_size = 10 * 1024 * 1024,                       \
    .max_file_count = 5,                                     \
    .async_file_write = true,                                \
    .include_timestamp = true,                               \
    .include_source_location = true,                         \
    .include_thread_id = false,                              \
    .include_level = true,                                   \
    .include_function = true,                                \
    .timestamp_format = "%Y-%m-%d %H:%M:%S",                 \
    .callback = NULL,                                        \
    .callback_user_data = NULL                               \
}

/* ============================================================================
 * Error Functions
 * ============================================================================ */

/**
 * @brief Get human-readable error string
 */
DCF_API const char* dcf_error_str(DCFError err);

/**
 * @brief Get error category name
 */
DCF_API const char* dcf_error_category_str(DCFError err);

/**
 * @brief Check if error is in a specific category
 */
DCF_API DCF_PURE bool dcf_error_is_category(DCFError err, uint8_t category);

/**
 * @brief Check if error is retriable
 */
DCF_API DCF_PURE bool dcf_error_is_retriable(DCFError err);

/**
 * @brief Check if error is transient (may resolve on its own)
 */
DCF_API DCF_PURE bool dcf_error_is_transient(DCFError err);

/**
 * @brief Convert OS errno to DCFError
 */
DCF_API DCFError dcf_error_from_errno(int err_no);

/**
 * @brief Set thread-local error context
 */
DCF_API void dcf_set_error_v(DCFError code, const char* file, int line,
                             const char* func, const char* fmt, va_list args);

DCF_API void dcf_set_error(DCFError code, const char* file, int line,
                           const char* func, const char* fmt, ...) DCF_PRINTF(5, 6);

/**
 * @brief Wrap an existing error with additional context
 */
DCF_API void dcf_wrap_error(DCFError new_code, const char* file, int line,
                            const char* func, const char* fmt, ...) DCF_PRINTF(5, 6);

/**
 * @brief Get last error context for current thread
 */
DCF_API const DCFErrorContext* dcf_get_last_error(void);

/**
 * @brief Clear last error
 */
DCF_API void dcf_clear_error(void);

/**
 * @brief Check if there is an error set
 */
DCF_API bool dcf_has_error(void);

/**
 * @brief Format error context as string
 */
DCF_API int dcf_error_format(const DCFErrorContext* ctx, char* buf, size_t size);

/**
 * @brief Capture current stack trace
 */
DCF_API int dcf_capture_stack_trace(DCFStackFrame* frames, int max_frames, int skip);

/* ============================================================================
 * Logging Functions
 * ============================================================================ */

/**
 * @brief Initialize logging system
 */
DCF_API DCFError dcf_log_init(const DCFLoggerConfig* config);

/**
 * @brief Shutdown logging system (flushes pending writes)
 */
DCF_API void dcf_log_shutdown(void);

/**
 * @brief Flush log buffers
 */
DCF_API void dcf_log_flush(void);

/**
 * @brief Set minimum log level at runtime
 */
DCF_API void dcf_log_set_level(DCFLogLevel level);

/**
 * @brief Get current log level
 */
DCF_API DCFLogLevel dcf_log_get_level(void);

/**
 * @brief Get log level from string
 */
DCF_API DCFLogLevel dcf_log_level_from_string(const char* str);

/**
 * @brief Get log level name
 */
DCF_API const char* dcf_log_level_str(DCFLogLevel level);

/**
 * @brief Core logging function (use macros instead)
 */
DCF_API void dcf_log_write(DCFLogLevel level, const char* file, int line,
                           const char* func, const char* fmt, ...) DCF_PRINTF(5, 6);

DCF_API void dcf_log_write_v(DCFLogLevel level, const char* file, int line,
                             const char* func, const char* fmt, va_list args);

/**
 * @brief Check if log level is enabled (for expensive log generation)
 */
DCF_API bool dcf_log_is_enabled(DCFLogLevel level);

/**
 * @brief Set log file path (triggers rotation if needed)
 */
DCF_API DCFError dcf_log_set_file(const char* path);

/**
 * @brief Rotate log files now
 */
DCF_API DCFError dcf_log_rotate(void);

/**
 * @brief Log statistics
 */
typedef struct DCFLogStats {
    uint64_t messages_logged[DCF_LOG_OFF];
    uint64_t messages_dropped;
    uint64_t bytes_written;
    uint64_t rotations;
    uint64_t errors;
} DCFLogStats;

DCF_API void dcf_log_get_stats(DCFLogStats* stats);

/* ============================================================================
 * Crash Recovery
 * ============================================================================ */

typedef void (*DCFCrashCallback)(int signal, void* user_data);

/**
 * @brief Install crash handlers (SIGSEGV, SIGABRT, etc.)
 */
DCF_API DCFError dcf_crash_handler_install(void);

/**
 * @brief Uninstall crash handlers
 */
DCF_API void dcf_crash_handler_uninstall(void);

/**
 * @brief Register crash callback
 */
DCF_API void dcf_crash_handler_set_callback(DCFCrashCallback callback, void* user_data);

/**
 * @brief Set crash log file
 */
DCF_API void dcf_crash_handler_set_log_file(const char* path);

/* ============================================================================
 * Convenience Macros
 * ============================================================================ */

/* Error setting macro with automatic file/line/function capture */
#define DCF_SET_ERROR(code, ...) \
    dcf_set_error((code), __FILE__, __LINE__, __func__, __VA_ARGS__)

/* Wrap existing error */
#define DCF_WRAP_ERROR(code, ...) \
    dcf_wrap_error((code), __FILE__, __LINE__, __func__, __VA_ARGS__)

/* Return with error if condition is true */
#define DCF_RETURN_IF(cond, code, ...) \
    do { \
        if (DCF_UNLIKELY(cond)) { \
            DCF_SET_ERROR(code, __VA_ARGS__); \
            return (code); \
        } \
    } while(0)

/* Return with error if pointer is NULL */
#define DCF_RETURN_IF_NULL(ptr, ...) \
    DCF_RETURN_IF((ptr) == NULL, DCF_ERR_NULL_PTR, __VA_ARGS__)

/* Return if error (non-success) */
#define DCF_RETURN_IF_ERROR(expr) \
    do { \
        DCFError _err = (expr); \
        if (DCF_UNLIKELY(_err != DCF_SUCCESS)) { \
            return _err; \
        } \
    } while(0)

/* Goto cleanup label if error */
#define DCF_GOTO_IF_ERROR(expr, label) \
    do { \
        DCFError _err = (expr); \
        if (DCF_UNLIKELY(_err != DCF_SUCCESS)) { \
            goto label; \
        } \
    } while(0)

/* Assert with logging */
#define DCF_ASSERT(cond, ...) \
    do { \
        if (DCF_UNLIKELY(!(cond))) { \
            DCF_LOG_FATAL("Assertion failed: " #cond " - " __VA_ARGS__); \
            dcf_crash_handler_uninstall(); \
            abort(); \
        } \
    } while(0)

/* Debug-only assertion */
#ifdef NDEBUG
    #define DCF_DEBUG_ASSERT(cond, ...) ((void)0)
#else
    #define DCF_DEBUG_ASSERT(cond, ...) DCF_ASSERT(cond, __VA_ARGS__)
#endif

/* Logging macros with automatic context */
#define DCF_LOG_TRACE(...) \
    do { if (dcf_log_is_enabled(DCF_LOG_TRACE)) \
        dcf_log_write(DCF_LOG_TRACE, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    } while(0)

#define DCF_LOG_DEBUG(...) \
    do { if (dcf_log_is_enabled(DCF_LOG_DEBUG)) \
        dcf_log_write(DCF_LOG_DEBUG, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    } while(0)

#define DCF_LOG_INFO(...) \
    do { if (dcf_log_is_enabled(DCF_LOG_INFO)) \
        dcf_log_write(DCF_LOG_INFO, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    } while(0)

#define DCF_LOG_WARN(...) \
    do { if (dcf_log_is_enabled(DCF_LOG_WARN)) \
        dcf_log_write(DCF_LOG_WARN, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    } while(0)

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

/* Conditional logging (avoids evaluation if level not enabled) */
#define DCF_LOG_IF(level, cond, ...) \
    do { if ((cond) && dcf_log_is_enabled(level)) \
        dcf_log_write(level, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    } while(0)

/* Rate-limited logging (log at most once per N calls) */
#define DCF_LOG_EVERY_N(level, n, ...) \
    do { \
        static dcf_atomic_uint _count = 0; \
        if (dcf_atomic_fetch_add(&_count, 1) % (n) == 0) \
            dcf_log_write(level, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    } while(0)

/* Log first N occurrences only */
#define DCF_LOG_FIRST_N(level, n, ...) \
    do { \
        static dcf_atomic_uint _count = 0; \
        if (dcf_atomic_fetch_add(&_count, 1) < (n)) \
            dcf_log_write(level, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    } while(0)

#ifdef __cplusplus
}
#endif

#endif /* DCF_ERROR_H */
