/**
 * @file dcf_error.c
 * @brief Error handling and logging implementation
 */

#include "dcf_error.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>

/* ============================================================================
 * Thread-Local Error Context
 * ============================================================================ */

static DCF_THREAD_LOCAL DCFErrorContext g_last_error = {0};

/* ============================================================================
 * Logger State
 * ============================================================================ */

static struct {
    DCFLoggerConfig config;
    FILE* log_file;
    pthread_mutex_t mutex;
    bool initialized;
} g_logger = {
    .config = {
        .min_level = DCF_LOG_INFO,
        .log_to_stderr = true,
        .log_to_file = false,
        .include_timestamp = true,
        .include_source_location = true
    },
    .log_file = NULL,
    .initialized = false
};

/* ============================================================================
 * Error String Mappings
 * ============================================================================ */

typedef struct {
    DCFError code;
    const char* name;
    const char* description;
} DCFErrorInfo;

static const DCFErrorInfo error_table[] = {
    /* Success */
    {DCF_SUCCESS,                     "SUCCESS",                     "Success"},
    
    /* Memory errors */
    {DCF_ERR_NULL_PTR,                "NULL_PTR",                    "Null pointer error"},
    {DCF_ERR_MALLOC_FAIL,             "MALLOC_FAIL",                 "Memory allocation failed"},
    {DCF_ERR_BUFFER_OVERFLOW,         "BUFFER_OVERFLOW",             "Buffer overflow detected"},
    {DCF_ERR_OUT_OF_MEMORY,           "OUT_OF_MEMORY",               "Out of memory"},
    
    /* Configuration errors */
    {DCF_ERR_CONFIG_NOT_FOUND,        "CONFIG_NOT_FOUND",            "Configuration file not found"},
    {DCF_ERR_CONFIG_INVALID,          "CONFIG_INVALID",              "Invalid configuration"},
    {DCF_ERR_CONFIG_PARSE_FAIL,       "CONFIG_PARSE_FAIL",           "Configuration parse failed"},
    {DCF_ERR_CONFIG_UPDATE_FAIL,      "CONFIG_UPDATE_FAIL",          "Configuration update failed"},
    {DCF_ERR_CONFIG_VALIDATION_FAIL,  "CONFIG_VALIDATION_FAIL",      "Configuration validation failed"},
    
    /* Network errors */
    {DCF_ERR_NETWORK_FAIL,            "NETWORK_FAIL",                "Network failure"},
    {DCF_ERR_GRPC_FAIL,               "GRPC_FAIL",                   "gRPC operation failed"},
    {DCF_ERR_CONNECTION_REFUSED,      "CONNECTION_REFUSED",          "Connection refused"},
    {DCF_ERR_CONNECTION_TIMEOUT,      "CONNECTION_TIMEOUT",          "Connection timeout"},
    {DCF_ERR_ROUTE_NOT_FOUND,         "ROUTE_NOT_FOUND",             "No route found"},
    {DCF_ERR_PEER_UNREACHABLE,        "PEER_UNREACHABLE",            "Peer unreachable"},
    {DCF_ERR_NETWORK_INIT_FAIL,       "NETWORK_INIT_FAIL",           "Network initialization failed"},
    
    /* Serialization errors */
    {DCF_ERR_SERIALIZATION_FAIL,      "SERIALIZATION_FAIL",          "Serialization failed"},
    {DCF_ERR_DESERIALIZATION_FAIL,    "DESERIALIZATION_FAIL",        "Deserialization failed"},
    {DCF_ERR_INVALID_MESSAGE,         "INVALID_MESSAGE",             "Invalid message format"},
    {DCF_ERR_MESSAGE_TOO_LARGE,       "MESSAGE_TOO_LARGE",           "Message exceeds size limit"},
    
    /* Plugin errors */
    {DCF_ERR_PLUGIN_FAIL,             "PLUGIN_FAIL",                 "Plugin error"},
    {DCF_ERR_PLUGIN_NOT_FOUND,        "PLUGIN_NOT_FOUND",            "Plugin not found"},
    {DCF_ERR_PLUGIN_VERSION_MISMATCH, "PLUGIN_VERSION_MISMATCH",     "Plugin version mismatch"},
    {DCF_ERR_PLUGIN_INIT_FAIL,        "PLUGIN_INIT_FAIL",            "Plugin initialization failed"},
    
    /* State errors */
    {DCF_ERR_INVALID_STATE,           "INVALID_STATE",               "Invalid state"},
    {DCF_ERR_NOT_INITIALIZED,         "NOT_INITIALIZED",             "Not initialized"},
    {DCF_ERR_ALREADY_RUNNING,         "ALREADY_RUNNING",             "Already running"},
    {DCF_ERR_NOT_RUNNING,             "NOT_RUNNING",                 "Not running"},
    {DCF_ERR_SHUTDOWN_IN_PROGRESS,    "SHUTDOWN_IN_PROGRESS",        "Shutdown in progress"},
    
    /* Security errors */
    {DCF_ERR_AUTH_FAIL,               "AUTH_FAIL",                   "Authentication failed"},
    {DCF_ERR_PERMISSION_DENIED,       "PERMISSION_DENIED",           "Permission denied"},
    {DCF_ERR_INVALID_CERTIFICATE,     "INVALID_CERTIFICATE",         "Invalid certificate"},
    
    /* Argument errors */
    {DCF_ERR_INVALID_ARG,             "INVALID_ARG",                 "Invalid argument"},
    {DCF_ERR_ARG_OUT_OF_RANGE,        "ARG_OUT_OF_RANGE",            "Argument out of range"},
    
    /* Generic */
    {DCF_ERR_UNKNOWN,                 "UNKNOWN",                     "Unknown error"}
};

static const size_t error_table_size = sizeof(error_table) / sizeof(error_table[0]);

/* ============================================================================
 * Log Level Names
 * ============================================================================ */

static const char* log_level_names[] = {
    "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL", "OFF"
};

static const char* log_level_colors[] = {
    "\033[90m",  /* TRACE - gray */
    "\033[36m",  /* DEBUG - cyan */
    "\033[32m",  /* INFO  - green */
    "\033[33m",  /* WARN  - yellow */
    "\033[31m",  /* ERROR - red */
    "\033[35m",  /* FATAL - magenta */
    ""           /* OFF */
};

#define COLOR_RESET "\033[0m"

/* ============================================================================
 * Error Functions Implementation
 * ============================================================================ */

const char* dcf_error_str(DCFError err) {
    for (size_t i = 0; i < error_table_size; i++) {
        if (error_table[i].code == err) {
            return error_table[i].description;
        }
    }
    return "Unknown error";
}

const char* dcf_error_category(DCFError err) {
    uint16_t category = (err >> 8) & 0xFF;
    switch (category) {
        case 0x00: return "General";
        case 0x01: return "Memory";
        case 0x02: return "Configuration";
        case 0x03: return "Network";
        case 0x04: return "Serialization";
        case 0x05: return "Plugin";
        case 0x06: return "State";
        case 0x07: return "Security";
        case 0x08: return "Argument";
        default:   return "Unknown";
    }
}

bool dcf_error_is_category(DCFError err, uint16_t category) {
    return ((err >> 8) & 0xFF) == category;
}

void dcf_set_error(DCFError code, const char* file, int line,
                   const char* func, const char* fmt, ...) {
    g_last_error.code = code;
    g_last_error.line = line;
    g_last_error.timestamp = time(NULL);
    g_last_error.os_errno = errno;
    
    if (file) {
        /* Extract just filename from path */
        const char* basename = strrchr(file, '/');
        basename = basename ? basename + 1 : file;
        strncpy(g_last_error.file, basename, DCF_ERROR_FILE_MAX_LEN - 1);
        g_last_error.file[DCF_ERROR_FILE_MAX_LEN - 1] = '\0';
    }
    
    if (func) {
        strncpy(g_last_error.function, func, DCF_ERROR_FUNC_MAX_LEN - 1);
        g_last_error.function[DCF_ERROR_FUNC_MAX_LEN - 1] = '\0';
    }
    
    if (fmt) {
        va_list args;
        va_start(args, fmt);
        vsnprintf(g_last_error.message, DCF_ERROR_MSG_MAX_LEN, fmt, args);
        va_end(args);
    } else {
        strncpy(g_last_error.message, dcf_error_str(code), DCF_ERROR_MSG_MAX_LEN - 1);
    }
}

const DCFErrorContext* dcf_get_last_error(void) {
    return &g_last_error;
}

void dcf_clear_error(void) {
    memset(&g_last_error, 0, sizeof(g_last_error));
}

/* ============================================================================
 * Logging Functions Implementation
 * ============================================================================ */

DCFError dcf_log_init(const DCFLoggerConfig* config) {
    if (g_logger.initialized) {
        dcf_log_shutdown();
    }
    
    if (pthread_mutex_init(&g_logger.mutex, NULL) != 0) {
        return DCF_ERR_MALLOC_FAIL;
    }
    
    if (config) {
        memcpy(&g_logger.config, config, sizeof(DCFLoggerConfig));
    }
    
    if (g_logger.config.log_to_file && g_logger.config.log_file_path[0]) {
        g_logger.log_file = fopen(g_logger.config.log_file_path, "a");
        if (!g_logger.log_file) {
            pthread_mutex_destroy(&g_logger.mutex);
            return DCF_ERR_CONFIG_INVALID;
        }
    }
    
    g_logger.initialized = true;
    return DCF_SUCCESS;
}

void dcf_log_shutdown(void) {
    if (!g_logger.initialized) return;
    
    pthread_mutex_lock(&g_logger.mutex);
    if (g_logger.log_file) {
        fclose(g_logger.log_file);
        g_logger.log_file = NULL;
    }
    pthread_mutex_unlock(&g_logger.mutex);
    
    pthread_mutex_destroy(&g_logger.mutex);
    g_logger.initialized = false;
}

void dcf_log_set_level(DCFLogLevel level) {
    g_logger.config.min_level = level;
}

DCFLogLevel dcf_log_get_level(void) {
    return g_logger.config.min_level;
}

void dcf_log_write(DCFLogLevel level, const char* file, int line,
                   const char* func, const char* fmt, ...) {
    if (level < g_logger.config.min_level) return;
    if (level >= DCF_LOG_OFF) return;
    
    char message[1024];
    va_list args;
    va_start(args, fmt);
    vsnprintf(message, sizeof(message), fmt, args);
    va_end(args);
    
    /* Build timestamp */
    char timestamp[32] = "";
    if (g_logger.config.include_timestamp) {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        struct tm* tm_info = localtime(&tv.tv_sec);
        snprintf(timestamp, sizeof(timestamp), "%04d-%02d-%02d %02d:%02d:%02d.%03ld ",
                 tm_info->tm_year + 1900, tm_info->tm_mon + 1, tm_info->tm_mday,
                 tm_info->tm_hour, tm_info->tm_min, tm_info->tm_sec,
                 tv.tv_usec / 1000);
    }
    
    /* Build location */
    char location[256] = "";
    if (g_logger.config.include_source_location && file) {
        const char* basename = strrchr(file, '/');
        basename = basename ? basename + 1 : file;
        snprintf(location, sizeof(location), " [%s:%d %s()]", basename, line, func);
    }
    
    /* Call custom callback if set */
    if (g_logger.config.callback) {
        g_logger.config.callback(level, file, line, func, message,
                                  g_logger.config.callback_user_data);
    }
    
    /* Lock for file I/O */
    if (g_logger.initialized) {
        pthread_mutex_lock(&g_logger.mutex);
    }
    
    /* Write to stderr */
    if (g_logger.config.log_to_stderr) {
        bool use_color = isatty(fileno(stderr));
        fprintf(stderr, "%s%s%s%-5s%s%s: %s\n",
                timestamp,
                use_color ? log_level_colors[level] : "",
                "[",
                log_level_names[level],
                "]",
                use_color ? COLOR_RESET : "",
                message);
        if (g_logger.config.include_source_location) {
            fprintf(stderr, "       %s\n", location);
        }
    }
    
    /* Write to file */
    if (g_logger.log_file) {
        fprintf(g_logger.log_file, "%s[%-5s]%s: %s\n",
                timestamp, log_level_names[level], location, message);
        fflush(g_logger.log_file);
    }
    
    if (g_logger.initialized) {
        pthread_mutex_unlock(&g_logger.mutex);
    }
}
