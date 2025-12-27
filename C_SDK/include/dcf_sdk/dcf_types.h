/**
 * @file dcf_types.h
 * @brief Common types and definitions for DCF
 * 
 * This header provides:
 * - Mode enumerations
 * - Common constants
 * - Forward declarations
 * - Platform compatibility macros
 */

#ifndef DCF_TYPES_H
#define DCF_TYPES_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Version Information
 * ============================================================================ */

#define DCF_VERSION_MAJOR 5
#define DCF_VERSION_MINOR 1
#define DCF_VERSION_PATCH 0
#define DCF_VERSION_STRING "5.1.0"

/* ============================================================================
 * Platform Detection and Compatibility
 * ============================================================================ */

#if defined(_WIN32) || defined(_WIN64)
    #define DCF_PLATFORM_WINDOWS 1
    #define DCF_EXPORT __declspec(dllexport)
    #define DCF_IMPORT __declspec(dllimport)
#else
    #define DCF_PLATFORM_POSIX 1
    #define DCF_EXPORT __attribute__((visibility("default")))
    #define DCF_IMPORT
#endif

#ifdef DCF_BUILD_SHARED
    #define DCF_API DCF_EXPORT
#else
    #define DCF_API
#endif

/* Thread-local storage */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    #define DCF_THREAD_LOCAL _Thread_local
#elif defined(__GNUC__)
    #define DCF_THREAD_LOCAL __thread
#elif defined(_MSC_VER)
    #define DCF_THREAD_LOCAL __declspec(thread)
#else
    #define DCF_THREAD_LOCAL
#endif

/* ============================================================================
 * Operating Modes
 * ============================================================================ */

typedef enum DCFMode {
    DCF_MODE_CLIENT = 0,     /**< Client-only mode */
    DCF_MODE_SERVER = 1,     /**< Server-only mode */
    DCF_MODE_P2P    = 2,     /**< Peer-to-peer mode */
    DCF_MODE_AUTO   = 3,     /**< Auto-detect mode */
    DCF_MODE_HYBRID = 4      /**< Combined client/server */
} DCFMode;

/* Legacy compatibility aliases */
#define CLIENT_MODE DCF_MODE_CLIENT
#define SERVER_MODE DCF_MODE_SERVER
#define P2P_MODE    DCF_MODE_P2P
#define AUTO_MODE   DCF_MODE_AUTO

/* ============================================================================
 * Command Enumeration
 * ============================================================================ */

typedef enum DCFCmd {
    DCF_CMD_UNKNOWN         = 0,
    DCF_CMD_INIT            = 1,
    DCF_CMD_START           = 2,
    DCF_CMD_STOP            = 3,
    DCF_CMD_STATUS          = 4,
    DCF_CMD_SEND            = 5,
    DCF_CMD_RECEIVE         = 6,
    DCF_CMD_HEALTH_CHECK    = 7,
    DCF_CMD_LIST_PEERS      = 8,
    DCF_CMD_HEAL            = 9,
    DCF_CMD_VERSION         = 10,
    DCF_CMD_BENCHMARK       = 11,
    DCF_CMD_GROUP_PEERS     = 12,
    DCF_CMD_SIMULATE_FAILURE = 13,
    DCF_CMD_LOG_LEVEL       = 14,
    DCF_CMD_LOAD_PLUGIN     = 15,
    DCF_CMD_TUI             = 16,
    DCF_CMD_RELOAD_CONFIG   = 17,
    DCF_CMD_SHUTDOWN        = 18,
    DCF_CMD_MAX
} DCFCmd;

/* ============================================================================
 * Peer Status
 * ============================================================================ */

typedef enum DCFPeerStatus {
    DCF_PEER_UNKNOWN        = 0,
    DCF_PEER_HEALTHY        = 1,
    DCF_PEER_DEGRADED       = 2,
    DCF_PEER_UNREACHABLE    = 3,
    DCF_PEER_CONNECTING     = 4,
    DCF_PEER_DISCONNECTED   = 5
} DCFPeerStatus;

typedef enum DCFPeerGroup {
    DCF_GROUP_UNKNOWN       = 0,
    DCF_GROUP_LOCAL         = 1,
    DCF_GROUP_REGIONAL      = 2,
    DCF_GROUP_REMOTE        = 3,
    DCF_GROUP_UNREACHABLE   = 4
} DCFPeerGroup;

/* ============================================================================
 * Constants
 * ============================================================================ */

#define DCF_MAX_PEERS           256
#define DCF_MAX_MESSAGE_SIZE    (16 * 1024 * 1024)  /* 16 MB */
#define DCF_MAX_NODE_ID_LEN     128
#define DCF_MAX_HOST_LEN        256
#define DCF_MAX_PATH_LEN        4096
#define DCF_DEFAULT_PORT        50051
#define DCF_DEFAULT_RTT_THRESHOLD 100  /* milliseconds */
#define DCF_HEALTH_CHECK_INTERVAL 30   /* seconds */

/* ============================================================================
 * Forward Declarations (Opaque Types)
 * ============================================================================ */

typedef struct DCFClient DCFClient;
typedef struct DCFConfig DCFConfig;
typedef struct DCFNetworking DCFNetworking;
typedef struct DCFRedundancy DCFRedundancy;
typedef struct DCFPluginManager DCFPluginManager;
typedef struct DCFMessage DCFMessage;

/* ============================================================================
 * Callback Types
 * ============================================================================ */

/**
 * @brief Message received callback
 * @param message Received message data (caller must free)
 * @param message_len Length of message
 * @param sender Sender identifier (caller must free)
 * @param user_data User-provided context
 */
typedef void (*DCFMessageCallback)(const uint8_t* message, size_t message_len,
                                   const char* sender, void* user_data);

/**
 * @brief Peer status change callback
 */
typedef void (*DCFPeerStatusCallback)(const char* peer, DCFPeerStatus old_status,
                                      DCFPeerStatus new_status, void* user_data);

/**
 * @brief Health check result callback
 */
typedef void (*DCFHealthCallback)(const char* peer, int rtt_ms, bool success,
                                  void* user_data);

/* ============================================================================
 * Plugin Transport Interface
 * ============================================================================ */

/**
 * @brief Transport plugin interface
 * 
 * Plugins must implement this interface to provide custom transport mechanisms.
 */
typedef struct ITransport {
    void* ctx;  /**< Plugin-specific context */
    
    /** Initialize transport with host and port */
    bool (*setup)(struct ITransport* self, const char* host, int port);
    
    /** Send data to recipient */
    bool (*send)(struct ITransport* self, const uint8_t* data, size_t len, 
                 const char* recipient);
    
    /** Receive data (blocking or with timeout) */
    uint8_t* (*receive)(struct ITransport* self, size_t* len_out);
    
    /** Receive with timeout in milliseconds (-1 for infinite) */
    uint8_t* (*receive_timeout)(struct ITransport* self, size_t* len_out, 
                                 int timeout_ms);
    
    /** Check if connected */
    bool (*is_connected)(struct ITransport* self);
    
    /** Destroy transport and free resources */
    void (*destroy)(struct ITransport* self);
    
    /** Get transport name/version for diagnostics */
    const char* (*get_info)(struct ITransport* self);
} ITransport;

/* ============================================================================
 * Utility Macros
 * ============================================================================ */

/** Safe string copy with null termination */
#define DCF_SAFE_STRCPY(dst, src, dst_size) \
    do { \
        strncpy((dst), (src), (dst_size) - 1); \
        (dst)[(dst_size) - 1] = '\0'; \
    } while(0)

/** Array element count */
#define DCF_ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

/** Minimum/Maximum */
#define DCF_MIN(a, b) (((a) < (b)) ? (a) : (b))
#define DCF_MAX(a, b) (((a) > (b)) ? (a) : (b))

/** Unused parameter marker */
#define DCF_UNUSED(x) ((void)(x))

#ifdef __cplusplus
}
#endif

#endif /* DCF_TYPES_H */
