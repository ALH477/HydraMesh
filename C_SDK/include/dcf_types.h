/**
 * @file dcf_types.h
 * @brief Common types and definitions for DCF
 * @version 5.2.0
 *
 * This header provides:
 * - Version information
 * - Operating mode enumerations
 * - Common structures
 * - Forward declarations
 * - Compile-time configuration
 * - Utility macros
 */

#ifndef DCF_TYPES_H
#define DCF_TYPES_H

#include "dcf_platform.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Version Information
 * ============================================================================ */

#define DCF_VERSION_MAJOR 5
#define DCF_VERSION_MINOR 2
#define DCF_VERSION_PATCH 0
#define DCF_VERSION_STRING "5.2.0"
#define DCF_VERSION_HEX ((DCF_VERSION_MAJOR << 16) | (DCF_VERSION_MINOR << 8) | DCF_VERSION_PATCH)

/**
 * @brief Runtime version check
 */
DCF_API uint32_t dcf_version(void);
DCF_API const char* dcf_version_string(void);

/* ============================================================================
 * Compile-Time Limits (can be overridden via compiler flags)
 * ============================================================================ */

#ifndef DCF_MAX_PEERS
    #define DCF_MAX_PEERS               1024
#endif

#ifndef DCF_MAX_MESSAGE_SIZE
    #define DCF_MAX_MESSAGE_SIZE        (64 * 1024 * 1024)  /* 64 MB */
#endif

#ifndef DCF_MAX_NODE_ID_LEN
    #define DCF_MAX_NODE_ID_LEN         256
#endif

#ifndef DCF_MAX_HOST_LEN
    #define DCF_MAX_HOST_LEN            256
#endif

#ifndef DCF_MAX_PATH_LEN
    #define DCF_MAX_PATH_LEN            4096
#endif

#ifndef DCF_MAX_ERROR_MSG_LEN
    #define DCF_MAX_ERROR_MSG_LEN       512
#endif

#ifndef DCF_MAX_PENDING_MESSAGES
    #define DCF_MAX_PENDING_MESSAGES    10000
#endif

#ifndef DCF_MAX_CALLBACKS
    #define DCF_MAX_CALLBACKS           64
#endif

/* Default timeouts in milliseconds */
#define DCF_DEFAULT_PORT                50051
#define DCF_DEFAULT_RTT_THRESHOLD       100
#define DCF_DEFAULT_CONNECT_TIMEOUT_MS  10000
#define DCF_DEFAULT_RECV_TIMEOUT_MS     30000
#define DCF_DEFAULT_SEND_TIMEOUT_MS     30000
#define DCF_DEFAULT_HEALTH_INTERVAL_MS  30000
#define DCF_DEFAULT_RECONNECT_DELAY_MS  1000
#define DCF_DEFAULT_MAX_RECONNECT       10

/* ============================================================================
 * Operating Modes
 * ============================================================================ */

typedef enum DCFMode {
    DCF_MODE_INVALID = -1,
    DCF_MODE_CLIENT  = 0,     /**< Client-only mode */
    DCF_MODE_SERVER  = 1,     /**< Server-only mode */
    DCF_MODE_P2P     = 2,     /**< Peer-to-peer mode */
    DCF_MODE_AUTO    = 3,     /**< Auto-detect mode */
    DCF_MODE_HYBRID  = 4,     /**< Combined client/server */
    DCF_MODE_MAX
} DCFMode;

/**
 * @brief Convert mode to string
 */
DCF_API const char* dcf_mode_to_string(DCFMode mode);

/**
 * @brief Parse mode from string
 * @return DCF_MODE_INVALID on failure
 */
DCF_API DCFMode dcf_mode_from_string(const char* str);

/* Legacy compatibility aliases */
#define CLIENT_MODE DCF_MODE_CLIENT
#define SERVER_MODE DCF_MODE_SERVER
#define P2P_MODE    DCF_MODE_P2P
#define AUTO_MODE   DCF_MODE_AUTO

/* ============================================================================
 * Client State Machine
 * ============================================================================ */

typedef enum DCFClientState {
    DCF_STATE_UNINITIALIZED = 0,
    DCF_STATE_INITIALIZED   = 1,
    DCF_STATE_STARTING      = 2,
    DCF_STATE_RUNNING       = 3,
    DCF_STATE_STOPPING      = 4,
    DCF_STATE_STOPPED       = 5,
    DCF_STATE_ERROR         = 6,
    DCF_STATE_RECONNECTING  = 7,
    DCF_STATE_DRAINING      = 8,  /**< Graceful shutdown, processing remaining */
    DCF_STATE_MAX
} DCFClientState;

DCF_API const char* dcf_state_to_string(DCFClientState state);

/* ============================================================================
 * Peer Status
 * ============================================================================ */

typedef enum DCFPeerStatus {
    DCF_PEER_UNKNOWN        = 0,
    DCF_PEER_CONNECTING     = 1,
    DCF_PEER_HEALTHY        = 2,
    DCF_PEER_DEGRADED       = 3,  /**< High latency or packet loss */
    DCF_PEER_UNREACHABLE    = 4,
    DCF_PEER_BANNED         = 5,  /**< Blacklisted peer */
    DCF_PEER_DRAINING       = 6,  /**< Being removed */
    DCF_PEER_MAX
} DCFPeerStatus;

typedef enum DCFPeerGroup {
    DCF_GROUP_UNKNOWN       = 0,
    DCF_GROUP_LOCAL         = 1,  /**< < 10ms RTT */
    DCF_GROUP_REGIONAL      = 2,  /**< 10-100ms RTT */
    DCF_GROUP_REMOTE        = 3,  /**< > 100ms RTT */
    DCF_GROUP_UNREACHABLE   = 4,
    DCF_GROUP_MAX
} DCFPeerGroup;

DCF_API const char* dcf_peer_status_to_string(DCFPeerStatus status);
DCF_API const char* dcf_peer_group_to_string(DCFPeerGroup group);

/* ============================================================================
 * Command Enumeration (for CLI and internal use)
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
    DCF_CMD_DRAIN           = 19,
    DCF_CMD_MAX
} DCFCmd;

/* ============================================================================
 * Forward Declarations (Opaque Types)
 * ============================================================================ */

typedef struct DCFClient        DCFClient;
typedef struct DCFConfig        DCFConfig;
typedef struct DCFNetworking    DCFNetworking;
typedef struct DCFRedundancy    DCFRedundancy;
typedef struct DCFPluginManager DCFPluginManager;
typedef struct DCFMessage       DCFMessage;
typedef struct DCFConnection    DCFConnection;
typedef struct DCFMessageQueue  DCFMessageQueue;
typedef struct DCFCircuitBreaker DCFCircuitBreaker;

/* ============================================================================
 * Callback Types
 * ============================================================================ */

/**
 * @brief Message received callback
 * @param message Received message data
 * @param message_len Length of message
 * @param sender Sender identifier
 * @param user_data User-provided context
 * @note message and sender are only valid for the duration of the callback
 */
typedef void (*DCFMessageCallback)(
    const uint8_t* message, 
    size_t message_len,
    const char* sender, 
    void* user_data
);

/**
 * @brief Peer status change callback
 */
typedef void (*DCFPeerStatusCallback)(
    const char* peer, 
    DCFPeerStatus old_status,
    DCFPeerStatus new_status, 
    void* user_data
);

/**
 * @brief Health check result callback
 */
typedef void (*DCFHealthCallback)(
    const char* peer, 
    int rtt_ms, 
    bool success,
    void* user_data
);

/**
 * @brief Configuration change callback
 */
typedef void (*DCFConfigChangeCallback)(
    const char* key, 
    const char* old_value,
    const char* new_value, 
    void* user_data
);

/**
 * @brief Error callback for async operations
 */
typedef void (*DCFErrorCallback)(
    int error_code,
    const char* error_message,
    void* user_data
);

/**
 * @brief Connection state change callback
 */
typedef void (*DCFConnectionCallback)(
    const char* peer,
    bool connected,
    void* user_data
);

/* ============================================================================
 * Message Structure
 * ============================================================================ */

/**
 * @brief Message header (wire format)
 */
typedef struct DCF_PACKED DCFMessageHeader {
    uint32_t magic;          /**< Magic number: 0x44434631 ('DCF1') */
    uint32_t version;        /**< Protocol version */
    uint32_t flags;          /**< Message flags */
    uint32_t sequence;       /**< Sequence number */
    uint64_t timestamp;      /**< Unix timestamp in microseconds */
    uint32_t payload_len;    /**< Payload length */
    uint32_t header_crc;     /**< CRC32 of header */
    uint32_t payload_crc;    /**< CRC32 of payload */
    uint8_t  sender_len;     /**< Sender ID length */
    uint8_t  recipient_len;  /**< Recipient ID length */
    uint16_t reserved;       /**< Reserved for future use */
} DCFMessageHeader;

#define DCF_MESSAGE_MAGIC       0x44434631  /* 'DCF1' */
#define DCF_MESSAGE_VERSION     2

/* Message flags */
#define DCF_MSG_FLAG_NONE       0x00000000
#define DCF_MSG_FLAG_COMPRESSED 0x00000001
#define DCF_MSG_FLAG_ENCRYPTED  0x00000002
#define DCF_MSG_FLAG_PRIORITY   0x00000004
#define DCF_MSG_FLAG_ACK_REQ    0x00000008
#define DCF_MSG_FLAG_IS_ACK     0x00000010
#define DCF_MSG_FLAG_HEARTBEAT  0x00000020
#define DCF_MSG_FLAG_HEALTH_REQ 0x00000040
#define DCF_MSG_FLAG_HEALTH_RSP 0x00000080
#define DCF_MSG_FLAG_BINARY     0x00000100
#define DCF_MSG_FLAG_FRAGMENTED 0x00000200
#define DCF_MSG_FLAG_LAST_FRAG  0x00000400

/* ============================================================================
 * Transport Plugin Interface
 * ============================================================================ */

/**
 * @brief Transport plugin interface
 * 
 * Plugins must implement this interface to provide custom transport mechanisms.
 * All functions must be thread-safe.
 */
typedef struct ITransport {
    void* ctx;  /**< Plugin-specific context */
    
    /** Initialize transport with host and port */
    int (*setup)(struct ITransport* self, const char* host, int port);
    
    /** Send data to recipient (blocking with timeout) */
    int (*send)(struct ITransport* self, const uint8_t* data, size_t len, 
                const char* recipient, uint32_t timeout_ms);
    
    /** Receive data with timeout */
    int (*receive)(struct ITransport* self, uint8_t** data_out, size_t* len_out,
                   char** sender_out, uint32_t timeout_ms);
    
    /** Check if connected to peer */
    bool (*is_connected)(struct ITransport* self, const char* peer);
    
    /** Disconnect from peer */
    int (*disconnect)(struct ITransport* self, const char* peer);
    
    /** Shutdown transport (initiate graceful shutdown) */
    int (*shutdown)(struct ITransport* self);
    
    /** Destroy transport and free resources */
    void (*destroy)(struct ITransport* self);
    
    /** Get transport name for diagnostics */
    const char* (*get_name)(struct ITransport* self);
    
    /** Get transport version for diagnostics */
    const char* (*get_version)(struct ITransport* self);
    
    /** Get file descriptor for poll/select (optional, return -1 if not applicable) */
    int (*get_fd)(struct ITransport* self);
} ITransport;

/* ============================================================================
 * Connection Pool Configuration
 * ============================================================================ */

typedef struct DCFConnectionPoolConfig {
    size_t min_connections;           /**< Minimum connections to maintain */
    size_t max_connections;           /**< Maximum total connections */
    size_t max_connections_per_peer;  /**< Max connections per peer */
    uint32_t idle_timeout_ms;         /**< Close idle connections after this */
    uint32_t connect_timeout_ms;      /**< Connection establishment timeout */
    bool enable_keepalive;            /**< Enable TCP keepalive */
    uint32_t keepalive_interval_ms;   /**< Keepalive probe interval */
} DCFConnectionPoolConfig;

#define DCF_CONNECTION_POOL_CONFIG_DEFAULT { \
    .min_connections = 0,                    \
    .max_connections = 100,                  \
    .max_connections_per_peer = 4,           \
    .idle_timeout_ms = 300000,               \
    .connect_timeout_ms = 10000,             \
    .enable_keepalive = true,                \
    .keepalive_interval_ms = 30000           \
}

/* ============================================================================
 * Circuit Breaker Configuration
 * ============================================================================ */

typedef enum DCFCircuitState {
    DCF_CIRCUIT_CLOSED   = 0,  /**< Normal operation */
    DCF_CIRCUIT_OPEN     = 1,  /**< Failing fast */
    DCF_CIRCUIT_HALF_OPEN = 2  /**< Testing recovery */
} DCFCircuitState;

typedef struct DCFCircuitBreakerConfig {
    uint32_t failure_threshold;     /**< Failures before opening */
    uint32_t success_threshold;     /**< Successes to close after half-open */
    uint32_t timeout_ms;            /**< Time before moving to half-open */
    double failure_rate_threshold;  /**< Failure rate (0.0-1.0) to open */
    size_t sample_window;           /**< Window size for rate calculation */
} DCFCircuitBreakerConfig;

#define DCF_CIRCUIT_BREAKER_CONFIG_DEFAULT { \
    .failure_threshold = 5,                  \
    .success_threshold = 3,                  \
    .timeout_ms = 30000,                     \
    .failure_rate_threshold = 0.5,           \
    .sample_window = 100                     \
}

/* ============================================================================
 * Retry Configuration
 * ============================================================================ */

typedef enum DCFRetryStrategy {
    DCF_RETRY_NONE          = 0,
    DCF_RETRY_FIXED         = 1,  /**< Fixed delay between retries */
    DCF_RETRY_LINEAR        = 2,  /**< delay * attempt_number */
    DCF_RETRY_EXPONENTIAL   = 3,  /**< delay * 2^attempt_number */
    DCF_RETRY_JITTER        = 4   /**< Exponential with random jitter */
} DCFRetryStrategy;

typedef struct DCFRetryConfig {
    DCFRetryStrategy strategy;
    uint32_t max_attempts;       /**< Maximum number of attempts (0 = infinite) */
    uint32_t base_delay_ms;      /**< Base delay between retries */
    uint32_t max_delay_ms;       /**< Maximum delay (caps exponential growth) */
    double jitter_factor;        /**< Jitter range (0.0-1.0) */
} DCFRetryConfig;

#define DCF_RETRY_CONFIG_DEFAULT {       \
    .strategy = DCF_RETRY_JITTER,        \
    .max_attempts = 5,                   \
    .base_delay_ms = 100,                \
    .max_delay_ms = 30000,               \
    .jitter_factor = 0.3                 \
}

/* ============================================================================
 * Rate Limiter Configuration
 * ============================================================================ */

typedef struct DCFRateLimitConfig {
    uint32_t max_requests;       /**< Maximum requests */
    uint32_t window_ms;          /**< Time window in milliseconds */
    uint32_t burst_size;         /**< Maximum burst size */
    bool drop_on_limit;          /**< Drop vs queue when limited */
} DCFRateLimitConfig;

#define DCF_RATE_LIMIT_CONFIG_DEFAULT {  \
    .max_requests = 1000,                \
    .window_ms = 1000,                   \
    .burst_size = 100,                   \
    .drop_on_limit = false               \
}

/* ============================================================================
 * Backpressure Configuration
 * ============================================================================ */

typedef enum DCFBackpressureStrategy {
    DCF_BP_BLOCK      = 0,  /**< Block sender until space available */
    DCF_BP_DROP_NEW   = 1,  /**< Drop new messages when full */
    DCF_BP_DROP_OLD   = 2,  /**< Drop oldest messages when full */
    DCF_BP_CALLBACK   = 3   /**< Call user callback for decision */
} DCFBackpressureStrategy;

typedef bool (*DCFBackpressureCallback)(
    size_t queue_size,
    size_t queue_capacity,
    const DCFMessageHeader* new_msg,
    void* user_data
);

typedef struct DCFBackpressureConfig {
    DCFBackpressureStrategy strategy;
    size_t high_watermark;         /**< Start applying backpressure */
    size_t low_watermark;          /**< Resume normal operation */
    DCFBackpressureCallback callback;
    void* callback_data;
} DCFBackpressureConfig;

#define DCF_BACKPRESSURE_CONFIG_DEFAULT { \
    .strategy = DCF_BP_BLOCK,             \
    .high_watermark = 8000,               \
    .low_watermark = 4000,                \
    .callback = NULL,                     \
    .callback_data = NULL                 \
}

/* ============================================================================
 * Utility Macros
 * ============================================================================ */

/** Safe string copy with null termination */
#define DCF_SAFE_STRCPY(dst, src, dst_size) do {               \
    if ((dst) && (src) && (dst_size) > 0) {                    \
        strncpy((dst), (src), (dst_size) - 1);                 \
        (dst)[(dst_size) - 1] = '\0';                          \
    }                                                          \
} while(0)

/** Array element count */
#define DCF_ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

/** Minimum/Maximum */
#define DCF_MIN(a, b) (((a) < (b)) ? (a) : (b))
#define DCF_MAX(a, b) (((a) > (b)) ? (a) : (b))
#define DCF_CLAMP(val, min, max) DCF_MAX((min), DCF_MIN((val), (max)))

/** Power of 2 check and alignment */
#define DCF_IS_POWER_OF_2(x)    (((x) != 0) && (((x) & ((x) - 1)) == 0))
#define DCF_ALIGN_UP(x, align)  (((x) + ((align) - 1)) & ~((align) - 1))
#define DCF_ALIGN_DOWN(x, align) ((x) & ~((align) - 1))

/** Container of - get struct from member pointer */
#define DCF_CONTAINER_OF(ptr, type, member) \
    ((type*)((char*)(ptr) - offsetof(type, member)))

/** Stringify */
#define DCF_STRINGIFY(x) #x
#define DCF_TOSTRING(x) DCF_STRINGIFY(x)

/** Concatenate */
#define DCF_CONCAT(a, b) a##b
#define DCF_CONCAT3(a, b, c) a##b##c

/** Compile-time assertion */
#define DCF_STATIC_ASSERT(expr, msg) _Static_assert(expr, msg)

/** Unused parameter */
#define DCF_UNUSED_PARAM(x) ((void)(x))

/** Branch hints */
#define DCF_EXPECT_TRUE(x)  DCF_LIKELY(x)
#define DCF_EXPECT_FALSE(x) DCF_UNLIKELY(x)

#ifdef __cplusplus
}
#endif

#endif /* DCF_TYPES_H */
