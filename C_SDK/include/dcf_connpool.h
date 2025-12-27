/**
 * @file dcf_connpool.h
 * @brief Production Connection Pool with Circuit Breaker
 * @version 5.2.0
 *
 * Features:
 * - Thread-safe connection pooling
 * - Per-peer connection limits
 * - Automatic health checking
 * - Circuit breaker pattern
 * - Exponential backoff retry
 * - Connection affinity (sticky sessions)
 * - Graceful draining for shutdown
 * - Comprehensive metrics
 */

#ifndef DCF_CONNPOOL_H
#define DCF_CONNPOOL_H

#include "dcf_platform.h"
#include "dcf_error.h"
#include "dcf_types.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Types
 * ============================================================================ */

typedef struct DCFConnPool DCFConnPool;

/**
 * @brief Connection handle (opaque to users)
 */
typedef struct DCFPooledConn {
    dcf_socket_t socket;
    char peer_id[DCF_MAX_NODE_ID_LEN];
    ITransport* transport;
    void* user_context;
    
    /* Internal tracking */
    uint64_t create_time;
    uint64_t last_used;
    uint32_t use_count;
    int state;
    
    /* Pool linkage */
    struct DCFPooledConn* next;
    struct DCFPooledConn* prev;
} DCFPooledConn;

/**
 * @brief Connection state
 */
typedef enum DCFConnState {
    DCF_CONN_DISCONNECTED = 0,
    DCF_CONN_CONNECTING   = 1,
    DCF_CONN_CONNECTED    = 2,
    DCF_CONN_AUTHENTICATED = 3,
    DCF_CONN_DRAINING     = 4,
    DCF_CONN_FAILED       = 5
} DCFConnState;

/**
 * @brief Connection factory callback
 */
typedef DCFPooledConn* (*DCFConnFactory)(const char* peer, void* user_data);

/**
 * @brief Connection destructor callback
 */
typedef void (*DCFConnDestructor)(DCFPooledConn* conn, void* user_data);

/**
 * @brief Connection validator callback (health check)
 */
typedef bool (*DCFConnValidator)(DCFPooledConn* conn, void* user_data);

/* ============================================================================
 * Configuration
 * ============================================================================ */

typedef struct DCFConnPoolConfig {
    /* Pool sizing */
    size_t min_connections;           /**< Minimum total connections */
    size_t max_connections;           /**< Maximum total connections */
    size_t max_per_peer;              /**< Maximum connections per peer */
    size_t min_per_peer;              /**< Minimum connections per peer */
    
    /* Timeouts */
    uint32_t connect_timeout_ms;      /**< Connection establishment timeout */
    uint32_t acquire_timeout_ms;      /**< Timeout to acquire a connection */
    uint32_t idle_timeout_ms;         /**< Close connections idle for this long */
    uint32_t max_lifetime_ms;         /**< Max connection lifetime (0 = unlimited) */
    uint32_t validation_interval_ms;  /**< Time between health checks */
    
    /* Health checking */
    bool validate_on_acquire;         /**< Validate before giving to caller */
    bool validate_on_release;         /**< Validate on return to pool */
    bool test_on_idle;                /**< Test idle connections periodically */
    
    /* Circuit breaker */
    DCFCircuitBreakerConfig circuit;
    
    /* Retry configuration */
    DCFRetryConfig retry;
    
    /* Callbacks */
    DCFConnFactory factory;
    DCFConnDestructor destructor;
    DCFConnValidator validator;
    void* callback_data;
    
    /* Features */
    bool enable_affinity;             /**< Try to give same peer same connection */
    bool enable_warmup;               /**< Pre-create min_connections on start */
    bool enable_background_eviction;  /**< Background thread for cleanup */
} DCFConnPoolConfig;

#define DCF_CONNPOOL_CONFIG_DEFAULT {                           \
    .min_connections = 0,                                       \
    .max_connections = 100,                                     \
    .max_per_peer = 10,                                         \
    .min_per_peer = 0,                                          \
    .connect_timeout_ms = 10000,                                \
    .acquire_timeout_ms = 30000,                                \
    .idle_timeout_ms = 300000,                                  \
    .max_lifetime_ms = 0,                                       \
    .validation_interval_ms = 30000,                            \
    .validate_on_acquire = true,                                \
    .validate_on_release = false,                               \
    .test_on_idle = true,                                       \
    .circuit = DCF_CIRCUIT_BREAKER_CONFIG_DEFAULT,              \
    .retry = DCF_RETRY_CONFIG_DEFAULT,                          \
    .factory = NULL,                                            \
    .destructor = NULL,                                         \
    .validator = NULL,                                          \
    .callback_data = NULL,                                      \
    .enable_affinity = true,                                    \
    .enable_warmup = false,                                     \
    .enable_background_eviction = true                          \
}

/* ============================================================================
 * Statistics
 * ============================================================================ */

typedef struct DCFConnPoolStats {
    size_t total_connections;
    size_t idle_connections;
    size_t active_connections;
    size_t pending_acquisitions;
    
    dcf_atomic_uint64 connections_created;
    dcf_atomic_uint64 connections_destroyed;
    dcf_atomic_uint64 connections_failed;
    dcf_atomic_uint64 connections_evicted;
    dcf_atomic_uint64 connections_timed_out;
    
    dcf_atomic_uint64 acquisitions_total;
    dcf_atomic_uint64 acquisitions_succeeded;
    dcf_atomic_uint64 acquisitions_failed;
    dcf_atomic_uint64 acquisitions_timed_out;
    dcf_atomic_uint64 acquisition_wait_time_us;
    
    dcf_atomic_uint64 validations_total;
    dcf_atomic_uint64 validations_failed;
    
    dcf_atomic_uint64 circuit_opens;
    dcf_atomic_uint64 circuit_half_opens;
    dcf_atomic_uint64 circuit_closes;
    dcf_atomic_uint64 fast_fails;
} DCFConnPoolStats;

typedef struct DCFPeerStats {
    char peer_id[DCF_MAX_NODE_ID_LEN];
    size_t total_connections;
    size_t idle_connections;
    size_t active_connections;
    DCFCircuitState circuit_state;
    uint64_t last_success_time;
    uint64_t last_failure_time;
    uint32_t consecutive_failures;
    uint32_t avg_latency_us;
} DCFPeerStats;

/* ============================================================================
 * Lifecycle
 * ============================================================================ */

DCF_API DCFConnPool* dcf_connpool_create(const DCFConnPoolConfig* config);
DCF_API void dcf_connpool_destroy(DCFConnPool* pool, bool graceful);
DCF_API DCFError dcf_connpool_start(DCFConnPool* pool);
DCF_API DCFError dcf_connpool_stop(DCFConnPool* pool);
DCF_API DCFError dcf_connpool_drain(DCFConnPool* pool, uint32_t timeout_ms);

/* ============================================================================
 * Connection Acquisition
 * ============================================================================ */

DCF_API DCFPooledConn* dcf_connpool_acquire(DCFConnPool* pool, const char* peer,
                                             int32_t timeout_ms);
DCF_API void dcf_connpool_release(DCFConnPool* pool, DCFPooledConn* conn,
                                   bool is_healthy);
DCF_API void dcf_connpool_discard(DCFConnPool* pool, DCFPooledConn* conn);

/* ============================================================================
 * Connection Operations
 * ============================================================================ */

DCF_API DCFConnState dcf_conn_get_state(const DCFPooledConn* conn);
DCF_API const char* dcf_conn_get_peer(const DCFPooledConn* conn);
DCF_API dcf_socket_t dcf_conn_get_socket(const DCFPooledConn* conn);
DCF_API uint64_t dcf_conn_get_create_time(const DCFPooledConn* conn);
DCF_API uint64_t dcf_conn_get_last_used(const DCFPooledConn* conn);
DCF_API uint64_t dcf_conn_get_age_ms(const DCFPooledConn* conn);
DCF_API void* dcf_conn_get_context(const DCFPooledConn* conn);
DCF_API void dcf_conn_set_context(DCFPooledConn* conn, void* ctx);
DCF_API ITransport* dcf_conn_get_transport(DCFPooledConn* conn);

/* ============================================================================
 * Circuit Breaker
 * ============================================================================ */

DCF_API DCFCircuitState dcf_connpool_get_circuit_state(DCFConnPool* pool,
                                                        const char* peer);
DCF_API void dcf_connpool_open_circuit(DCFConnPool* pool, const char* peer);
DCF_API void dcf_connpool_reset_circuit(DCFConnPool* pool, const char* peer);
DCF_API void dcf_connpool_record_success(DCFConnPool* pool, const char* peer,
                                          uint32_t latency_us);
DCF_API void dcf_connpool_record_failure(DCFConnPool* pool, const char* peer);

/* ============================================================================
 * Pool Management
 * ============================================================================ */

DCF_API DCFError dcf_connpool_warmup(DCFConnPool* pool, const char* peer, size_t count);
DCF_API size_t dcf_connpool_evict_idle(DCFConnPool* pool);
DCF_API size_t dcf_connpool_close_peer(DCFConnPool* pool, const char* peer, bool graceful);
DCF_API size_t dcf_connpool_cleanup(DCFConnPool* pool);
DCF_API void dcf_connpool_close_all(DCFConnPool* pool);

/* ============================================================================
 * Statistics
 * ============================================================================ */

DCF_API void dcf_connpool_get_stats(const DCFConnPool* pool, DCFConnPoolStats* stats);
DCF_API void dcf_connpool_reset_stats(DCFConnPool* pool);
DCF_API DCFError dcf_connpool_get_peer_stats(const DCFConnPool* pool,
                                              const char* peer, DCFPeerStats* stats);
DCF_API size_t dcf_connpool_list_peers(const DCFConnPool* pool,
                                        char** peers, size_t max_peers);

typedef void (*DCFConnPoolEventCallback)(const char* peer, DCFConnState old_state,
                                         DCFConnState new_state, void* user_data);
DCF_API void dcf_connpool_set_event_callback(DCFConnPool* pool,
                                              DCFConnPoolEventCallback callback,
                                              void* user_data);

/* ============================================================================
 * Retry Helper
 * ============================================================================ */

DCF_API uint32_t dcf_retry_calc_delay(const DCFRetryConfig* config, uint32_t attempt);
DCF_API bool dcf_retry_should_retry(const DCFRetryConfig* config, DCFError err,
                                     uint32_t attempt);

typedef DCFError (*DCFRetryableOp)(void* context);
DCF_API DCFError dcf_retry_execute(const DCFRetryConfig* config,
                                    DCFRetryableOp operation, void* context);

#ifdef __cplusplus
}
#endif

#endif /* DCF_CONNPOOL_H */
