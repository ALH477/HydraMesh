/**
 * @file dcf_connpool.c
 * @brief Connection Pool with Circuit Breaker Implementation
 * @version 5.2.0
 */

#include "dcf_connpool.h"
#include "dcf_platform.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* ============================================================================
 * Internal Types
 * ============================================================================ */

typedef struct DCFPeerEntry {
    char peer_id[DCF_MAX_NODE_ID_LEN];
    
    /* Connections */
    DCFPooledConn* idle_head;
    DCFPooledConn* idle_tail;
    size_t idle_count;
    size_t active_count;
    
    /* Circuit breaker state */
    DCFCircuitState circuit_state;
    uint64_t circuit_opened_at;
    uint32_t consecutive_failures;
    uint32_t consecutive_successes;
    uint64_t failure_times[100];   /* Sliding window */
    size_t failure_idx;
    
    /* Metrics */
    uint64_t last_success_time;
    uint64_t last_failure_time;
    uint64_t total_latency_us;
    uint32_t request_count;
    
    struct DCFPeerEntry* next;
} DCFPeerEntry;

struct DCFConnPool {
    DCFConnPoolConfig config;
    
    /* Peer map (simple linked list for now, could be hash table) */
    DCFPeerEntry* peers_head;
    size_t peer_count;
    
    /* Global counts */
    size_t total_connections;
    size_t idle_connections;
    size_t active_connections;
    dcf_atomic_uint pending_acquisitions;
    
    /* Synchronization */
    dcf_rwlock_t peers_lock;
    dcf_mutex_t state_mutex;
    dcf_cond_t conn_available;
    
    /* Background threads */
    dcf_thread_t eviction_thread;
    dcf_thread_t health_thread;
    dcf_atomic_bool running;
    dcf_atomic_bool draining;
    
    /* Event callback */
    DCFConnPoolEventCallback event_callback;
    void* event_user_data;
    
    /* Statistics */
    DCFConnPoolStats stats;
};

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static DCFPeerEntry* find_peer(DCFConnPool* pool, const char* peer_id) {
    DCFPeerEntry* entry = pool->peers_head;
    while (entry) {
        if (strcmp(entry->peer_id, peer_id) == 0) return entry;
        entry = entry->next;
    }
    return NULL;
}

static DCFPeerEntry* get_or_create_peer(DCFConnPool* pool, const char* peer_id) {
    DCFPeerEntry* entry = find_peer(pool, peer_id);
    if (entry) return entry;
    
    entry = dcf_calloc(1, sizeof(DCFPeerEntry));
    if (!entry) return NULL;
    
    DCF_SAFE_STRCPY(entry->peer_id, peer_id, sizeof(entry->peer_id));
    entry->circuit_state = DCF_CIRCUIT_CLOSED;
    
    entry->next = pool->peers_head;
    pool->peers_head = entry;
    pool->peer_count++;
    
    return entry;
}

static void add_to_idle(DCFPeerEntry* peer, DCFPooledConn* conn) {
    conn->next = peer->idle_head;
    conn->prev = NULL;
    if (peer->idle_head) peer->idle_head->prev = conn;
    peer->idle_head = conn;
    if (!peer->idle_tail) peer->idle_tail = conn;
    peer->idle_count++;
}

static void remove_from_idle(DCFPeerEntry* peer, DCFPooledConn* conn) {
    if (conn->prev) conn->prev->next = conn->next;
    else peer->idle_head = conn->next;
    if (conn->next) conn->next->prev = conn->prev;
    else peer->idle_tail = conn->prev;
    conn->next = conn->prev = NULL;
    peer->idle_count--;
}

static bool check_circuit_breaker(DCFConnPool* pool, DCFPeerEntry* peer) {
    uint64_t now = dcf_time_monotonic_ms();
    
    switch (peer->circuit_state) {
        case DCF_CIRCUIT_CLOSED:
            /* Check failure rate */
            if (peer->consecutive_failures >= pool->config.circuit.failure_threshold) {
                peer->circuit_state = DCF_CIRCUIT_OPEN;
                peer->circuit_opened_at = now;
                peer->consecutive_successes = 0;
                dcf_atomic_fetch_add(&pool->stats.circuit_opens, 1);
                DCF_LOG_WARN("Circuit opened for peer %s after %u failures",
                             peer->peer_id, peer->consecutive_failures);
                return false;
            }
            return true;
            
        case DCF_CIRCUIT_OPEN:
            /* Check if timeout expired */
            if (now - peer->circuit_opened_at >= pool->config.circuit.timeout_ms) {
                peer->circuit_state = DCF_CIRCUIT_HALF_OPEN;
                dcf_atomic_fetch_add(&pool->stats.circuit_half_opens, 1);
                DCF_LOG_INFO("Circuit half-open for peer %s", peer->peer_id);
                return true;
            }
            dcf_atomic_fetch_add(&pool->stats.fast_fails, 1);
            return false;
            
        case DCF_CIRCUIT_HALF_OPEN:
            /* Allow one request through */
            return true;
    }
    return true;
}

static void destroy_connection(DCFConnPool* pool, DCFPooledConn* conn) {
    if (!conn) return;
    
    if (conn->socket != DCF_INVALID_SOCKET) {
        dcf_socket_close(conn->socket);
        conn->socket = DCF_INVALID_SOCKET;
    }
    
    if (pool->config.destructor) {
        /* Destructor is responsible for freeing the connection */
        pool->config.destructor(conn, pool->config.callback_data);
    } else {
        /* No destructor, free ourselves */
        dcf_free(conn);
    }
    
    dcf_atomic_fetch_add(&pool->stats.connections_destroyed, 1);
}

static DCFPooledConn* create_connection(DCFConnPool* pool, const char* peer_id) {
    DCFPooledConn* conn = NULL;
    
    if (pool->config.factory) {
        conn = pool->config.factory(peer_id, pool->config.callback_data);
    } else {
        conn = dcf_calloc(1, sizeof(DCFPooledConn));
        if (!conn) return NULL;
        
        DCF_SAFE_STRCPY(conn->peer_id, peer_id, sizeof(conn->peer_id));
        conn->socket = DCF_INVALID_SOCKET;
        conn->create_time = dcf_time_monotonic_ms();
        conn->state = DCF_CONN_DISCONNECTED;
    }
    
    if (conn) {
        conn->create_time = dcf_time_monotonic_ms();
        conn->last_used = conn->create_time;
        dcf_atomic_fetch_add(&pool->stats.connections_created, 1);
    }
    
    return conn;
}

/* ============================================================================
 * Background Threads
 * ============================================================================ */

static void* eviction_thread_func(void* arg) {
    DCFConnPool* pool = (DCFConnPool*)arg;
    
    while (dcf_atomic_load(&pool->running)) {
        dcf_sleep_ms(10000);  /* Check every 10 seconds */
        
        if (!dcf_atomic_load(&pool->running)) break;
        
        dcf_rwlock_wrlock(&pool->peers_lock);
        
        uint64_t now = dcf_time_monotonic_ms();
        
        DCFPeerEntry* peer = pool->peers_head;
        while (peer) {
            DCFPooledConn* conn = peer->idle_head;
            while (conn) {
                DCFPooledConn* next = conn->next;
                
                bool evict = false;
                
                /* Check idle timeout */
                if (pool->config.idle_timeout_ms > 0 &&
                    now - conn->last_used > pool->config.idle_timeout_ms) {
                    evict = true;
                }
                
                /* Check max lifetime */
                if (pool->config.max_lifetime_ms > 0 &&
                    now - conn->create_time > pool->config.max_lifetime_ms) {
                    evict = true;
                }
                
                if (evict) {
                    remove_from_idle(peer, conn);
                    pool->total_connections--;
                    pool->idle_connections--;
                    destroy_connection(pool, conn);
                    dcf_atomic_fetch_add(&pool->stats.connections_evicted, 1);
                }
                
                conn = next;
            }
            peer = peer->next;
        }
        
        dcf_rwlock_unlock(&pool->peers_lock);
    }
    
    return NULL;
}

static void* health_thread_func(void* arg) {
    DCFConnPool* pool = (DCFConnPool*)arg;
    
    while (dcf_atomic_load(&pool->running)) {
        dcf_sleep_ms(pool->config.validation_interval_ms);
        
        if (!dcf_atomic_load(&pool->running)) break;
        if (!pool->config.test_on_idle) continue;
        
        dcf_rwlock_rdlock(&pool->peers_lock);
        
        DCFPeerEntry* peer = pool->peers_head;
        while (peer) {
            DCFPooledConn* conn = peer->idle_head;
            while (conn) {
                DCFPooledConn* next = conn->next;
                
                dcf_atomic_fetch_add(&pool->stats.validations_total, 1);
                
                bool healthy = true;
                if (pool->config.validator) {
                    healthy = pool->config.validator(conn, pool->config.callback_data);
                }
                
                if (!healthy) {
                    dcf_atomic_fetch_add(&pool->stats.validations_failed, 1);
                    /* Mark for eviction on next pass */
                    conn->state = DCF_CONN_FAILED;
                }
                
                conn = next;
            }
            peer = peer->next;
        }
        
        dcf_rwlock_unlock(&pool->peers_lock);
    }
    
    return NULL;
}

/* ============================================================================
 * Lifecycle
 * ============================================================================ */

DCFConnPool* dcf_connpool_create(const DCFConnPoolConfig* config) {
    DCFConnPool* pool = dcf_calloc(1, sizeof(DCFConnPool));
    if (!pool) return NULL;
    
    if (config) {
        memcpy(&pool->config, config, sizeof(DCFConnPoolConfig));
    } else {
        DCFConnPoolConfig defaults = DCF_CONNPOOL_CONFIG_DEFAULT;
        memcpy(&pool->config, &defaults, sizeof(DCFConnPoolConfig));
    }
    
    dcf_rwlock_init(&pool->peers_lock);
    dcf_mutex_init(&pool->state_mutex);
    dcf_cond_init(&pool->conn_available);
    
    dcf_atomic_init(&pool->running, false);
    dcf_atomic_init(&pool->draining, false);
    dcf_atomic_init(&pool->pending_acquisitions, 0);
    
    return pool;
}

void dcf_connpool_destroy(DCFConnPool* pool, bool graceful) {
    if (!pool) return;
    
    /* Stop background threads */
    dcf_connpool_stop(pool);
    
    /* Wait for pending acquisitions if graceful */
    if (graceful) {
        while (dcf_atomic_load(&pool->pending_acquisitions) > 0) {
            dcf_sleep_ms(10);
        }
    }
    
    /* Close all connections */
    dcf_connpool_close_all(pool);
    
    /* Free peer entries */
    DCFPeerEntry* peer = pool->peers_head;
    while (peer) {
        DCFPeerEntry* next = peer->next;
        dcf_free(peer);
        peer = next;
    }
    
    dcf_cond_destroy(&pool->conn_available);
    dcf_mutex_destroy(&pool->state_mutex);
    dcf_rwlock_destroy(&pool->peers_lock);
    
    dcf_free(pool);
}

DCFError dcf_connpool_start(DCFConnPool* pool) {
    if (!pool) return DCF_ERR_NULL_PTR;
    if (dcf_atomic_load(&pool->running)) return DCF_ERR_ALREADY_RUNNING;
    
    dcf_atomic_store(&pool->running, true);
    
    if (pool->config.enable_background_eviction) {
        if (dcf_thread_create(&pool->eviction_thread, eviction_thread_func, pool) != 0) {
            dcf_atomic_store(&pool->running, false);
            return DCF_ERR_THREAD_CREATE_FAIL;
        }
    }
    
    if (pool->config.test_on_idle) {
        if (dcf_thread_create(&pool->health_thread, health_thread_func, pool) != 0) {
            /* Continue anyway, health checking is optional */
        }
    }
    
    return DCF_SUCCESS;
}

DCFError dcf_connpool_stop(DCFConnPool* pool) {
    if (!pool) return DCF_ERR_NULL_PTR;
    if (!dcf_atomic_load(&pool->running)) return DCF_SUCCESS;
    
    dcf_atomic_store(&pool->running, false);
    
    /* Wake up waiters */
    dcf_mutex_lock(&pool->state_mutex);
    dcf_cond_broadcast(&pool->conn_available);
    dcf_mutex_unlock(&pool->state_mutex);
    
    /* Wait for threads */
    if (pool->config.enable_background_eviction) {
        dcf_thread_join(pool->eviction_thread, NULL);
    }
    
    return DCF_SUCCESS;
}

DCFError dcf_connpool_drain(DCFConnPool* pool, uint32_t timeout_ms) {
    if (!pool) return DCF_ERR_NULL_PTR;
    
    dcf_atomic_store(&pool->draining, true);
    
    uint64_t deadline = dcf_time_monotonic_ms() + timeout_ms;
    
    while (pool->active_connections > 0) {
        if (dcf_time_monotonic_ms() >= deadline) {
            return DCF_ERR_TIMEOUT;
        }
        dcf_sleep_ms(10);
    }
    
    return DCF_SUCCESS;
}

/* ============================================================================
 * Connection Acquisition
 * ============================================================================ */

DCFPooledConn* dcf_connpool_acquire(DCFConnPool* pool, const char* peer_id,
                                     int32_t timeout_ms) {
    if (!pool || !peer_id) return NULL;
    if (dcf_atomic_load(&pool->draining)) return NULL;
    
    uint32_t actual_timeout = (timeout_ms == 0) ? 
        pool->config.acquire_timeout_ms : (uint32_t)timeout_ms;
    uint64_t deadline = dcf_time_monotonic_ms() + actual_timeout;
    
    dcf_atomic_fetch_add(&pool->pending_acquisitions, 1);
    dcf_atomic_fetch_add(&pool->stats.acquisitions_total, 1);
    
    dcf_timer_t timer;
    dcf_timer_start(&timer);
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    
    DCFPeerEntry* peer = get_or_create_peer(pool, peer_id);
    if (!peer) {
        dcf_rwlock_unlock(&pool->peers_lock);
        dcf_atomic_fetch_sub(&pool->pending_acquisitions, 1);
        dcf_atomic_fetch_add(&pool->stats.acquisitions_failed, 1);
        return NULL;
    }
    
    /* Check circuit breaker */
    if (!check_circuit_breaker(pool, peer)) {
        dcf_rwlock_unlock(&pool->peers_lock);
        dcf_atomic_fetch_sub(&pool->pending_acquisitions, 1);
        dcf_atomic_fetch_add(&pool->stats.acquisitions_failed, 1);
        DCF_SET_ERROR(DCF_ERR_CIRCUIT_OPEN, "Circuit open for peer %s", peer_id);
        return NULL;
    }
    
    DCFPooledConn* conn = NULL;
    
    /* Try to get idle connection */
    while (!conn) {
        if (peer->idle_head) {
            conn = peer->idle_head;
            remove_from_idle(peer, conn);
            pool->idle_connections--;
            peer->active_count++;
            pool->active_connections++;
            
            /* Validate if configured */
            if (pool->config.validate_on_acquire && pool->config.validator) {
                if (!pool->config.validator(conn, pool->config.callback_data)) {
                    destroy_connection(pool, conn);
                    pool->total_connections--;
                    peer->active_count--;
                    pool->active_connections--;
                    conn = NULL;
                    continue;
                }
            }
            break;
        }
        
        /* Create new connection if under limit */
        if (pool->total_connections < pool->config.max_connections &&
            (peer->idle_count + peer->active_count) < pool->config.max_per_peer) {
            
            dcf_rwlock_unlock(&pool->peers_lock);
            
            conn = create_connection(pool, peer_id);
            
            dcf_rwlock_wrlock(&pool->peers_lock);
            peer = find_peer(pool, peer_id);
            
            if (conn && peer) {
                pool->total_connections++;
                peer->active_count++;
                pool->active_connections++;
            } else if (conn) {
                destroy_connection(pool, conn);
                conn = NULL;
            }
            break;
        }
        
        /* Wait for connection to become available */
        if (timeout_ms < 0 || dcf_time_monotonic_ms() < deadline) {
            dcf_rwlock_unlock(&pool->peers_lock);
            
            dcf_mutex_lock(&pool->state_mutex);
            dcf_cond_timedwait(&pool->conn_available, &pool->state_mutex, 100);
            dcf_mutex_unlock(&pool->state_mutex);
            
            dcf_rwlock_wrlock(&pool->peers_lock);
            peer = find_peer(pool, peer_id);
            if (!peer) break;
        } else {
            dcf_atomic_fetch_add(&pool->stats.acquisitions_timed_out, 1);
            break;
        }
    }
    
    dcf_rwlock_unlock(&pool->peers_lock);
    
    dcf_timer_stop(&timer);
    dcf_atomic_fetch_add(&pool->stats.acquisition_wait_time_us, 
                         (uint64_t)dcf_timer_elapsed_ms(&timer) * 1000);
    
    dcf_atomic_fetch_sub(&pool->pending_acquisitions, 1);
    
    if (conn) {
        conn->last_used = dcf_time_monotonic_ms();
        conn->use_count++;
        conn->state = DCF_CONN_CONNECTED;
        dcf_atomic_fetch_add(&pool->stats.acquisitions_succeeded, 1);
    } else {
        dcf_atomic_fetch_add(&pool->stats.acquisitions_failed, 1);
    }
    
    return conn;
}

void dcf_connpool_release(DCFConnPool* pool, DCFPooledConn* conn, bool is_healthy) {
    if (!pool || !conn) return;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    
    DCFPeerEntry* peer = find_peer(pool, conn->peer_id);
    if (!peer) {
        dcf_rwlock_unlock(&pool->peers_lock);
        destroy_connection(pool, conn);
        return;
    }
    
    peer->active_count--;
    pool->active_connections--;
    
    if (!is_healthy || dcf_atomic_load(&pool->draining)) {
        pool->total_connections--;
        destroy_connection(pool, conn);
    } else {
        /* Validate if configured */
        if (pool->config.validate_on_release && pool->config.validator) {
            if (!pool->config.validator(conn, pool->config.callback_data)) {
                pool->total_connections--;
                destroy_connection(pool, conn);
                goto done;
            }
        }
        
        conn->last_used = dcf_time_monotonic_ms();
        conn->state = DCF_CONN_CONNECTED;
        add_to_idle(peer, conn);
        pool->idle_connections++;
    }
    
done:
    /* Signal waiters */
    dcf_mutex_lock(&pool->state_mutex);
    dcf_cond_signal(&pool->conn_available);
    dcf_mutex_unlock(&pool->state_mutex);
    
    dcf_rwlock_unlock(&pool->peers_lock);
}

void dcf_connpool_discard(DCFConnPool* pool, DCFPooledConn* conn) {
    dcf_connpool_release(pool, conn, false);
}

/* ============================================================================
 * Circuit Breaker
 * ============================================================================ */

DCFCircuitState dcf_connpool_get_circuit_state(DCFConnPool* pool, const char* peer_id) {
    if (!pool || !peer_id) return DCF_CIRCUIT_CLOSED;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    DCFPeerEntry* peer = find_peer(pool, peer_id);
    DCFCircuitState state = DCF_CIRCUIT_CLOSED;
    
    if (peer) {
        /* Check if circuit should transition to HALF_OPEN */
        if (peer->circuit_state == DCF_CIRCUIT_OPEN) {
            uint64_t now = dcf_time_monotonic_ms();
            if (now - peer->circuit_opened_at >= pool->config.circuit.timeout_ms) {
                peer->circuit_state = DCF_CIRCUIT_HALF_OPEN;
                dcf_atomic_fetch_add(&pool->stats.circuit_half_opens, 1);
            }
        }
        state = peer->circuit_state;
    }
    
    dcf_rwlock_unlock(&pool->peers_lock);
    return state;
}

void dcf_connpool_open_circuit(DCFConnPool* pool, const char* peer_id) {
    if (!pool || !peer_id) return;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    DCFPeerEntry* peer = find_peer(pool, peer_id);
    if (peer) {
        peer->circuit_state = DCF_CIRCUIT_OPEN;
        peer->circuit_opened_at = dcf_time_monotonic_ms();
        dcf_atomic_fetch_add(&pool->stats.circuit_opens, 1);
    }
    dcf_rwlock_unlock(&pool->peers_lock);
}

void dcf_connpool_reset_circuit(DCFConnPool* pool, const char* peer_id) {
    if (!pool || !peer_id) return;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    DCFPeerEntry* peer = find_peer(pool, peer_id);
    if (peer) {
        peer->circuit_state = DCF_CIRCUIT_CLOSED;
        peer->consecutive_failures = 0;
        peer->consecutive_successes = 0;
        dcf_atomic_fetch_add(&pool->stats.circuit_closes, 1);
    }
    dcf_rwlock_unlock(&pool->peers_lock);
}

void dcf_connpool_record_success(DCFConnPool* pool, const char* peer_id,
                                  uint32_t latency_us) {
    if (!pool || !peer_id) return;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    DCFPeerEntry* peer = find_peer(pool, peer_id);
    if (peer) {
        peer->last_success_time = dcf_time_monotonic_ms();
        peer->consecutive_failures = 0;
        peer->consecutive_successes++;
        peer->total_latency_us += latency_us;
        peer->request_count++;
        
        if (peer->circuit_state == DCF_CIRCUIT_HALF_OPEN) {
            if (peer->consecutive_successes >= pool->config.circuit.success_threshold) {
                peer->circuit_state = DCF_CIRCUIT_CLOSED;
                dcf_atomic_fetch_add(&pool->stats.circuit_closes, 1);
                DCF_LOG_INFO("Circuit closed for peer %s", peer_id);
            }
        }
    }
    dcf_rwlock_unlock(&pool->peers_lock);
}

void dcf_connpool_record_failure(DCFConnPool* pool, const char* peer_id) {
    if (!pool || !peer_id) return;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    DCFPeerEntry* peer = get_or_create_peer(pool, peer_id);
    if (peer) {
        peer->last_failure_time = dcf_time_monotonic_ms();
        peer->consecutive_failures++;
        peer->consecutive_successes = 0;
        
        /* Record in sliding window */
        peer->failure_times[peer->failure_idx % 100] = peer->last_failure_time;
        peer->failure_idx++;
        
        /* Check if we should open the circuit */
        if (peer->circuit_state == DCF_CIRCUIT_CLOSED) {
            if (peer->consecutive_failures >= pool->config.circuit.failure_threshold) {
                peer->circuit_state = DCF_CIRCUIT_OPEN;
                peer->circuit_opened_at = dcf_time_monotonic_ms();
                dcf_atomic_fetch_add(&pool->stats.circuit_opens, 1);
                DCF_LOG_WARN("Circuit opened for peer %s after %u failures",
                             peer_id, peer->consecutive_failures);
            }
        } else if (peer->circuit_state == DCF_CIRCUIT_HALF_OPEN) {
            /* Any failure in half-open re-opens the circuit */
            peer->circuit_state = DCF_CIRCUIT_OPEN;
            peer->circuit_opened_at = dcf_time_monotonic_ms();
            dcf_atomic_fetch_add(&pool->stats.circuit_opens, 1);
        }
    }
    dcf_rwlock_unlock(&pool->peers_lock);
}

/* ============================================================================
 * Pool Management
 * ============================================================================ */

DCFError dcf_connpool_warmup(DCFConnPool* pool, const char* peer_id, size_t count) {
    if (!pool || !peer_id) return DCF_ERR_INVALID_ARG;
    
    for (size_t i = 0; i < count; i++) {
        DCFPooledConn* conn = create_connection(pool, peer_id);
        if (!conn) break;
        
        dcf_rwlock_wrlock(&pool->peers_lock);
        DCFPeerEntry* peer = get_or_create_peer(pool, peer_id);
        if (peer) {
            add_to_idle(peer, conn);
            pool->total_connections++;
            pool->idle_connections++;
        } else {
            destroy_connection(pool, conn);
        }
        dcf_rwlock_unlock(&pool->peers_lock);
    }
    
    return DCF_SUCCESS;
}

size_t dcf_connpool_evict_idle(DCFConnPool* pool) {
    if (!pool) return 0;
    
    size_t evicted = 0;
    uint64_t now = dcf_time_monotonic_ms();
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    
    DCFPeerEntry* peer = pool->peers_head;
    while (peer) {
        DCFPooledConn* conn = peer->idle_head;
        while (conn) {
            DCFPooledConn* next = conn->next;
            
            if (pool->config.idle_timeout_ms > 0 &&
                now - conn->last_used > pool->config.idle_timeout_ms) {
                remove_from_idle(peer, conn);
                pool->total_connections--;
                pool->idle_connections--;
                destroy_connection(pool, conn);
                evicted++;
            }
            
            conn = next;
        }
        peer = peer->next;
    }
    
    dcf_rwlock_unlock(&pool->peers_lock);
    
    return evicted;
}

size_t dcf_connpool_close_peer(DCFConnPool* pool, const char* peer_id, bool graceful) {
    if (!pool || !peer_id) return 0;
    
    size_t closed = 0;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    
    DCFPeerEntry* peer = find_peer(pool, peer_id);
    if (peer) {
        while (peer->idle_head) {
            DCFPooledConn* conn = peer->idle_head;
            remove_from_idle(peer, conn);
            pool->total_connections--;
            pool->idle_connections--;
            destroy_connection(pool, conn);
            closed++;
        }
    }
    
    dcf_rwlock_unlock(&pool->peers_lock);
    
    (void)graceful; /* TODO: wait for active connections if graceful */
    
    return closed;
}

void dcf_connpool_close_all(DCFConnPool* pool) {
    if (!pool) return;
    
    dcf_rwlock_wrlock(&pool->peers_lock);
    
    DCFPeerEntry* peer = pool->peers_head;
    while (peer) {
        while (peer->idle_head) {
            DCFPooledConn* conn = peer->idle_head;
            remove_from_idle(peer, conn);
            destroy_connection(pool, conn);
        }
        peer = peer->next;
    }
    
    pool->total_connections = pool->active_connections;
    pool->idle_connections = 0;
    
    dcf_rwlock_unlock(&pool->peers_lock);
}

/* ============================================================================
 * Statistics
 * ============================================================================ */

void dcf_connpool_get_stats(const DCFConnPool* pool, DCFConnPoolStats* stats) {
    if (!pool || !stats) return;
    
    memcpy(stats, &pool->stats, sizeof(DCFConnPoolStats));
    stats->total_connections = pool->total_connections;
    stats->idle_connections = pool->idle_connections;
    stats->active_connections = pool->active_connections;
    stats->pending_acquisitions = dcf_atomic_load(&pool->pending_acquisitions);
}

void dcf_connpool_reset_stats(DCFConnPool* pool) {
    if (!pool) return;
    memset(&pool->stats, 0, sizeof(DCFConnPoolStats));
}

/* ============================================================================
 * Connection Accessors
 * ============================================================================ */

DCFConnState dcf_conn_get_state(const DCFPooledConn* conn) {
    return conn ? (DCFConnState)conn->state : DCF_CONN_DISCONNECTED;
}

const char* dcf_conn_get_peer(const DCFPooledConn* conn) {
    return conn ? conn->peer_id : NULL;
}

dcf_socket_t dcf_conn_get_socket(const DCFPooledConn* conn) {
    return conn ? conn->socket : DCF_INVALID_SOCKET;
}

uint64_t dcf_conn_get_create_time(const DCFPooledConn* conn) {
    return conn ? conn->create_time : 0;
}

uint64_t dcf_conn_get_last_used(const DCFPooledConn* conn) {
    return conn ? conn->last_used : 0;
}

uint64_t dcf_conn_get_age_ms(const DCFPooledConn* conn) {
    return conn ? dcf_time_monotonic_ms() - conn->create_time : 0;
}

void* dcf_conn_get_context(const DCFPooledConn* conn) {
    return conn ? conn->user_context : NULL;
}

void dcf_conn_set_context(DCFPooledConn* conn, void* ctx) {
    if (conn) conn->user_context = ctx;
}

ITransport* dcf_conn_get_transport(DCFPooledConn* conn) {
    return conn ? conn->transport : NULL;
}

/* ============================================================================
 * Retry Helper
 * ============================================================================ */

uint32_t dcf_retry_calc_delay(const DCFRetryConfig* config, uint32_t attempt) {
    if (!config || attempt == 0) return 0;
    
    uint32_t delay = config->base_delay_ms;
    
    switch (config->strategy) {
        case DCF_RETRY_NONE:
            return 0;
            
        case DCF_RETRY_FIXED:
            break;
            
        case DCF_RETRY_LINEAR:
            delay *= attempt;
            break;
            
        case DCF_RETRY_EXPONENTIAL:
            delay *= (1 << (attempt - 1));
            break;
            
        case DCF_RETRY_JITTER: {
            delay *= (1 << (attempt - 1));
            uint32_t jitter_range = (uint32_t)(delay * config->jitter_factor);
            if (jitter_range > 0) {
                delay += dcf_random_u32() % jitter_range - jitter_range / 2;
            }
            break;
        }
    }
    
    return DCF_MIN(delay, config->max_delay_ms);
}

bool dcf_retry_should_retry(const DCFRetryConfig* config, DCFError err, uint32_t attempt) {
    if (!config || config->strategy == DCF_RETRY_NONE) return false;
    if (config->max_attempts > 0 && attempt >= config->max_attempts) return false;
    return dcf_error_is_retriable(err);
}

DCFError dcf_retry_execute(const DCFRetryConfig* config, DCFRetryableOp operation,
                            void* context) {
    if (!config || !operation) return DCF_ERR_INVALID_ARG;
    
    DCFError err = DCF_SUCCESS;
    uint32_t attempt = 0;
    
    do {
        attempt++;
        err = operation(context);
        
        if (err == DCF_SUCCESS) return DCF_SUCCESS;
        
        if (!dcf_retry_should_retry(config, err, attempt)) break;
        
        uint32_t delay = dcf_retry_calc_delay(config, attempt);
        if (delay > 0) dcf_sleep_ms(delay);
        
    } while (true);
    
    return err;
}
