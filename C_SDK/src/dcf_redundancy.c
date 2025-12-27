/**
 * @file dcf_redundancy.c
 * @brief Redundancy and health monitoring implementation
 * 
 * Fixed issues from original:
 * - Removed duplicate free() call
 * - Added thread safety
 * - Improved memory management
 * - Better error handling
 * - RTT measurement improvements
 */

#include "dcf_redundancy.h"
#include "dcf_networking.h"
#include "dcf_serialization.h"
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <limits.h>
#include <sys/time.h>

/* ============================================================================
 * Internal Structure
 * ============================================================================ */

typedef struct PeerEntry {
    char* address;
    DCFPeerStatus status;
    DCFPeerGroup group;
    int rtt_ms;
    int rtt_samples[10];       /* Circular buffer for averaging */
    int rtt_sample_count;
    int rtt_sample_idx;
    int rtt_min_ms;
    int rtt_max_ms;
    uint64_t last_check_time;
    uint64_t last_success_time;
    uint32_t consecutive_failures;
    uint32_t total_checks;
    uint32_t successful_checks;
    bool simulated_failure;
} PeerEntry;

struct DCFRedundancy {
    PeerEntry* peers;
    size_t peer_count;
    size_t peer_capacity;
    
    int rtt_threshold;
    int regional_threshold;
    
    DCFNetworking* networking;
    
    bool running;
    DCFMode mode;
    
    /* Thread safety */
    pthread_rwlock_t lock;
    bool lock_initialized;
    
    /* Callbacks */
    DCFHealthCallback health_callback;
    void* health_callback_data;
    DCFPeerStatusCallback status_callback;
    void* status_callback_data;
    
    /* Statistics */
    DCFRedundancyStats stats;
};

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static uint64_t get_timestamp_ms(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

static PeerEntry* find_peer(DCFRedundancy* r, const char* address) {
    for (size_t i = 0; i < r->peer_count; i++) {
        if (strcmp(r->peers[i].address, address) == 0) {
            return &r->peers[i];
        }
    }
    return NULL;
}

static void update_peer_status(DCFRedundancy* r, PeerEntry* peer, 
                                DCFPeerStatus new_status) {
    if (peer->status != new_status) {
        DCFPeerStatus old = peer->status;
        peer->status = new_status;
        
        /* Update stats */
        switch (old) {
            case DCF_PEER_HEALTHY:    r->stats.healthy_peers--; break;
            case DCF_PEER_DEGRADED:   r->stats.degraded_peers--; break;
            case DCF_PEER_UNREACHABLE: r->stats.unreachable_peers--; break;
            default: break;
        }
        switch (new_status) {
            case DCF_PEER_HEALTHY:    r->stats.healthy_peers++; break;
            case DCF_PEER_DEGRADED:   r->stats.degraded_peers++; break;
            case DCF_PEER_UNREACHABLE: r->stats.unreachable_peers++; break;
            default: break;
        }
        
        /* Notify callback */
        if (r->status_callback) {
            r->status_callback(peer->address, old, new_status, r->status_callback_data);
        }
    }
}

static void update_peer_rtt(PeerEntry* peer, int rtt_ms) {
    /* Add to circular buffer */
    peer->rtt_samples[peer->rtt_sample_idx] = rtt_ms;
    peer->rtt_sample_idx = (peer->rtt_sample_idx + 1) % 10;
    if (peer->rtt_sample_count < 10) peer->rtt_sample_count++;
    
    /* Calculate average */
    int sum = 0;
    for (int i = 0; i < peer->rtt_sample_count; i++) {
        sum += peer->rtt_samples[i];
    }
    peer->rtt_ms = sum / peer->rtt_sample_count;
    
    /* Update min/max */
    if (rtt_ms < peer->rtt_min_ms || peer->rtt_min_ms == 0) {
        peer->rtt_min_ms = rtt_ms;
    }
    if (rtt_ms > peer->rtt_max_ms) {
        peer->rtt_max_ms = rtt_ms;
    }
}

static DCFPeerGroup determine_group(int rtt_ms, int local_threshold, int regional_threshold) {
    if (rtt_ms < local_threshold) return DCF_GROUP_LOCAL;
    if (rtt_ms < regional_threshold) return DCF_GROUP_REGIONAL;
    return DCF_GROUP_REMOTE;
}

/* ============================================================================
 * Lifecycle Functions
 * ============================================================================ */

DCFRedundancy* dcf_redundancy_new(void) {
    DCFRedundancy* r = calloc(1, sizeof(DCFRedundancy));
    if (!r) {
        DCF_LOG_ERROR("Failed to allocate redundancy manager");
        return NULL;
    }
    
    if (pthread_rwlock_init(&r->lock, NULL) != 0) {
        DCF_LOG_ERROR("Failed to initialize lock");
        free(r);
        return NULL;
    }
    r->lock_initialized = true;
    
    r->peer_capacity = 16;
    r->peers = calloc(r->peer_capacity, sizeof(PeerEntry));
    if (!r->peers) {
        pthread_rwlock_destroy(&r->lock);
        free(r);
        return NULL;
    }
    
    r->rtt_threshold = DCF_DEFAULT_RTT_THRESHOLD;
    r->regional_threshold = 200;  /* Default regional threshold */
    
    DCF_LOG_DEBUG("Redundancy manager created");
    return r;
}

DCFError dcf_redundancy_initialize(DCFRedundancy* r, DCFConfig* config, 
                                    DCFNetworking* networking) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    DCF_RETURN_IF_NULL(config, "config is NULL");
    DCF_RETURN_IF_NULL(networking, "networking is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    
    r->networking = networking;
    r->rtt_threshold = dcf_config_get_rtt_threshold(config);
    
    /* Load peers from config */
    char** peers = NULL;
    size_t peer_count = 0;
    DCFError err = dcf_config_get_peers(config, &peers, &peer_count);
    
    if (err == DCF_SUCCESS && peers) {
        for (size_t i = 0; i < peer_count; i++) {
            /* Ensure capacity */
            if (r->peer_count >= r->peer_capacity) {
                size_t new_cap = r->peer_capacity * 2;
                PeerEntry* new_peers = realloc(r->peers, new_cap * sizeof(PeerEntry));
                if (!new_peers) {
                    DCF_LOG_ERROR("Failed to expand peer array");
                    continue;
                }
                memset(new_peers + r->peer_capacity, 0, 
                       (new_cap - r->peer_capacity) * sizeof(PeerEntry));
                r->peers = new_peers;
                r->peer_capacity = new_cap;
            }
            
            /* Add peer */
            PeerEntry* entry = &r->peers[r->peer_count];
            entry->address = strdup(peers[i]);
            entry->status = DCF_PEER_UNKNOWN;
            entry->group = DCF_GROUP_UNKNOWN;
            entry->rtt_ms = INT_MAX;
            r->peer_count++;
            r->stats.total_peers++;
            
            free(peers[i]);
        }
        free(peers);
    }
    
    pthread_rwlock_unlock(&r->lock);
    
    DCF_LOG_INFO("Redundancy initialized with %zu peers", r->peer_count);
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_start(DCFRedundancy* r, DCFMode mode) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    
    if (r->running) {
        pthread_rwlock_unlock(&r->lock);
        DCF_LOG_WARN("Redundancy already running");
        return DCF_ERR_ALREADY_RUNNING;
    }
    
    r->running = true;
    r->mode = mode;
    
    pthread_rwlock_unlock(&r->lock);
    
    /* Initial health check */
    dcf_redundancy_group_peers(r);
    
    DCF_LOG_INFO("Redundancy started in mode %d", mode);
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_stop(DCFRedundancy* r) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    
    if (!r->running) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_NOT_RUNNING;
    }
    
    r->running = false;
    
    pthread_rwlock_unlock(&r->lock);
    
    DCF_LOG_INFO("Redundancy stopped");
    return DCF_SUCCESS;
}

void dcf_redundancy_free(DCFRedundancy* r) {
    if (!r) return;
    
    DCF_LOG_DEBUG("Freeing redundancy manager");
    
    /* Free peer data */
    for (size_t i = 0; i < r->peer_count; i++) {
        free(r->peers[i].address);
    }
    free(r->peers);
    
    if (r->lock_initialized) {
        pthread_rwlock_destroy(&r->lock);
    }
    
    free(r);
    /* NOTE: Original code had duplicate free(r) here - REMOVED */
}

/* ============================================================================
 * Health Checking
 * ============================================================================ */

DCFError dcf_redundancy_health_check(DCFRedundancy* r, const char* peer, int* rtt_out) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    DCF_RETURN_IF_NULL(peer, "peer is NULL");
    DCF_RETURN_IF_NULL(rtt_out, "rtt_out is NULL");
    
    pthread_rwlock_rdlock(&r->lock);
    
    if (!r->running) {
        pthread_rwlock_unlock(&r->lock);
        DCF_SET_ERROR(DCF_ERR_NOT_RUNNING, "Redundancy not running");
        return DCF_ERR_NOT_RUNNING;
    }
    
    PeerEntry* entry = find_peer(r, peer);
    pthread_rwlock_unlock(&r->lock);
    
    if (!entry) {
        DCF_SET_ERROR(DCF_ERR_INVALID_ARG, "Unknown peer: %s", peer);
        return DCF_ERR_INVALID_ARG;
    }
    
    /* Check for simulated failure */
    if (entry->simulated_failure) {
        *rtt_out = INT_MAX;
        return DCF_ERR_PEER_UNREACHABLE;
    }
    
    /* Serialize health request */
    uint8_t* request = NULL;
    size_t req_len = 0;
    DCFError err = dcf_serialize_health_request(peer, &request, &req_len);
    if (err != DCF_SUCCESS) {
        return err;
    }
    
    /* Measure RTT */
    uint64_t start = get_timestamp_ms();
    
    err = dcf_networking_send(r->networking, request, req_len, peer);
    free(request);
    
    if (err != DCF_SUCCESS) {
        pthread_rwlock_wrlock(&r->lock);
        entry = find_peer(r, peer);  /* Re-find under write lock */
        if (entry) {
            entry->consecutive_failures++;
            entry->total_checks++;
            entry->last_check_time = get_timestamp_ms();
            
            if (entry->consecutive_failures >= 3) {
                update_peer_status(r, entry, DCF_PEER_UNREACHABLE);
                entry->group = DCF_GROUP_UNREACHABLE;
            }
        }
        r->stats.total_health_checks++;
        r->stats.failed_health_checks++;
        pthread_rwlock_unlock(&r->lock);
        
        return err;
    }
    
    /* Receive response */
    char* response = NULL;
    char* sender = NULL;
    err = dcf_networking_receive(r->networking, &response, &sender);
    
    uint64_t end = get_timestamp_ms();
    int rtt = (int)(end - start);
    
    free(response);
    free(sender);
    
    /* Update peer stats */
    pthread_rwlock_wrlock(&r->lock);
    entry = find_peer(r, peer);
    if (entry) {
        entry->total_checks++;
        entry->last_check_time = end;
        
        if (err == DCF_SUCCESS) {
            entry->successful_checks++;
            entry->consecutive_failures = 0;
            entry->last_success_time = end;
            
            update_peer_rtt(entry, rtt);
            entry->group = determine_group(entry->rtt_ms, r->rtt_threshold, 
                                           r->regional_threshold);
            
            /* Update status based on RTT stability */
            if (entry->rtt_sample_count >= 3) {
                int variance = entry->rtt_max_ms - entry->rtt_min_ms;
                if (variance < r->rtt_threshold) {
                    update_peer_status(r, entry, DCF_PEER_HEALTHY);
                } else {
                    update_peer_status(r, entry, DCF_PEER_DEGRADED);
                }
            } else {
                update_peer_status(r, entry, DCF_PEER_HEALTHY);
            }
            
            *rtt_out = entry->rtt_ms;
        } else {
            entry->consecutive_failures++;
            if (entry->consecutive_failures >= 3) {
                update_peer_status(r, entry, DCF_PEER_UNREACHABLE);
                entry->group = DCF_GROUP_UNREACHABLE;
            }
            *rtt_out = INT_MAX;
        }
    }
    
    r->stats.total_health_checks++;
    if (err != DCF_SUCCESS) {
        r->stats.failed_health_checks++;
    }
    
    pthread_rwlock_unlock(&r->lock);
    
    /* Notify callback */
    if (r->health_callback) {
        r->health_callback(peer, *rtt_out, err == DCF_SUCCESS, r->health_callback_data);
    }
    
    return err;
}

DCFError dcf_redundancy_health_check_all(DCFRedundancy* r) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    
    pthread_rwlock_rdlock(&r->lock);
    size_t count = r->peer_count;
    char** addresses = calloc(count, sizeof(char*));
    if (!addresses) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_MALLOC_FAIL;
    }
    
    for (size_t i = 0; i < count; i++) {
        addresses[i] = strdup(r->peers[i].address);
    }
    pthread_rwlock_unlock(&r->lock);
    
    DCFError last_err = DCF_SUCCESS;
    for (size_t i = 0; i < count; i++) {
        int rtt;
        DCFError err = dcf_redundancy_health_check(r, addresses[i], &rtt);
        if (err != DCF_SUCCESS) {
            last_err = err;
        }
        free(addresses[i]);
    }
    free(addresses);
    
    return last_err;
}

DCFError dcf_redundancy_set_health_callback(DCFRedundancy* r, DCFHealthCallback callback,
                                             void* user_data) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    r->health_callback = callback;
    r->health_callback_data = user_data;
    pthread_rwlock_unlock(&r->lock);
    
    return DCF_SUCCESS;
}

/* ============================================================================
 * Routing
 * ============================================================================ */

DCFError dcf_redundancy_get_optimal_route(DCFRedundancy* r, const char* recipient,
                                           char** route_out) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    DCF_RETURN_IF_NULL(recipient, "recipient is NULL");
    DCF_RETURN_IF_NULL(route_out, "route_out is NULL");
    
    pthread_rwlock_rdlock(&r->lock);
    
    if (!r->running) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_NOT_RUNNING;
    }
    
    int min_rtt = INT_MAX;
    const char* best = NULL;
    
    for (size_t i = 0; i < r->peer_count; i++) {
        PeerEntry* entry = &r->peers[i];
        
        /* Skip the recipient itself */
        if (strcmp(entry->address, recipient) == 0) continue;
        
        /* Skip unreachable/failed peers */
        if (entry->status == DCF_PEER_UNREACHABLE) continue;
        if (entry->simulated_failure) continue;
        
        if (entry->rtt_ms < min_rtt) {
            min_rtt = entry->rtt_ms;
            best = entry->address;
        }
    }
    
    if (!best) {
        pthread_rwlock_unlock(&r->lock);
        r->stats.route_calculations++;
        DCF_SET_ERROR(DCF_ERR_ROUTE_NOT_FOUND, "No route to %s", recipient);
        return DCF_ERR_ROUTE_NOT_FOUND;
    }
    
    *route_out = strdup(best);
    pthread_rwlock_unlock(&r->lock);
    
    r->stats.route_calculations++;
    
    if (!*route_out) {
        return DCF_ERR_MALLOC_FAIL;
    }
    
    DCF_LOG_DEBUG("Optimal route to %s: %s (RTT: %d ms)", recipient, *route_out, min_rtt);
    return DCF_SUCCESS;
}

/* ============================================================================
 * Peer Management
 * ============================================================================ */

DCFError dcf_redundancy_group_peers(DCFRedundancy* r) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    
    pthread_rwlock_rdlock(&r->lock);
    
    if (!r->running) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_NOT_RUNNING;
    }
    
    size_t count = r->peer_count;
    pthread_rwlock_unlock(&r->lock);
    
    /* Health check all peers to update grouping */
    for (size_t i = 0; i < count; i++) {
        pthread_rwlock_rdlock(&r->lock);
        if (i >= r->peer_count) {
            pthread_rwlock_unlock(&r->lock);
            break;
        }
        char* addr = strdup(r->peers[i].address);
        pthread_rwlock_unlock(&r->lock);
        
        if (addr) {
            int rtt;
            dcf_redundancy_health_check(r, addr, &rtt);
            free(addr);
        }
    }
    
    DCF_LOG_DEBUG("Peer grouping complete");
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_get_peer_info(DCFRedundancy* r, const char* peer,
                                       DCFPeerInfo* info_out) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    DCF_RETURN_IF_NULL(peer, "peer is NULL");
    DCF_RETURN_IF_NULL(info_out, "info_out is NULL");
    
    pthread_rwlock_rdlock(&r->lock);
    
    PeerEntry* entry = find_peer(r, peer);
    if (!entry) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_INVALID_ARG;
    }
    
    info_out->address = strdup(entry->address);
    info_out->status = entry->status;
    info_out->group = entry->group;
    info_out->rtt_ms = entry->rtt_ms;
    info_out->rtt_min_ms = entry->rtt_min_ms;
    info_out->rtt_max_ms = entry->rtt_max_ms;
    info_out->last_check_time = entry->last_check_time;
    info_out->last_success_time = entry->last_success_time;
    info_out->consecutive_failures = entry->consecutive_failures;
    info_out->total_checks = entry->total_checks;
    info_out->successful_checks = entry->successful_checks;
    
    /* Calculate average RTT */
    if (entry->rtt_sample_count > 0) {
        int sum = 0;
        for (int i = 0; i < entry->rtt_sample_count; i++) {
            sum += entry->rtt_samples[i];
        }
        info_out->rtt_avg_ms = sum / entry->rtt_sample_count;
    } else {
        info_out->rtt_avg_ms = entry->rtt_ms;
    }
    
    pthread_rwlock_unlock(&r->lock);
    return DCF_SUCCESS;
}

void dcf_redundancy_free_peer_info(DCFPeerInfo* peers, size_t count) {
    if (!peers) return;
    for (size_t i = 0; i < count; i++) {
        free(peers[i].address);
    }
    free(peers);
}

const char* dcf_redundancy_group_str(DCFPeerGroup group) {
    switch (group) {
        case DCF_GROUP_LOCAL:       return "local";
        case DCF_GROUP_REGIONAL:    return "regional";
        case DCF_GROUP_REMOTE:      return "remote";
        case DCF_GROUP_UNREACHABLE: return "unreachable";
        default:                    return "unknown";
    }
}

const char* dcf_redundancy_status_str(DCFPeerStatus status) {
    switch (status) {
        case DCF_PEER_HEALTHY:      return "healthy";
        case DCF_PEER_DEGRADED:     return "degraded";
        case DCF_PEER_UNREACHABLE:  return "unreachable";
        case DCF_PEER_CONNECTING:   return "connecting";
        case DCF_PEER_DISCONNECTED: return "disconnected";
        default:                    return "unknown";
    }
}

/* ============================================================================
 * Failure Simulation
 * ============================================================================ */

DCFError dcf_redundancy_simulate_failure(DCFRedundancy* r, const char* peer) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    DCF_RETURN_IF_NULL(peer, "peer is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    
    if (!r->running) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_NOT_RUNNING;
    }
    
    PeerEntry* entry = find_peer(r, peer);
    if (!entry) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_INVALID_ARG;
    }
    
    entry->simulated_failure = true;
    entry->rtt_ms = INT_MAX;
    update_peer_status(r, entry, DCF_PEER_UNREACHABLE);
    entry->group = DCF_GROUP_UNREACHABLE;
    r->stats.failovers++;
    
    pthread_rwlock_unlock(&r->lock);
    
    DCF_LOG_INFO("Simulated failure for peer: %s", peer);
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_clear_failure(DCFRedundancy* r, const char* peer) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    DCF_RETURN_IF_NULL(peer, "peer is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    
    PeerEntry* entry = find_peer(r, peer);
    if (!entry) {
        pthread_rwlock_unlock(&r->lock);
        return DCF_ERR_INVALID_ARG;
    }
    
    entry->simulated_failure = false;
    entry->consecutive_failures = 0;
    update_peer_status(r, entry, DCF_PEER_UNKNOWN);
    
    pthread_rwlock_unlock(&r->lock);
    
    /* Trigger health check to restore */
    int rtt;
    dcf_redundancy_health_check(r, peer, &rtt);
    
    DCF_LOG_INFO("Cleared simulated failure for peer: %s", peer);
    return DCF_SUCCESS;
}

/* ============================================================================
 * Statistics
 * ============================================================================ */

DCFError dcf_redundancy_get_stats(const DCFRedundancy* r, DCFRedundancyStats* stats_out) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    DCF_RETURN_IF_NULL(stats_out, "stats_out is NULL");
    
    pthread_rwlock_rdlock((pthread_rwlock_t*)&r->lock);
    memcpy(stats_out, &r->stats, sizeof(DCFRedundancyStats));
    pthread_rwlock_unlock((pthread_rwlock_t*)&r->lock);
    
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_reset_stats(DCFRedundancy* r) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    
    /* Keep peer counts, reset others */
    r->stats.total_health_checks = 0;
    r->stats.failed_health_checks = 0;
    r->stats.route_calculations = 0;
    r->stats.failovers = 0;
    
    pthread_rwlock_unlock(&r->lock);
    return DCF_SUCCESS;
}

DCFError dcf_redundancy_set_status_callback(DCFRedundancy* r, 
                                             DCFPeerStatusCallback callback,
                                             void* user_data) {
    DCF_RETURN_IF_NULL(r, "redundancy is NULL");
    
    pthread_rwlock_wrlock(&r->lock);
    r->status_callback = callback;
    r->status_callback_data = user_data;
    pthread_rwlock_unlock(&r->lock);
    
    return DCF_SUCCESS;
}
