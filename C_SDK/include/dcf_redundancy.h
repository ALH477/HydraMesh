/**
 * @file dcf_redundancy.h
 * @brief Redundancy and Health Monitoring for DCF
 * 
 * Features:
 * - Automatic peer health monitoring
 * - RTT-based peer grouping
 * - Optimal route selection
 * - Failure detection and recovery
 * - Thread-safe operations
 */

#ifndef DCF_REDUNDANCY_H
#define DCF_REDUNDANCY_H

#include "dcf_types.h"
#include "dcf_error.h"
#include "dcf_config.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct DCFNetworking DCFNetworking;

/* ============================================================================
 * Peer Information
 * ============================================================================ */

typedef struct DCFPeerInfo {
    char* address;
    DCFPeerStatus status;
    DCFPeerGroup group;
    int rtt_ms;
    int rtt_avg_ms;         /* Moving average RTT */
    int rtt_min_ms;
    int rtt_max_ms;
    uint64_t last_check_time;
    uint64_t last_success_time;
    uint32_t consecutive_failures;
    uint32_t total_checks;
    uint32_t successful_checks;
} DCFPeerInfo;

/* ============================================================================
 * Redundancy Statistics
 * ============================================================================ */

typedef struct DCFRedundancyStats {
    size_t total_peers;
    size_t healthy_peers;
    size_t degraded_peers;
    size_t unreachable_peers;
    uint64_t total_health_checks;
    uint64_t failed_health_checks;
    uint64_t route_calculations;
    uint64_t failovers;
} DCFRedundancyStats;

/* ============================================================================
 * Lifecycle Functions
 * ============================================================================ */

/**
 * @brief Create new redundancy manager
 * @return New instance or NULL on failure
 */
DCF_API DCFRedundancy* dcf_redundancy_new(void);

/**
 * @brief Initialize redundancy manager
 * @param redundancy Redundancy manager
 * @param config Configuration
 * @param networking Networking component
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_initialize(DCFRedundancy* redundancy,
                                            DCFConfig* config,
                                            DCFNetworking* networking);

/**
 * @brief Start redundancy monitoring
 * @param redundancy Redundancy manager
 * @param mode Operating mode
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_start(DCFRedundancy* redundancy, DCFMode mode);

/**
 * @brief Stop redundancy monitoring
 * @param redundancy Redundancy manager
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_stop(DCFRedundancy* redundancy);

/**
 * @brief Free redundancy manager
 * @param redundancy Redundancy manager
 */
DCF_API void dcf_redundancy_free(DCFRedundancy* redundancy);

/* ============================================================================
 * Health Checking
 * ============================================================================ */

/**
 * @brief Perform health check on specific peer
 * @param redundancy Redundancy manager
 * @param peer Peer address
 * @param rtt_out RTT in milliseconds (output)
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_health_check(DCFRedundancy* redundancy,
                                              const char* peer, int* rtt_out);

/**
 * @brief Perform health check on all peers
 * @param redundancy Redundancy manager
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_health_check_all(DCFRedundancy* redundancy);

/**
 * @brief Set health check callback
 */
DCF_API DCFError dcf_redundancy_set_health_callback(DCFRedundancy* redundancy,
                                                     DCFHealthCallback callback,
                                                     void* user_data);

/* ============================================================================
 * Routing
 * ============================================================================ */

/**
 * @brief Get optimal route to recipient
 * @param redundancy Redundancy manager
 * @param recipient Target recipient
 * @param route_out Optimal route (caller must free)
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_get_optimal_route(DCFRedundancy* redundancy,
                                                   const char* recipient,
                                                   char** route_out);

/**
 * @brief Get all available routes to recipient
 * @param redundancy Redundancy manager
 * @param recipient Target recipient
 * @param routes_out Array of routes (caller must free each and array)
 * @param count_out Number of routes
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_get_routes(DCFRedundancy* redundancy,
                                            const char* recipient,
                                            char*** routes_out, size_t* count_out);

/* ============================================================================
 * Peer Management
 * ============================================================================ */

/**
 * @brief Group peers by RTT
 * @param redundancy Redundancy manager
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_group_peers(DCFRedundancy* redundancy);

/**
 * @brief Get peer information
 * @param redundancy Redundancy manager
 * @param peer Peer address
 * @param info_out Peer information (output)
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_get_peer_info(DCFRedundancy* redundancy,
                                               const char* peer,
                                               DCFPeerInfo* info_out);

/**
 * @brief Get all peer information
 * @param redundancy Redundancy manager
 * @param peers_out Array of peer info (caller must free)
 * @param count_out Number of peers
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_get_all_peers(DCFRedundancy* redundancy,
                                               DCFPeerInfo** peers_out,
                                               size_t* count_out);

/**
 * @brief Free peer info array
 */
DCF_API void dcf_redundancy_free_peer_info(DCFPeerInfo* peers, size_t count);

/**
 * @brief Get peer group string name
 */
DCF_API const char* dcf_redundancy_group_str(DCFPeerGroup group);

/**
 * @brief Get peer status string name
 */
DCF_API const char* dcf_redundancy_status_str(DCFPeerStatus status);

/* ============================================================================
 * Failure Simulation (Testing)
 * ============================================================================ */

/**
 * @brief Simulate peer failure
 * @param redundancy Redundancy manager
 * @param peer Peer to mark as failed
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_simulate_failure(DCFRedundancy* redundancy,
                                                  const char* peer);

/**
 * @brief Clear simulated failure
 * @param redundancy Redundancy manager
 * @param peer Peer to restore
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_redundancy_clear_failure(DCFRedundancy* redundancy,
                                               const char* peer);

/* ============================================================================
 * Statistics
 * ============================================================================ */

/**
 * @brief Get redundancy statistics
 */
DCF_API DCFError dcf_redundancy_get_stats(const DCFRedundancy* redundancy,
                                           DCFRedundancyStats* stats_out);

/**
 * @brief Reset redundancy statistics
 */
DCF_API DCFError dcf_redundancy_reset_stats(DCFRedundancy* redundancy);

/* ============================================================================
 * Status Callback
 * ============================================================================ */

/**
 * @brief Set peer status change callback
 */
DCF_API DCFError dcf_redundancy_set_status_callback(DCFRedundancy* redundancy,
                                                     DCFPeerStatusCallback callback,
                                                     void* user_data);

#ifdef __cplusplus
}
#endif

#endif /* DCF_REDUNDANCY_H */
