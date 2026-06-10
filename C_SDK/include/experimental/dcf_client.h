/**
 * @file dcf_client.h
 * @brief DCF Client API - Main Entry Point
 * @version 5.2.0
 */

#ifndef DCF_CLIENT_H
#define DCF_CLIENT_H

#include "dcf_platform.h"
#include "dcf_error.h"
#include "dcf_types.h"
#include "dcf_config.h"
#include "dcf_cancel.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct DCFClient DCFClient;
typedef struct DCFAsyncContext DCFAsyncContext;

typedef struct DCFClientOptions {
    char node_id[DCF_MAX_NODE_ID_LEN];
    DCFMode mode;
    char host[DCF_MAX_HOST_LEN];
    int port;
    bool use_tls;
    char tls_cert_path[DCF_MAX_PATH_LEN];
    char tls_key_path[DCF_MAX_PATH_LEN];
    char tls_ca_path[DCF_MAX_PATH_LEN];
    uint32_t connect_timeout_ms;
    uint32_t send_timeout_ms;
    uint32_t recv_timeout_ms;
    uint32_t health_check_interval_ms;
    DCFRetryConfig retry;
    DCFConnectionPoolConfig pool;
    DCFBackpressureConfig backpressure;
    DCFRateLimitConfig rate_limit;
    DCFMessageCallback on_message;
    void* on_message_data;
    DCFPeerStatusCallback on_peer_status;
    void* on_peer_status_data;
    DCFConnectionCallback on_connection;
    void* on_connection_data;
    DCFErrorCallback on_error;
    void* on_error_data;
    bool auto_reconnect;
    bool enable_compression;
    bool enable_health_checks;
    size_t max_message_size;
    size_t recv_buffer_size;
    size_t send_buffer_size;
} DCFClientOptions;

typedef struct DCFClientStats {
    dcf_atomic_uint64 messages_sent;
    dcf_atomic_uint64 messages_received;
    dcf_atomic_uint64 messages_failed;
    dcf_atomic_uint64 messages_dropped;
    dcf_atomic_uint64 bytes_sent;
    dcf_atomic_uint64 bytes_received;
    dcf_atomic_uint64 total_send_time_us;
    dcf_atomic_uint64 total_recv_time_us;
    uint32_t avg_send_time_us;
    uint32_t avg_recv_time_us;
    uint32_t max_send_time_us;
    uint32_t max_recv_time_us;
    dcf_atomic_uint64 connections_established;
    dcf_atomic_uint64 connections_failed;
    dcf_atomic_uint64 reconnections;
    uint32_t current_connections;
    dcf_atomic_uint64 health_checks;
    dcf_atomic_uint64 health_check_failures;
    uint32_t healthy_peers;
    uint32_t unhealthy_peers;
    uint32_t send_queue_size;
    uint32_t recv_queue_size;
    dcf_atomic_uint64 errors_total;
    dcf_atomic_uint64 errors_by_category[16];
    uint64_t start_time;
    uint64_t uptime_ms;
} DCFClientStats;

typedef void (*DCFAsyncCallback)(DCFAsyncContext* ctx, DCFError error,
                                  const uint8_t* response, size_t response_len,
                                  void* user_data);

/* Lifecycle */
DCF_API DCFClient* dcf_client_new(void);
DCF_API DCFClient* dcf_client_new_with_options(const DCFClientOptions* options);
DCF_API DCFError dcf_client_initialize(DCFClient* client, const char* config_path);
DCF_API DCFError dcf_client_initialize_with_config(DCFClient* client, DCFConfig* config);
DCF_API DCFError dcf_client_initialize_with_options(DCFClient* client, const DCFClientOptions* options);
DCF_API DCFError dcf_client_start(DCFClient* client);
DCF_API DCFError dcf_client_start_with_token(DCFClient* client, DCFCancelToken* token);
DCF_API DCFError dcf_client_stop(DCFClient* client, uint32_t drain_timeout_ms);
DCF_API DCFError dcf_client_stop_immediate(DCFClient* client);
DCF_API void dcf_client_free(DCFClient* client);

/* State */
DCF_API DCFClientState dcf_client_get_state(const DCFClient* client);
DCF_API bool dcf_client_is_running(const DCFClient* client);
DCF_API bool dcf_client_is_connected(const DCFClient* client);
DCF_API DCFError dcf_client_wait_state(DCFClient* client, DCFClientState state, uint32_t timeout_ms);
DCF_API DCFError dcf_client_wait_connected(DCFClient* client, uint32_t timeout_ms);

/* Synchronous Messaging */
DCF_API DCFError dcf_client_send_receive(DCFClient* client, const uint8_t* message, size_t message_len,
                                          const char* recipient, uint8_t** response, size_t* response_len,
                                          uint32_t timeout_ms);
DCF_API DCFError dcf_client_send_text(DCFClient* client, const char* message, const char* recipient, char** response);
DCF_API DCFError dcf_client_send_receive_cancellable(DCFClient* client, const uint8_t* message, size_t message_len,
                                                      const char* recipient, uint8_t** response, size_t* response_len,
                                                      DCFCancelToken* token);
DCF_API DCFError dcf_client_send_async(DCFClient* client, const uint8_t* message, size_t message_len, const char* recipient);

/* Asynchronous Messaging */
DCF_API DCFError dcf_client_send_async_callback(DCFClient* client, const uint8_t* message, size_t message_len,
                                                 const char* recipient, DCFAsyncCallback callback,
                                                 void* user_data, DCFAsyncContext** ctx);
DCF_API void dcf_async_cancel(DCFAsyncContext* ctx);
DCF_API DCFError dcf_async_wait(DCFAsyncContext* ctx, uint32_t timeout_ms);
DCF_API void dcf_async_free(DCFAsyncContext* ctx);

/* Push Receiving */
DCF_API uint32_t dcf_client_on_message(DCFClient* client, DCFMessageCallback callback, void* user_data);
DCF_API void dcf_client_off_message(DCFClient* client, uint32_t id);
DCF_API uint32_t dcf_client_on_message_from(DCFClient* client, const char* sender, DCFMessageCallback callback, void* user_data);

/* Pull Receiving */
DCF_API DCFError dcf_client_receive(DCFClient* client, uint8_t** message, size_t* message_len, char** sender, uint32_t timeout_ms);
DCF_API DCFError dcf_client_try_receive(DCFClient* client, uint8_t** message, size_t* message_len, char** sender);
DCF_API size_t dcf_client_pending_count(const DCFClient* client);

/* Peer Management */
DCF_API DCFError dcf_client_connect_peer(DCFClient* client, const char* peer_id, const char* host, int port);
DCF_API DCFError dcf_client_disconnect_peer(DCFClient* client, const char* peer_id);
DCF_API DCFError dcf_client_list_peers(DCFClient* client, char*** peers, size_t* count);
DCF_API DCFPeerStatus dcf_client_get_peer_status(DCFClient* client, const char* peer_id);
DCF_API int32_t dcf_client_get_peer_rtt(DCFClient* client, const char* peer_id);
DCF_API uint32_t dcf_client_on_peer_status(DCFClient* client, DCFPeerStatusCallback callback, void* user_data);
DCF_API void dcf_client_off_peer_status(DCFClient* client, uint32_t id);

/* Health Monitoring */
DCF_API DCFError dcf_client_health_check(DCFClient* client);
DCF_API DCFError dcf_client_health_check_peer(DCFClient* client, const char* peer_id, int32_t* rtt_ms);
DCF_API uint32_t dcf_client_on_health(DCFClient* client, DCFHealthCallback callback, void* user_data);
DCF_API void dcf_client_off_health(DCFClient* client, uint32_t id);

/* Statistics */
DCF_API void dcf_client_get_stats(const DCFClient* client, DCFClientStats* stats);
DCF_API void dcf_client_reset_stats(DCFClient* client);

/* Configuration */
DCF_API DCFConfig* dcf_client_get_config(DCFClient* client);
DCF_API const char* dcf_client_get_node_id(const DCFClient* client);
DCF_API DCFMode dcf_client_get_mode(const DCFClient* client);
DCF_API DCFError dcf_client_update_config(DCFClient* client, const char* key, const char* value);

/* Plugin Support */
DCF_API DCFError dcf_client_load_plugin(DCFClient* client, const char* path);
DCF_API DCFError dcf_client_unload_plugin(DCFClient* client, const char* name);
DCF_API DCFError dcf_client_set_transport(DCFClient* client, ITransport* transport);
DCF_API ITransport* dcf_client_get_transport(DCFClient* client);

/* Utility */
DCF_API const char* dcf_client_get_last_error(const DCFClient* client);
DCF_API void dcf_client_dump_state(const DCFClient* client);
DCF_API DCFCancelToken* dcf_client_get_cancel_token(DCFClient* client);

#ifdef __cplusplus
}
#endif

#endif /* DCF_CLIENT_H */
