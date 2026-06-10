/**
 * @file dcf_cancel.h
 * @brief Cooperative Cancellation Token System
 * @version 5.2.0
 *
 * Provides structured cancellation for:
 * - Graceful shutdown propagation
 * - Timeout enforcement
 * - User-initiated abort
 * - Hierarchical cancellation (parent cancels children)
 *
 * Usage:
 *   DCFCancelToken* token = dcf_cancel_token_create(NULL);
 *   
 *   // In operation:
 *   while (!dcf_cancel_is_requested(token)) {
 *       do_work();
 *   }
 *   
 *   // To cancel:
 *   dcf_cancel_request(token);
 *   
 *   dcf_cancel_token_free(token);
 */

#ifndef DCF_CANCEL_H
#define DCF_CANCEL_H

#include "dcf_platform.h"
#include "dcf_error.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Types
 * ============================================================================ */

typedef struct DCFCancelToken DCFCancelToken;

/**
 * @brief Cancellation reason
 */
typedef enum DCFCancelReason {
    DCF_CANCEL_NONE         = 0,
    DCF_CANCEL_REQUESTED    = 1,  /**< Explicit cancel request */
    DCF_CANCEL_TIMEOUT      = 2,  /**< Timeout expired */
    DCF_CANCEL_PARENT       = 3,  /**< Parent token cancelled */
    DCF_CANCEL_SHUTDOWN     = 4,  /**< System shutdown */
    DCF_CANCEL_ERROR        = 5   /**< Cancelled due to error */
} DCFCancelReason;

/**
 * @brief Callback invoked when cancellation is requested
 */
typedef void (*DCFCancelCallback)(DCFCancelToken* token, DCFCancelReason reason, void* user_data);

/* ============================================================================
 * Token Lifecycle
 * ============================================================================ */

/**
 * @brief Create a new cancellation token
 * @param parent Parent token (NULL for root token)
 * @return New token or NULL on failure
 * 
 * If parent is cancelled, this token is automatically cancelled.
 */
DCF_API DCFCancelToken* dcf_cancel_token_create(DCFCancelToken* parent);

/**
 * @brief Create a token that auto-cancels after timeout
 * @param parent Parent token (NULL for root)
 * @param timeout_ms Timeout in milliseconds
 * @return New token or NULL on failure
 */
DCF_API DCFCancelToken* dcf_cancel_token_create_timeout(DCFCancelToken* parent, uint32_t timeout_ms);

/**
 * @brief Create a linked token that cancels when any source cancels
 * @param sources Array of source tokens
 * @param count Number of sources
 * @return Linked token or NULL on failure
 */
DCF_API DCFCancelToken* dcf_cancel_token_link(DCFCancelToken** sources, size_t count);

/**
 * @brief Free a cancellation token
 * @note Does not affect parent or child tokens
 */
DCF_API void dcf_cancel_token_free(DCFCancelToken* token);

/**
 * @brief Add reference to token (thread-safe)
 */
DCF_API void dcf_cancel_token_ref(DCFCancelToken* token);

/**
 * @brief Release reference (free when ref count reaches 0)
 */
DCF_API void dcf_cancel_token_unref(DCFCancelToken* token);

/* ============================================================================
 * Cancellation Control
 * ============================================================================ */

/**
 * @brief Request cancellation
 * @param token Token to cancel
 * 
 * Cancellation propagates to all child tokens.
 */
DCF_API void dcf_cancel_request(DCFCancelToken* token);

/**
 * @brief Request cancellation with reason
 */
DCF_API void dcf_cancel_request_with_reason(DCFCancelToken* token, DCFCancelReason reason);

/**
 * @brief Check if cancellation has been requested
 */
DCF_API bool dcf_cancel_is_requested(const DCFCancelToken* token);

/**
 * @brief Get cancellation reason
 * @return DCF_CANCEL_NONE if not cancelled
 */
DCF_API DCFCancelReason dcf_cancel_get_reason(const DCFCancelToken* token);

/**
 * @brief Throw error if cancelled
 * @return DCF_SUCCESS or DCF_CANCELLED
 */
DCF_API DCFError dcf_cancel_check(const DCFCancelToken* token);

/**
 * @brief Reset a token (clear cancellation state)
 * @note Only valid for tokens without children
 */
DCF_API DCFError dcf_cancel_reset(DCFCancelToken* token);

/* ============================================================================
 * Waiting
 * ============================================================================ */

/**
 * @brief Wait for cancellation
 * @param token Token to wait on
 * @param timeout_ms Maximum wait time (-1 for infinite)
 * @return DCF_SUCCESS if cancelled, DCF_ERR_TIMEOUT if timed out
 */
DCF_API DCFError dcf_cancel_wait(DCFCancelToken* token, int32_t timeout_ms);

/**
 * @brief Get waitable handle (for use with poll/select)
 * @return File descriptor or -1 if not supported
 */
DCF_API int dcf_cancel_get_fd(const DCFCancelToken* token);

/* ============================================================================
 * Callbacks
 * ============================================================================ */

/**
 * @brief Register callback for when cancellation is requested
 * @param token Token to monitor
 * @param callback Function to call
 * @param user_data User data passed to callback
 * @return Registration ID for unregistering
 * 
 * Callback is invoked synchronously from the thread that requests cancellation.
 * Callback must not block.
 */
DCF_API uint32_t dcf_cancel_register_callback(DCFCancelToken* token,
                                               DCFCancelCallback callback,
                                               void* user_data);

/**
 * @brief Unregister a callback
 * @param token Token the callback was registered with
 * @param id Registration ID from register
 */
DCF_API void dcf_cancel_unregister_callback(DCFCancelToken* token, uint32_t id);

/* ============================================================================
 * Global Shutdown Token
 * ============================================================================ */

/**
 * @brief Get the global shutdown token
 * 
 * This token is cancelled when dcf_shutdown() is called.
 * Use as parent for operation tokens to enable graceful shutdown.
 */
DCF_API DCFCancelToken* dcf_shutdown_token(void);

/**
 * @brief Request global shutdown
 */
DCF_API void dcf_shutdown(void);

/**
 * @brief Check if shutdown has been requested
 */
DCF_API bool dcf_is_shutting_down(void);

/* ============================================================================
 * Utility
 * ============================================================================ */

/**
 * @brief Get string representation of cancel reason
 */
DCF_API const char* dcf_cancel_reason_str(DCFCancelReason reason);

/**
 * @brief Sleep with cancellation support
 * @param token Cancellation token
 * @param ms Sleep duration
 * @return DCF_SUCCESS or DCF_CANCELLED
 */
DCF_API DCFError dcf_sleep_cancellable(DCFCancelToken* token, uint32_t ms);

#ifdef __cplusplus
}
#endif

#endif /* DCF_CANCEL_H */
