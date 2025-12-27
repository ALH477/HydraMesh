/**
 * @file dcf_ringbuf.h
 * @brief Lock-free Ring Buffer for High-Performance Message Queuing
 * @version 5.2.0
 *
 * Features:
 * - Lock-free SPSC (single-producer, single-consumer) mode
 * - Mutex-protected MPMC (multi-producer, multi-consumer) mode
 * - Backpressure support with configurable strategies
 * - Zero-copy batch operations
 * - Memory-efficient circular design
 * - Statistics and monitoring
 */

#ifndef DCF_RINGBUF_H
#define DCF_RINGBUF_H

#include "dcf_platform.h"
#include "dcf_error.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Ring Buffer Types
 * ============================================================================ */

typedef enum DCFRingBufMode {
    DCF_RINGBUF_SPSC = 0,    /**< Single-producer, single-consumer (lock-free) */
    DCF_RINGBUF_MPMC = 1     /**< Multi-producer, multi-consumer (locked) */
} DCFRingBufMode;

typedef enum DCFRingBufOverflow {
    DCF_RINGBUF_BLOCK     = 0,   /**< Block until space available */
    DCF_RINGBUF_DROP_NEW  = 1,   /**< Drop new items when full */
    DCF_RINGBUF_DROP_OLD  = 2,   /**< Drop oldest items when full */
    DCF_RINGBUF_OVERWRITE = 3    /**< Overwrite oldest (for fixed-size items) */
} DCFRingBufOverflow;

/* Forward declaration */
typedef struct DCFRingBuf DCFRingBuf;

/* ============================================================================
 * Ring Buffer Configuration
 * ============================================================================ */

typedef struct DCFRingBufConfig {
    size_t capacity;                  /**< Buffer capacity in bytes */
    size_t item_size;                 /**< Fixed item size (0 for variable) */
    DCFRingBufMode mode;              /**< SPSC or MPMC */
    DCFRingBufOverflow overflow;      /**< Overflow handling strategy */
    size_t high_watermark;            /**< Start backpressure signal */
    size_t low_watermark;             /**< End backpressure signal */
} DCFRingBufConfig;

#define DCF_RINGBUF_CONFIG_DEFAULT {     \
    .capacity = 1024 * 1024,             \
    .item_size = 0,                      \
    .mode = DCF_RINGBUF_MPMC,            \
    .overflow = DCF_RINGBUF_BLOCK,       \
    .high_watermark = 0,                 \
    .low_watermark = 0                   \
}

/* ============================================================================
 * Ring Buffer Statistics
 * ============================================================================ */

typedef struct DCFRingBufStats {
    dcf_atomic_uint64 items_written;
    dcf_atomic_uint64 items_read;
    dcf_atomic_uint64 bytes_written;
    dcf_atomic_uint64 bytes_read;
    dcf_atomic_uint64 items_dropped;
    dcf_atomic_uint64 bytes_dropped;
    dcf_atomic_uint64 overflows;
    dcf_atomic_uint64 underflows;
    dcf_atomic_uint64 waits;
    dcf_atomic_uint64 peak_usage;
} DCFRingBufStats;

/* ============================================================================
 * Ring Buffer Lifecycle
 * ============================================================================ */

/**
 * @brief Create a new ring buffer
 * @param config Configuration options (NULL for defaults)
 * @return Ring buffer or NULL on failure
 */
DCF_API DCFRingBuf* dcf_ringbuf_create(const DCFRingBufConfig* config);

/**
 * @brief Destroy ring buffer and free resources
 */
DCF_API void dcf_ringbuf_destroy(DCFRingBuf* rb);

/**
 * @brief Reset ring buffer to empty state
 */
DCF_API void dcf_ringbuf_reset(DCFRingBuf* rb);

/* ============================================================================
 * Fixed-Size Item Operations
 * ============================================================================ */

/**
 * @brief Write a fixed-size item to the buffer
 * @param rb Ring buffer
 * @param item Pointer to item data
 * @return DCF_SUCCESS, DCF_ERR_QUEUE_FULL, or other error
 */
DCF_API DCFError dcf_ringbuf_write(DCFRingBuf* rb, const void* item);

/**
 * @brief Write with timeout
 * @param timeout_ms Timeout in milliseconds (0 = non-blocking, -1 = infinite)
 */
DCF_API DCFError dcf_ringbuf_write_timeout(DCFRingBuf* rb, const void* item, 
                                            int32_t timeout_ms);

/**
 * @brief Read a fixed-size item from the buffer
 * @param rb Ring buffer
 * @param item Pointer to store item data
 * @return DCF_SUCCESS, DCF_EOF (empty), or other error
 */
DCF_API DCFError dcf_ringbuf_read(DCFRingBuf* rb, void* item);

/**
 * @brief Read with timeout
 */
DCF_API DCFError dcf_ringbuf_read_timeout(DCFRingBuf* rb, void* item,
                                           int32_t timeout_ms);

/**
 * @brief Peek at next item without removing it
 */
DCF_API DCFError dcf_ringbuf_peek(DCFRingBuf* rb, void* item);

/* ============================================================================
 * Variable-Size Item Operations
 * ============================================================================ */

/**
 * @brief Write variable-size data to the buffer
 * @param rb Ring buffer
 * @param data Pointer to data
 * @param size Size of data in bytes
 * @return DCF_SUCCESS, DCF_ERR_QUEUE_FULL, or other error
 */
DCF_API DCFError dcf_ringbuf_write_bytes(DCFRingBuf* rb, const void* data, size_t size);

/**
 * @brief Write bytes with timeout
 */
DCF_API DCFError dcf_ringbuf_write_bytes_timeout(DCFRingBuf* rb, const void* data,
                                                  size_t size, int32_t timeout_ms);

/**
 * @brief Read variable-size data from the buffer
 * @param rb Ring buffer
 * @param data Buffer to store data (NULL to just get size)
 * @param size Size of buffer / output: actual size read
 * @return DCF_SUCCESS, DCF_EOF, DCF_ERR_BUFFER_TOO_SMALL, or other error
 */
DCF_API DCFError dcf_ringbuf_read_bytes(DCFRingBuf* rb, void* data, size_t* size);

/**
 * @brief Read bytes with timeout
 */
DCF_API DCFError dcf_ringbuf_read_bytes_timeout(DCFRingBuf* rb, void* data,
                                                 size_t* size, int32_t timeout_ms);

/* ============================================================================
 * Batch Operations
 * ============================================================================ */

/**
 * @brief Write multiple fixed-size items
 * @param rb Ring buffer
 * @param items Array of items
 * @param count Number of items to write
 * @param written Output: number actually written
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_ringbuf_write_batch(DCFRingBuf* rb, const void* items,
                                          size_t count, size_t* written);

/**
 * @brief Read multiple fixed-size items
 * @param rb Ring buffer
 * @param items Buffer for items
 * @param count Maximum items to read
 * @param read Output: number actually read
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_ringbuf_read_batch(DCFRingBuf* rb, void* items,
                                         size_t count, size_t* read);

/* ============================================================================
 * Zero-Copy Operations
 * ============================================================================ */

/**
 * @brief Reserve space for writing (zero-copy producer)
 * @param rb Ring buffer
 * @param size Size to reserve
 * @param ptr1 First contiguous region pointer
 * @param len1 First region length
 * @param ptr2 Second region pointer (wrap-around, may be NULL)
 * @param len2 Second region length
 * @return DCF_SUCCESS or error code
 */
DCF_API DCFError dcf_ringbuf_reserve(DCFRingBuf* rb, size_t size,
                                      void** ptr1, size_t* len1,
                                      void** ptr2, size_t* len2);

/**
 * @brief Commit reserved space after writing
 * @param rb Ring buffer
 * @param size Bytes actually written
 */
DCF_API void dcf_ringbuf_commit(DCFRingBuf* rb, size_t size);

/**
 * @brief Acquire data for reading (zero-copy consumer)
 * @param rb Ring buffer
 * @param ptr1 First contiguous region pointer
 * @param len1 First region length
 * @param ptr2 Second region pointer (wrap-around, may be NULL)
 * @param len2 Second region length
 * @return Total bytes available
 */
DCF_API size_t dcf_ringbuf_acquire(DCFRingBuf* rb,
                                    const void** ptr1, size_t* len1,
                                    const void** ptr2, size_t* len2);

/**
 * @brief Release data after reading
 * @param rb Ring buffer
 * @param size Bytes consumed
 */
DCF_API void dcf_ringbuf_release(DCFRingBuf* rb, size_t size);

/* ============================================================================
 * Status and Monitoring
 * ============================================================================ */

/**
 * @brief Check if buffer is empty
 */
DCF_API bool dcf_ringbuf_is_empty(const DCFRingBuf* rb);

/**
 * @brief Check if buffer is full
 */
DCF_API bool dcf_ringbuf_is_full(const DCFRingBuf* rb);

/**
 * @brief Get number of bytes used
 */
DCF_API size_t dcf_ringbuf_used(const DCFRingBuf* rb);

/**
 * @brief Get number of bytes available for writing
 */
DCF_API size_t dcf_ringbuf_available(const DCFRingBuf* rb);

/**
 * @brief Get total capacity
 */
DCF_API size_t dcf_ringbuf_capacity(const DCFRingBuf* rb);

/**
 * @brief Get number of items in buffer (fixed-size mode)
 */
DCF_API size_t dcf_ringbuf_count(const DCFRingBuf* rb);

/**
 * @brief Check if backpressure is active
 */
DCF_API bool dcf_ringbuf_has_backpressure(const DCFRingBuf* rb);

/**
 * @brief Get statistics
 */
DCF_API void dcf_ringbuf_get_stats(const DCFRingBuf* rb, DCFRingBufStats* stats);

/**
 * @brief Reset statistics
 */
DCF_API void dcf_ringbuf_reset_stats(DCFRingBuf* rb);

/* ============================================================================
 * Waiting and Signaling
 * ============================================================================ */

/**
 * @brief Wait until buffer has data available
 * @param rb Ring buffer
 * @param timeout_ms Timeout (-1 for infinite)
 * @return DCF_SUCCESS, DCF_ERR_TIMEOUT, or DCF_CANCELLED
 */
DCF_API DCFError dcf_ringbuf_wait_readable(DCFRingBuf* rb, int32_t timeout_ms);

/**
 * @brief Wait until buffer has space available
 */
DCF_API DCFError dcf_ringbuf_wait_writable(DCFRingBuf* rb, int32_t timeout_ms);

/**
 * @brief Signal all waiters to wake up (for shutdown)
 */
DCF_API void dcf_ringbuf_signal_all(DCFRingBuf* rb);

/**
 * @brief Get file descriptor for poll/select (if available)
 * @return fd or -1 if not supported
 */
DCF_API int dcf_ringbuf_get_fd(const DCFRingBuf* rb);

#ifdef __cplusplus
}
#endif

#endif /* DCF_RINGBUF_H */
