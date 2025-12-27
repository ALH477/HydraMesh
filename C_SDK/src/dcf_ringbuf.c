/**
 * @file dcf_ringbuf.c
 * @brief Lock-free Ring Buffer Implementation
 * @version 5.2.0
 */

#include "dcf_ringbuf.h"
#include "dcf_platform.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* ============================================================================
 * Ring Buffer Structure
 * ============================================================================ */

struct DCFRingBuf {
    /* Configuration */
    DCFRingBufConfig config;
    
    /* Buffer */
    uint8_t* buffer;
    size_t capacity;
    size_t mask;      /* capacity - 1 for power-of-2 optimization */
    
    /* Indices (cache-line aligned to prevent false sharing) */
    DCF_ALIGNED(64) dcf_atomic_uint64 write_idx;
    DCF_ALIGNED(64) dcf_atomic_uint64 read_idx;
    
    /* MPMC synchronization */
    dcf_mutex_t mutex;
    dcf_cond_t not_empty;
    dcf_cond_t not_full;
    bool mpmc_mode;
    
    /* State */
    dcf_atomic_bool shutdown;
    dcf_atomic_bool backpressure_active;
    
    /* Statistics */
    DCFRingBufStats stats;
};

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static inline size_t roundup_pow2(size_t n) {
    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
#if SIZE_MAX > 0xFFFFFFFF
    n |= n >> 32;
#endif
    return n + 1;
}

static inline size_t rb_used_internal(const DCFRingBuf* rb) {
    uint64_t write = dcf_atomic_load(&rb->write_idx);
    uint64_t read = dcf_atomic_load(&rb->read_idx);
    return (size_t)(write - read);
}

static inline size_t rb_available_internal(const DCFRingBuf* rb) {
    return rb->capacity - rb_used_internal(rb);
}

/* ============================================================================
 * Lifecycle
 * ============================================================================ */

DCFRingBuf* dcf_ringbuf_create(const DCFRingBufConfig* config) {
    DCFRingBufConfig cfg;
    if (config) {
        cfg = *config;
    } else {
        DCFRingBufConfig defaults = DCF_RINGBUF_CONFIG_DEFAULT;
        cfg = defaults;
    }
    
    /* Validate and adjust capacity to power of 2 */
    if (cfg.capacity < 16) cfg.capacity = 16;
    size_t actual_capacity = roundup_pow2(cfg.capacity);
    
    /* Allocate structure */
    DCFRingBuf* rb = dcf_calloc(1, sizeof(DCFRingBuf));
    if (!rb) return NULL;
    
    /* Allocate buffer (cache-line aligned) */
    rb->buffer = dcf_aligned_alloc(64, actual_capacity);
    if (!rb->buffer) {
        dcf_free(rb);
        return NULL;
    }
    
    rb->config = cfg;
    rb->capacity = actual_capacity;
    rb->mask = actual_capacity - 1;
    rb->mpmc_mode = (cfg.mode == DCF_RINGBUF_MPMC);
    
    dcf_atomic_init(&rb->write_idx, 0);
    dcf_atomic_init(&rb->read_idx, 0);
    dcf_atomic_init(&rb->shutdown, false);
    dcf_atomic_init(&rb->backpressure_active, false);
    
    if (rb->mpmc_mode) {
        dcf_mutex_init(&rb->mutex);
        dcf_cond_init(&rb->not_empty);
        dcf_cond_init(&rb->not_full);
    }
    
    /* Initialize watermarks */
    if (cfg.high_watermark == 0) cfg.high_watermark = actual_capacity * 8 / 10;
    if (cfg.low_watermark == 0) cfg.low_watermark = actual_capacity * 2 / 10;
    
    return rb;
}

void dcf_ringbuf_destroy(DCFRingBuf* rb) {
    if (!rb) return;
    
    /* Signal shutdown */
    dcf_atomic_store(&rb->shutdown, true);
    
    if (rb->mpmc_mode) {
        dcf_cond_broadcast(&rb->not_empty);
        dcf_cond_broadcast(&rb->not_full);
        dcf_cond_destroy(&rb->not_empty);
        dcf_cond_destroy(&rb->not_full);
        dcf_mutex_destroy(&rb->mutex);
    }
    
    dcf_aligned_free(rb->buffer);
    dcf_free(rb);
}

void dcf_ringbuf_reset(DCFRingBuf* rb) {
    if (!rb) return;
    
    if (rb->mpmc_mode) dcf_mutex_lock(&rb->mutex);
    
    dcf_atomic_store(&rb->write_idx, 0);
    dcf_atomic_store(&rb->read_idx, 0);
    dcf_atomic_store(&rb->backpressure_active, false);
    
    if (rb->mpmc_mode) {
        dcf_cond_broadcast(&rb->not_full);
        dcf_mutex_unlock(&rb->mutex);
    }
}

/* ============================================================================
 * Fixed-Size Operations
 * ============================================================================ */

DCFError dcf_ringbuf_write(DCFRingBuf* rb, const void* item) {
    return dcf_ringbuf_write_timeout(rb, item, 0);
}

DCFError dcf_ringbuf_write_timeout(DCFRingBuf* rb, const void* item, int32_t timeout_ms) {
    if (!rb || !item || rb->config.item_size == 0) {
        return DCF_ERR_INVALID_ARG;
    }
    
    size_t item_size = rb->config.item_size;
    
    if (rb->mpmc_mode) {
        dcf_mutex_lock(&rb->mutex);
        
        /* Wait for space */
        while (rb_available_internal(rb) < item_size) {
            if (dcf_atomic_load(&rb->shutdown)) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_CANCELLED;
            }
            
            if (rb->config.overflow == DCF_RINGBUF_DROP_NEW) {
                dcf_atomic_fetch_add(&rb->stats.items_dropped, 1);
                dcf_mutex_unlock(&rb->mutex);
                return DCF_ERR_QUEUE_FULL;
            }
            
            if (timeout_ms == 0) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_WOULD_BLOCK;
            }
            
            dcf_atomic_fetch_add(&rb->stats.waits, 1);
            if (timeout_ms < 0) {
                dcf_cond_wait(&rb->not_full, &rb->mutex);
            } else {
                int ret = dcf_cond_timedwait(&rb->not_full, &rb->mutex, timeout_ms);
                if (ret == ETIMEDOUT) {
                    dcf_mutex_unlock(&rb->mutex);
                    return DCF_ERR_TIMEOUT;
                }
            }
        }
        
        /* Write item */
        uint64_t write = dcf_atomic_load(&rb->write_idx);
        size_t offset = (size_t)(write & rb->mask);
        
        /* Handle wrap-around */
        if (offset + item_size <= rb->capacity) {
            memcpy(rb->buffer + offset, item, item_size);
        } else {
            size_t first = rb->capacity - offset;
            memcpy(rb->buffer + offset, item, first);
            memcpy(rb->buffer, (const uint8_t*)item + first, item_size - first);
        }
        
        dcf_atomic_store(&rb->write_idx, write + item_size);
        dcf_atomic_fetch_add(&rb->stats.items_written, 1);
        dcf_atomic_fetch_add(&rb->stats.bytes_written, item_size);
        
        /* Update peak */
        size_t used = rb_used_internal(rb);
        uint64_t peak = dcf_atomic_load(&rb->stats.peak_usage);
        while (used > peak) {
            if (dcf_atomic_compare_exchange(&rb->stats.peak_usage, &peak, used)) break;
        }
        
        /* Check backpressure */
        if (used >= rb->config.high_watermark) {
            dcf_atomic_store(&rb->backpressure_active, true);
        }
        
        dcf_cond_signal(&rb->not_empty);
        dcf_mutex_unlock(&rb->mutex);
        
    } else {
        /* SPSC lock-free path */
        if (rb_available_internal(rb) < item_size) {
            if (rb->config.overflow == DCF_RINGBUF_DROP_NEW) {
                dcf_atomic_fetch_add(&rb->stats.items_dropped, 1);
                return DCF_ERR_QUEUE_FULL;
            }
            return DCF_WOULD_BLOCK;
        }
        
        uint64_t write = dcf_atomic_load(&rb->write_idx);
        size_t offset = (size_t)(write & rb->mask);
        
        if (offset + item_size <= rb->capacity) {
            memcpy(rb->buffer + offset, item, item_size);
        } else {
            size_t first = rb->capacity - offset;
            memcpy(rb->buffer + offset, item, first);
            memcpy(rb->buffer, (const uint8_t*)item + first, item_size - first);
        }
        
        dcf_atomic_thread_fence();
        dcf_atomic_store(&rb->write_idx, write + item_size);
        dcf_atomic_fetch_add(&rb->stats.items_written, 1);
        dcf_atomic_fetch_add(&rb->stats.bytes_written, item_size);
    }
    
    return DCF_SUCCESS;
}

DCFError dcf_ringbuf_read(DCFRingBuf* rb, void* item) {
    return dcf_ringbuf_read_timeout(rb, item, 0);
}

DCFError dcf_ringbuf_read_timeout(DCFRingBuf* rb, void* item, int32_t timeout_ms) {
    if (!rb || !item || rb->config.item_size == 0) {
        return DCF_ERR_INVALID_ARG;
    }
    
    size_t item_size = rb->config.item_size;
    
    if (rb->mpmc_mode) {
        dcf_mutex_lock(&rb->mutex);
        
        /* Wait for data */
        while (rb_used_internal(rb) < item_size) {
            if (dcf_atomic_load(&rb->shutdown)) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_CANCELLED;
            }
            
            if (timeout_ms == 0) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_WOULD_BLOCK;
            }
            
            dcf_atomic_fetch_add(&rb->stats.waits, 1);
            if (timeout_ms < 0) {
                dcf_cond_wait(&rb->not_empty, &rb->mutex);
            } else {
                int ret = dcf_cond_timedwait(&rb->not_empty, &rb->mutex, timeout_ms);
                if (ret == ETIMEDOUT) {
                    dcf_mutex_unlock(&rb->mutex);
                    return DCF_ERR_TIMEOUT;
                }
            }
        }
        
        /* Read item */
        uint64_t read = dcf_atomic_load(&rb->read_idx);
        size_t offset = (size_t)(read & rb->mask);
        
        if (offset + item_size <= rb->capacity) {
            memcpy(item, rb->buffer + offset, item_size);
        } else {
            size_t first = rb->capacity - offset;
            memcpy(item, rb->buffer + offset, first);
            memcpy((uint8_t*)item + first, rb->buffer, item_size - first);
        }
        
        dcf_atomic_store(&rb->read_idx, read + item_size);
        dcf_atomic_fetch_add(&rb->stats.items_read, 1);
        dcf_atomic_fetch_add(&rb->stats.bytes_read, item_size);
        
        /* Check backpressure release */
        if (rb_used_internal(rb) <= rb->config.low_watermark) {
            dcf_atomic_store(&rb->backpressure_active, false);
        }
        
        dcf_cond_signal(&rb->not_full);
        dcf_mutex_unlock(&rb->mutex);
        
    } else {
        /* SPSC lock-free path */
        if (rb_used_internal(rb) < item_size) {
            dcf_atomic_fetch_add(&rb->stats.underflows, 1);
            return DCF_EOF;
        }
        
        uint64_t read = dcf_atomic_load(&rb->read_idx);
        size_t offset = (size_t)(read & rb->mask);
        
        if (offset + item_size <= rb->capacity) {
            memcpy(item, rb->buffer + offset, item_size);
        } else {
            size_t first = rb->capacity - offset;
            memcpy(item, rb->buffer + offset, first);
            memcpy((uint8_t*)item + first, rb->buffer, item_size - first);
        }
        
        dcf_atomic_thread_fence();
        dcf_atomic_store(&rb->read_idx, read + item_size);
        dcf_atomic_fetch_add(&rb->stats.items_read, 1);
        dcf_atomic_fetch_add(&rb->stats.bytes_read, item_size);
    }
    
    return DCF_SUCCESS;
}

DCFError dcf_ringbuf_peek(DCFRingBuf* rb, void* item) {
    if (!rb || !item || rb->config.item_size == 0) {
        return DCF_ERR_INVALID_ARG;
    }
    
    size_t item_size = rb->config.item_size;
    
    if (rb->mpmc_mode) dcf_mutex_lock(&rb->mutex);
    
    if (rb_used_internal(rb) < item_size) {
        if (rb->mpmc_mode) dcf_mutex_unlock(&rb->mutex);
        return DCF_EOF;
    }
    
    uint64_t read = dcf_atomic_load(&rb->read_idx);
    size_t offset = (size_t)(read & rb->mask);
    
    if (offset + item_size <= rb->capacity) {
        memcpy(item, rb->buffer + offset, item_size);
    } else {
        size_t first = rb->capacity - offset;
        memcpy(item, rb->buffer + offset, first);
        memcpy((uint8_t*)item + first, rb->buffer, item_size - first);
    }
    
    if (rb->mpmc_mode) dcf_mutex_unlock(&rb->mutex);
    
    return DCF_SUCCESS;
}

/* ============================================================================
 * Variable-Size Operations
 * ============================================================================ */

DCFError dcf_ringbuf_write_bytes(DCFRingBuf* rb, const void* data, size_t size) {
    return dcf_ringbuf_write_bytes_timeout(rb, data, size, 0);
}

DCFError dcf_ringbuf_write_bytes_timeout(DCFRingBuf* rb, const void* data,
                                          size_t size, int32_t timeout_ms) {
    if (!rb || !data) return DCF_ERR_INVALID_ARG;
    if (size == 0) return DCF_SUCCESS;
    
    /* Include length prefix */
    size_t total = sizeof(uint32_t) + size;
    if (total > rb->capacity / 2) return DCF_ERR_MESSAGE_TOO_LARGE;
    
    if (rb->mpmc_mode) {
        dcf_mutex_lock(&rb->mutex);
        
        while (rb_available_internal(rb) < total) {
            if (dcf_atomic_load(&rb->shutdown)) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_CANCELLED;
            }
            
            if (timeout_ms == 0) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_WOULD_BLOCK;
            }
            
            if (timeout_ms < 0) {
                dcf_cond_wait(&rb->not_full, &rb->mutex);
            } else {
                if (dcf_cond_timedwait(&rb->not_full, &rb->mutex, timeout_ms) == ETIMEDOUT) {
                    dcf_mutex_unlock(&rb->mutex);
                    return DCF_ERR_TIMEOUT;
                }
            }
        }
        
        uint64_t write = dcf_atomic_load(&rb->write_idx);
        size_t offset = (size_t)(write & rb->mask);
        
        /* Write length prefix */
        uint32_t len32 = (uint32_t)size;
        for (size_t i = 0; i < sizeof(len32); i++) {
            rb->buffer[(offset + i) & rb->mask] = ((uint8_t*)&len32)[i];
        }
        offset = (offset + sizeof(len32)) & rb->mask;
        
        /* Write data */
        for (size_t i = 0; i < size; i++) {
            rb->buffer[(offset + i) & rb->mask] = ((const uint8_t*)data)[i];
        }
        
        dcf_atomic_store(&rb->write_idx, write + total);
        dcf_atomic_fetch_add(&rb->stats.items_written, 1);
        dcf_atomic_fetch_add(&rb->stats.bytes_written, size);
        
        dcf_cond_signal(&rb->not_empty);
        dcf_mutex_unlock(&rb->mutex);
    } else {
        /* SPSC path - similar logic without locking */
        if (rb_available_internal(rb) < total) return DCF_WOULD_BLOCK;
        
        uint64_t write = dcf_atomic_load(&rb->write_idx);
        size_t offset = (size_t)(write & rb->mask);
        
        uint32_t len32 = (uint32_t)size;
        for (size_t i = 0; i < sizeof(len32); i++) {
            rb->buffer[(offset + i) & rb->mask] = ((uint8_t*)&len32)[i];
        }
        offset = (offset + sizeof(len32)) & rb->mask;
        
        for (size_t i = 0; i < size; i++) {
            rb->buffer[(offset + i) & rb->mask] = ((const uint8_t*)data)[i];
        }
        
        dcf_atomic_thread_fence();
        dcf_atomic_store(&rb->write_idx, write + total);
        dcf_atomic_fetch_add(&rb->stats.items_written, 1);
        dcf_atomic_fetch_add(&rb->stats.bytes_written, size);
    }
    
    return DCF_SUCCESS;
}

DCFError dcf_ringbuf_read_bytes(DCFRingBuf* rb, void* data, size_t* size) {
    return dcf_ringbuf_read_bytes_timeout(rb, data, size, 0);
}

DCFError dcf_ringbuf_read_bytes_timeout(DCFRingBuf* rb, void* data,
                                         size_t* size, int32_t timeout_ms) {
    if (!rb || !size) return DCF_ERR_INVALID_ARG;
    
    if (rb->mpmc_mode) dcf_mutex_lock(&rb->mutex);
    
    /* Check for length prefix */
    while (rb_used_internal(rb) < sizeof(uint32_t)) {
        if (dcf_atomic_load(&rb->shutdown)) {
            if (rb->mpmc_mode) dcf_mutex_unlock(&rb->mutex);
            return DCF_CANCELLED;
        }
        
        if (!rb->mpmc_mode || timeout_ms == 0) {
            if (rb->mpmc_mode) dcf_mutex_unlock(&rb->mutex);
            return DCF_EOF;
        }
        
        if (timeout_ms < 0) {
            dcf_cond_wait(&rb->not_empty, &rb->mutex);
        } else {
            if (dcf_cond_timedwait(&rb->not_empty, &rb->mutex, timeout_ms) == ETIMEDOUT) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_ERR_TIMEOUT;
            }
        }
    }
    
    uint64_t read = dcf_atomic_load(&rb->read_idx);
    size_t offset = (size_t)(read & rb->mask);
    
    /* Read length prefix */
    uint32_t len32 = 0;
    for (size_t i = 0; i < sizeof(len32); i++) {
        ((uint8_t*)&len32)[i] = rb->buffer[(offset + i) & rb->mask];
    }
    
    size_t msg_size = len32;
    size_t total = sizeof(uint32_t) + msg_size;
    
    /* Check if full message available */
    if (rb_used_internal(rb) < total) {
        if (rb->mpmc_mode) dcf_mutex_unlock(&rb->mutex);
        return DCF_EOF;
    }
    
    /* Check buffer size */
    if (data && *size < msg_size) {
        *size = msg_size;
        if (rb->mpmc_mode) dcf_mutex_unlock(&rb->mutex);
        return DCF_ERR_BUFFER_TOO_SMALL;
    }
    
    *size = msg_size;
    offset = (offset + sizeof(uint32_t)) & rb->mask;
    
    /* Read data */
    if (data) {
        for (size_t i = 0; i < msg_size; i++) {
            ((uint8_t*)data)[i] = rb->buffer[(offset + i) & rb->mask];
        }
    }
    
    dcf_atomic_store(&rb->read_idx, read + total);
    dcf_atomic_fetch_add(&rb->stats.items_read, 1);
    dcf_atomic_fetch_add(&rb->stats.bytes_read, msg_size);
    
    if (rb->mpmc_mode) {
        dcf_cond_signal(&rb->not_full);
        dcf_mutex_unlock(&rb->mutex);
    }
    
    return DCF_SUCCESS;
}

/* ============================================================================
 * Status Functions
 * ============================================================================ */

bool dcf_ringbuf_is_empty(const DCFRingBuf* rb) {
    return rb ? (rb_used_internal(rb) == 0) : true;
}

bool dcf_ringbuf_is_full(const DCFRingBuf* rb) {
    return rb ? (rb_available_internal(rb) == 0) : true;
}

size_t dcf_ringbuf_used(const DCFRingBuf* rb) {
    return rb ? rb_used_internal(rb) : 0;
}

size_t dcf_ringbuf_available(const DCFRingBuf* rb) {
    return rb ? rb_available_internal(rb) : 0;
}

size_t dcf_ringbuf_capacity(const DCFRingBuf* rb) {
    return rb ? rb->capacity : 0;
}

size_t dcf_ringbuf_count(const DCFRingBuf* rb) {
    if (!rb || rb->config.item_size == 0) return 0;
    return rb_used_internal(rb) / rb->config.item_size;
}

bool dcf_ringbuf_has_backpressure(const DCFRingBuf* rb) {
    return rb ? dcf_atomic_load(&rb->backpressure_active) : false;
}

void dcf_ringbuf_get_stats(const DCFRingBuf* rb, DCFRingBufStats* stats) {
    if (rb && stats) memcpy(stats, &rb->stats, sizeof(DCFRingBufStats));
}

void dcf_ringbuf_reset_stats(DCFRingBuf* rb) {
    if (!rb) return;
    memset(&rb->stats, 0, sizeof(DCFRingBufStats));
}

/* ============================================================================
 * Waiting and Signaling
 * ============================================================================ */

DCFError dcf_ringbuf_wait_readable(DCFRingBuf* rb, int32_t timeout_ms) {
    if (!rb) return DCF_ERR_INVALID_ARG;
    if (!rb->mpmc_mode) return DCF_ERR_INVALID_STATE;
    
    dcf_mutex_lock(&rb->mutex);
    while (rb_used_internal(rb) == 0) {
        if (dcf_atomic_load(&rb->shutdown)) {
            dcf_mutex_unlock(&rb->mutex);
            return DCF_CANCELLED;
        }
        if (timeout_ms < 0) {
            dcf_cond_wait(&rb->not_empty, &rb->mutex);
        } else {
            if (dcf_cond_timedwait(&rb->not_empty, &rb->mutex, timeout_ms) == ETIMEDOUT) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_ERR_TIMEOUT;
            }
        }
    }
    dcf_mutex_unlock(&rb->mutex);
    return DCF_SUCCESS;
}

DCFError dcf_ringbuf_wait_writable(DCFRingBuf* rb, int32_t timeout_ms) {
    if (!rb) return DCF_ERR_INVALID_ARG;
    if (!rb->mpmc_mode) return DCF_ERR_INVALID_STATE;
    
    dcf_mutex_lock(&rb->mutex);
    while (rb_available_internal(rb) == 0) {
        if (dcf_atomic_load(&rb->shutdown)) {
            dcf_mutex_unlock(&rb->mutex);
            return DCF_CANCELLED;
        }
        if (timeout_ms < 0) {
            dcf_cond_wait(&rb->not_full, &rb->mutex);
        } else {
            if (dcf_cond_timedwait(&rb->not_full, &rb->mutex, timeout_ms) == ETIMEDOUT) {
                dcf_mutex_unlock(&rb->mutex);
                return DCF_ERR_TIMEOUT;
            }
        }
    }
    dcf_mutex_unlock(&rb->mutex);
    return DCF_SUCCESS;
}

void dcf_ringbuf_signal_all(DCFRingBuf* rb) {
    if (!rb) return;
    
    dcf_atomic_store(&rb->shutdown, true);
    
    if (rb->mpmc_mode) {
        dcf_mutex_lock(&rb->mutex);
        dcf_cond_broadcast(&rb->not_empty);
        dcf_cond_broadcast(&rb->not_full);
        dcf_mutex_unlock(&rb->mutex);
    }
}

int dcf_ringbuf_get_fd(const DCFRingBuf* rb) {
    (void)rb;
    return -1; /* Not supported in this implementation */
}
