/**
 * @file test_suite.c
 * @brief Comprehensive Test Suite for DCF
 * @version 5.2.0
 * 
 * Tests cover:
 * - Memory management and leak detection
 * - Thread safety and race conditions
 * - Ring buffer under load
 * - Connection pool circuit breaker
 * - Error handling paths
 * - Edge cases and boundary conditions
 */

#include "dcf_platform.h"
#include "dcf_error.h"
#include "dcf_types.h"
#include "dcf_ringbuf.h"
#include "dcf_connpool.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* ============================================================================
 * Test Framework
 * ============================================================================ */

#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        fprintf(stderr, "FAIL: %s:%d: %s\n", __func__, __LINE__, msg); \
        g_failed++; \
        return; \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) TEST_ASSERT((a) == (b), msg)
#define TEST_ASSERT_NE(a, b, msg) TEST_ASSERT((a) != (b), msg)
#define TEST_ASSERT_GT(a, b, msg) TEST_ASSERT((a) > (b), msg)
#define TEST_ASSERT_LT(a, b, msg) TEST_ASSERT((a) < (b), msg)
#define TEST_ASSERT_NULL(p, msg) TEST_ASSERT((p) == NULL, msg)
#define TEST_ASSERT_NOT_NULL(p, msg) TEST_ASSERT((p) != NULL, msg)

#define RUN_TEST(test) do { \
    printf("  Running %s... ", #test); \
    fflush(stdout); \
    test(); \
    if (g_failed == failed_before) { \
        printf("OK\n"); \
        g_passed++; \
    } else { \
        failed_before = g_failed; \
    } \
} while(0)

static int g_passed = 0;
static int g_failed = 0;

/* ============================================================================
 * Memory Tests
 * ============================================================================ */

static void test_memory_basic(void) {
    void* ptr = dcf_malloc(100);
    TEST_ASSERT_NOT_NULL(ptr, "malloc should succeed");
    
    memset(ptr, 0xAB, 100);
    dcf_free(ptr);
    
    /* Test stats tracking */
    const dcf_mem_stats_t* stats = dcf_mem_get_stats();
    TEST_ASSERT_GT(stats->allocation_count, 0, "should track allocations");
}

static void test_memory_calloc(void) {
    int* arr = dcf_calloc(100, sizeof(int));
    TEST_ASSERT_NOT_NULL(arr, "calloc should succeed");
    
    /* Verify zero-initialization */
    for (int i = 0; i < 100; i++) {
        TEST_ASSERT_EQ(arr[i], 0, "calloc should zero memory");
    }
    
    dcf_free(arr);
}

static void test_memory_realloc(void) {
    char* ptr = dcf_malloc(50);
    TEST_ASSERT_NOT_NULL(ptr, "malloc should succeed");
    
    strcpy(ptr, "hello");
    
    ptr = dcf_realloc(ptr, 100);
    TEST_ASSERT_NOT_NULL(ptr, "realloc should succeed");
    TEST_ASSERT_EQ(strcmp(ptr, "hello"), 0, "data should be preserved");
    
    ptr = dcf_realloc(ptr, 25);
    TEST_ASSERT_NOT_NULL(ptr, "shrink realloc should succeed");
    
    dcf_free(ptr);
}

static void test_memory_strdup(void) {
    const char* original = "test string";
    char* copy = dcf_strdup(original);
    
    TEST_ASSERT_NOT_NULL(copy, "strdup should succeed");
    TEST_ASSERT_EQ(strcmp(copy, original), 0, "strings should match");
    TEST_ASSERT_NE((void*)copy, (void*)original, "should be different pointers");
    
    dcf_free(copy);
}

static void test_memory_aligned(void) {
    void* ptr = dcf_aligned_alloc(64, 1000);
    TEST_ASSERT_NOT_NULL(ptr, "aligned alloc should succeed");
    TEST_ASSERT_EQ((uintptr_t)ptr % 64, 0, "should be 64-byte aligned");
    
    dcf_aligned_free(ptr);
}

static void test_memory_null_free(void) {
    /* Should not crash */
    dcf_free(NULL);
}

static void test_memory_zero_malloc(void) {
    void* ptr = dcf_malloc(0);
    /* Behavior is implementation-defined, but shouldn't crash */
    dcf_free(ptr);
}

static void test_secure_zero(void) {
    char buf[100];
    memset(buf, 0xAB, sizeof(buf));
    
    dcf_secure_zero(buf, sizeof(buf));
    
    for (size_t i = 0; i < sizeof(buf); i++) {
        TEST_ASSERT_EQ(buf[i], 0, "memory should be zeroed");
    }
}

/* ============================================================================
 * Ring Buffer Tests
 * ============================================================================ */

static void test_ringbuf_create_destroy(void) {
    DCFRingBuf* rb = dcf_ringbuf_create(NULL);
    TEST_ASSERT_NOT_NULL(rb, "ringbuf create should succeed");
    
    TEST_ASSERT(dcf_ringbuf_is_empty(rb), "new buffer should be empty");
    TEST_ASSERT(!dcf_ringbuf_is_full(rb), "new buffer should not be full");
    
    dcf_ringbuf_destroy(rb);
}

static void test_ringbuf_fixed_size(void) {
    DCFRingBufConfig cfg = {
        .capacity = 1024,
        .item_size = sizeof(int),
        .mode = DCF_RINGBUF_MPMC,
        .overflow = DCF_RINGBUF_DROP_NEW
    };
    
    DCFRingBuf* rb = dcf_ringbuf_create(&cfg);
    TEST_ASSERT_NOT_NULL(rb, "ringbuf create should succeed");
    
    /* Write items */
    for (int i = 0; i < 100; i++) {
        DCFError err = dcf_ringbuf_write(rb, &i);
        TEST_ASSERT_EQ(err, DCF_SUCCESS, "write should succeed");
    }
    
    TEST_ASSERT_EQ(dcf_ringbuf_count(rb), 100, "should have 100 items");
    
    /* Read items */
    for (int i = 0; i < 100; i++) {
        int val;
        DCFError err = dcf_ringbuf_read(rb, &val);
        TEST_ASSERT_EQ(err, DCF_SUCCESS, "read should succeed");
        TEST_ASSERT_EQ(val, i, "values should match");
    }
    
    TEST_ASSERT(dcf_ringbuf_is_empty(rb), "buffer should be empty");
    
    dcf_ringbuf_destroy(rb);
}

static void test_ringbuf_variable_size(void) {
    DCFRingBuf* rb = dcf_ringbuf_create(NULL);
    TEST_ASSERT_NOT_NULL(rb, "ringbuf create should succeed");
    
    const char* messages[] = {
        "Hello",
        "World",
        "This is a longer message for testing",
        "Short"
    };
    
    /* Write messages */
    for (size_t i = 0; i < sizeof(messages) / sizeof(messages[0]); i++) {
        DCFError err = dcf_ringbuf_write_bytes(rb, messages[i], strlen(messages[i]) + 1);
        TEST_ASSERT_EQ(err, DCF_SUCCESS, "write should succeed");
    }
    
    /* Read messages */
    for (size_t i = 0; i < sizeof(messages) / sizeof(messages[0]); i++) {
        char buf[256];
        size_t size = sizeof(buf);
        DCFError err = dcf_ringbuf_read_bytes(rb, buf, &size);
        TEST_ASSERT_EQ(err, DCF_SUCCESS, "read should succeed");
        TEST_ASSERT_EQ(strcmp(buf, messages[i]), 0, "message should match");
    }
    
    dcf_ringbuf_destroy(rb);
}

static void test_ringbuf_overflow_drop(void) {
    DCFRingBufConfig cfg = {
        .capacity = 64,
        .item_size = sizeof(int),
        .mode = DCF_RINGBUF_MPMC,
        .overflow = DCF_RINGBUF_DROP_NEW
    };
    
    DCFRingBuf* rb = dcf_ringbuf_create(&cfg);
    TEST_ASSERT_NOT_NULL(rb, "ringbuf create should succeed");
    
    /* Fill buffer */
    int count = 0;
    for (int i = 0; i < 1000; i++) {
        DCFError err = dcf_ringbuf_write(rb, &i);
        if (err == DCF_SUCCESS) count++;
    }
    
    TEST_ASSERT_LT(count, 1000, "some writes should fail");
    TEST_ASSERT(dcf_ringbuf_is_full(rb), "buffer should be full");
    
    /* Check stats */
    DCFRingBufStats stats;
    dcf_ringbuf_get_stats(rb, &stats);
    TEST_ASSERT_GT(stats.items_dropped, 0, "should have dropped items");
    
    dcf_ringbuf_destroy(rb);
}

static void test_ringbuf_peek(void) {
    DCFRingBufConfig cfg = {
        .capacity = 1024,
        .item_size = sizeof(int),
        .mode = DCF_RINGBUF_MPMC
    };
    
    DCFRingBuf* rb = dcf_ringbuf_create(&cfg);
    
    int val = 42;
    dcf_ringbuf_write(rb, &val);
    
    int peeked;
    DCFError err = dcf_ringbuf_peek(rb, &peeked);
    TEST_ASSERT_EQ(err, DCF_SUCCESS, "peek should succeed");
    TEST_ASSERT_EQ(peeked, 42, "peeked value should match");
    
    /* Peek again - should still be 42 */
    err = dcf_ringbuf_peek(rb, &peeked);
    TEST_ASSERT_EQ(peeked, 42, "second peek should still get same value");
    
    /* Read should get the same value */
    int read_val;
    dcf_ringbuf_read(rb, &read_val);
    TEST_ASSERT_EQ(read_val, 42, "read should get same value");
    
    dcf_ringbuf_destroy(rb);
}

/* Multi-threaded ring buffer test */
typedef struct {
    DCFRingBuf* rb;
    int thread_id;
    int count;
    int errors;
} RingBufThreadArgs;

static void* ringbuf_producer(void* arg) {
    RingBufThreadArgs* args = (RingBufThreadArgs*)arg;
    
    for (int i = 0; i < args->count; i++) {
        int val = args->thread_id * 10000 + i;
        DCFError err = dcf_ringbuf_write_timeout(args->rb, &val, 1000);
        if (err != DCF_SUCCESS) args->errors++;
    }
    
    return NULL;
}

static void* ringbuf_consumer(void* arg) {
    RingBufThreadArgs* args = (RingBufThreadArgs*)arg;
    
    int received = 0;
    while (received < args->count) {
        int val;
        DCFError err = dcf_ringbuf_read_timeout(args->rb, &val, 100);
        if (err == DCF_SUCCESS) {
            received++;
        } else if (err == DCF_ERR_TIMEOUT || err == DCF_WOULD_BLOCK) {
            /* Expected, try again */
        } else {
            args->errors++;
            break;
        }
    }
    
    return NULL;
}

static void test_ringbuf_multithreaded(void) {
    DCFRingBufConfig cfg = {
        .capacity = 4096,
        .item_size = sizeof(int),
        .mode = DCF_RINGBUF_MPMC,
        .overflow = DCF_RINGBUF_BLOCK
    };
    
    DCFRingBuf* rb = dcf_ringbuf_create(&cfg);
    TEST_ASSERT_NOT_NULL(rb, "ringbuf create should succeed");
    
    const int NUM_ITEMS = 1000;
    
    RingBufThreadArgs producer_args = { .rb = rb, .thread_id = 1, .count = NUM_ITEMS, .errors = 0 };
    RingBufThreadArgs consumer_args = { .rb = rb, .thread_id = 0, .count = NUM_ITEMS, .errors = 0 };
    
    dcf_thread_t producer, consumer;
    
    dcf_thread_create(&consumer, ringbuf_consumer, &consumer_args);
    dcf_thread_create(&producer, ringbuf_producer, &producer_args);
    
    dcf_thread_join(producer, NULL);
    
    /* Give consumer time to finish */
    dcf_sleep_ms(100);
    dcf_ringbuf_signal_all(rb);
    dcf_thread_join(consumer, NULL);
    
    TEST_ASSERT_EQ(producer_args.errors, 0, "producer should have no errors");
    
    dcf_ringbuf_destroy(rb);
}

/* ============================================================================
 * Connection Pool Tests
 * ============================================================================ */

static DCFPooledConn* test_conn_factory(const char* peer, void* user_data) {
    (void)user_data;
    DCFPooledConn* conn = dcf_calloc(1, sizeof(DCFPooledConn));
    if (conn) {
        DCF_SAFE_STRCPY(conn->peer_id, peer, sizeof(conn->peer_id));
        conn->socket = DCF_INVALID_SOCKET;
        conn->state = DCF_CONN_CONNECTED;
    }
    return conn;
}

static void test_conn_destructor(DCFPooledConn* conn, void* user_data) {
    (void)user_data;
    dcf_free(conn);
}

static bool test_conn_validator(DCFPooledConn* conn, void* user_data) {
    (void)user_data;
    return conn && conn->state == DCF_CONN_CONNECTED;
}

static void test_connpool_create_destroy(void) {
    DCFConnPool* pool = dcf_connpool_create(NULL);
    TEST_ASSERT_NOT_NULL(pool, "pool create should succeed");
    
    dcf_connpool_destroy(pool, false);
}

static void test_connpool_acquire_release(void) {
    DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
    cfg.factory = test_conn_factory;
    cfg.destructor = test_conn_destructor;
    cfg.validator = test_conn_validator;
    
    DCFConnPool* pool = dcf_connpool_create(&cfg);
    TEST_ASSERT_NOT_NULL(pool, "pool create should succeed");
    
    dcf_connpool_start(pool);
    
    /* Acquire connection */
    DCFPooledConn* conn = dcf_connpool_acquire(pool, "peer1", 1000);
    TEST_ASSERT_NOT_NULL(conn, "acquire should succeed");
    TEST_ASSERT_EQ(strcmp(dcf_conn_get_peer(conn), "peer1"), 0, "peer should match");
    
    /* Release back */
    dcf_connpool_release(pool, conn, true);
    
    /* Acquire again - should get same connection */
    DCFPooledConn* conn2 = dcf_connpool_acquire(pool, "peer1", 1000);
    TEST_ASSERT_NOT_NULL(conn2, "second acquire should succeed");
    TEST_ASSERT_EQ(conn, conn2, "should reuse connection");
    
    dcf_connpool_release(pool, conn2, true);
    
    dcf_connpool_stop(pool);
    dcf_connpool_destroy(pool, true);
}

static void test_connpool_circuit_breaker(void) {
    DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
    cfg.factory = test_conn_factory;
    cfg.destructor = test_conn_destructor;
    cfg.circuit.failure_threshold = 3;
    cfg.circuit.timeout_ms = 1000;
    cfg.circuit.success_threshold = 2;
    
    DCFConnPool* pool = dcf_connpool_create(&cfg);
    dcf_connpool_start(pool);
    
    /* Record failures */
    for (int i = 0; i < 3; i++) {
        dcf_connpool_record_failure(pool, "peer1");
    }
    
    /* Circuit should be open */
    TEST_ASSERT_EQ(dcf_connpool_get_circuit_state(pool, "peer1"), 
                   DCF_CIRCUIT_OPEN, "circuit should be open");
    
    /* Acquire should fail */
    DCFPooledConn* conn = dcf_connpool_acquire(pool, "peer1", 100);
    TEST_ASSERT_NULL(conn, "acquire should fail with open circuit");
    
    /* Wait for timeout */
    dcf_sleep_ms(1100);
    
    /* Circuit should be half-open */
    TEST_ASSERT_EQ(dcf_connpool_get_circuit_state(pool, "peer1"),
                   DCF_CIRCUIT_HALF_OPEN, "circuit should be half-open");
    
    /* Acquire should now work */
    conn = dcf_connpool_acquire(pool, "peer1", 1000);
    TEST_ASSERT_NOT_NULL(conn, "acquire should succeed in half-open");
    dcf_connpool_release(pool, conn, true);
    
    /* Record successes to close */
    dcf_connpool_record_success(pool, "peer1", 100);
    dcf_connpool_record_success(pool, "peer1", 100);
    
    TEST_ASSERT_EQ(dcf_connpool_get_circuit_state(pool, "peer1"),
                   DCF_CIRCUIT_CLOSED, "circuit should be closed");
    
    dcf_connpool_stop(pool);
    dcf_connpool_destroy(pool, true);
}

static void test_connpool_max_connections(void) {
    DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
    cfg.factory = test_conn_factory;
    cfg.destructor = test_conn_destructor;
    cfg.max_connections = 5;
    cfg.max_per_peer = 2;
    cfg.acquire_timeout_ms = 100;
    
    DCFConnPool* pool = dcf_connpool_create(&cfg);
    dcf_connpool_start(pool);
    
    DCFPooledConn* conns[10] = {0};
    int acquired = 0;
    
    /* Try to acquire more than max */
    for (int i = 0; i < 10; i++) {
        conns[i] = dcf_connpool_acquire(pool, "peer1", 100);
        if (conns[i]) acquired++;
    }
    
    TEST_ASSERT_EQ(acquired, 2, "should only get max_per_peer connections");
    
    /* Release all */
    for (int i = 0; i < 10; i++) {
        if (conns[i]) dcf_connpool_release(pool, conns[i], true);
    }
    
    dcf_connpool_stop(pool);
    dcf_connpool_destroy(pool, true);
}

static void test_connpool_discard(void) {
    DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
    cfg.factory = test_conn_factory;
    cfg.destructor = test_conn_destructor;
    
    DCFConnPool* pool = dcf_connpool_create(&cfg);
    dcf_connpool_start(pool);
    
    DCFPooledConn* conn1 = dcf_connpool_acquire(pool, "peer1", 1000);
    TEST_ASSERT_NOT_NULL(conn1, "first acquire should succeed");
    
    /* Check stats before discard */
    DCFConnPoolStats stats1;
    dcf_connpool_get_stats(pool, &stats1);
    uint64_t created_before = dcf_atomic_load(&stats1.connections_created);
    
    /* Discard instead of release */
    dcf_connpool_discard(pool, conn1);
    /* Note: conn1 is now freed, don't use it */
    
    /* Next acquire should create new connection */
    DCFPooledConn* conn2 = dcf_connpool_acquire(pool, "peer1", 1000);
    TEST_ASSERT_NOT_NULL(conn2, "second acquire should succeed");
    
    /* Verify a new connection was created */
    DCFConnPoolStats stats2;
    dcf_connpool_get_stats(pool, &stats2);
    uint64_t created_after = dcf_atomic_load(&stats2.connections_created);
    TEST_ASSERT_GT(created_after, created_before, "new connection should be created");
    
    dcf_connpool_release(pool, conn2, true);
    
    dcf_connpool_stop(pool);
    dcf_connpool_destroy(pool, true);
}

/* ============================================================================
 * Error Handling Tests
 * ============================================================================ */

static void test_error_set_get(void) {
    dcf_clear_error();
    TEST_ASSERT(!dcf_has_error(), "should have no error initially");
    
    DCF_SET_ERROR(DCF_ERR_INVALID_ARG, "test error %d", 42);
    
    TEST_ASSERT(dcf_has_error(), "should have error after set");
    
    const DCFErrorContext* ctx = dcf_get_last_error();
    TEST_ASSERT_NOT_NULL(ctx, "error context should exist");
    TEST_ASSERT_EQ(ctx->code, DCF_ERR_INVALID_ARG, "error code should match");
    TEST_ASSERT(strstr(ctx->message, "42") != NULL, "message should contain args");
    
    dcf_clear_error();
    TEST_ASSERT(!dcf_has_error(), "should have no error after clear");
}

static void test_error_wrap(void) {
    dcf_clear_error();
    
    DCF_SET_ERROR(DCF_ERR_FILE_NOT_FOUND, "original error");
    DCF_WRAP_ERROR(DCF_ERR_CONFIG_PARSE_FAIL, "wrapped error");
    
    const DCFErrorContext* ctx = dcf_get_last_error();
    TEST_ASSERT_EQ(ctx->code, DCF_ERR_CONFIG_PARSE_FAIL, "top error should be wrapped");
    TEST_ASSERT_EQ(ctx->chain_depth, 1, "should have one error in chain");
    TEST_ASSERT_EQ(ctx->chain[0].code, DCF_ERR_FILE_NOT_FOUND, "chain should have original");
    
    dcf_clear_error();
}

static void test_error_categories(void) {
    TEST_ASSERT(dcf_error_is_category(DCF_ERR_MALLOC_FAIL, DCF_ERROR_CAT_MEMORY), 
                "malloc should be memory category");
    TEST_ASSERT(dcf_error_is_category(DCF_ERR_CONNECTION_REFUSED, DCF_ERROR_CAT_NETWORK),
                "connection refused should be network category");
    TEST_ASSERT(dcf_error_is_category(DCF_ERR_TIMEOUT, DCF_ERROR_CAT_TIMEOUT),
                "timeout should be timeout category");
}

static void test_error_retriable(void) {
    TEST_ASSERT(dcf_error_is_retriable(DCF_WOULD_BLOCK), "would_block should be retriable");
    TEST_ASSERT(dcf_error_is_retriable(DCF_ERR_TIMEOUT), "timeout should be retriable");
    TEST_ASSERT(dcf_error_is_retriable(DCF_ERR_RATE_LIMITED), "rate_limited should be retriable");
    TEST_ASSERT(!dcf_error_is_retriable(DCF_ERR_INVALID_ARG), "invalid_arg should not be retriable");
    TEST_ASSERT(!dcf_error_is_retriable(DCF_ERR_PERMISSION_DENIED), "permission_denied should not be retriable");
}

/* ============================================================================
 * Retry Logic Tests
 * ============================================================================ */

static int g_retry_count = 0;

static DCFError retry_test_op(void* ctx) {
    int* max_fails = (int*)ctx;
    g_retry_count++;
    
    if (g_retry_count <= *max_fails) {
        return DCF_ERR_TIMEOUT;
    }
    return DCF_SUCCESS;
}

static void test_retry_success(void) {
    DCFRetryConfig cfg = DCF_RETRY_CONFIG_DEFAULT;
    cfg.max_attempts = 5;
    cfg.base_delay_ms = 10;
    
    g_retry_count = 0;
    int max_fails = 2;
    
    DCFError err = dcf_retry_execute(&cfg, retry_test_op, &max_fails);
    TEST_ASSERT_EQ(err, DCF_SUCCESS, "should eventually succeed");
    TEST_ASSERT_EQ(g_retry_count, 3, "should take 3 attempts");
}

static void test_retry_exhausted(void) {
    DCFRetryConfig cfg = DCF_RETRY_CONFIG_DEFAULT;
    cfg.max_attempts = 3;
    cfg.base_delay_ms = 10;
    
    g_retry_count = 0;
    int max_fails = 10;
    
    DCFError err = dcf_retry_execute(&cfg, retry_test_op, &max_fails);
    TEST_ASSERT_NE(err, DCF_SUCCESS, "should fail after max attempts");
    TEST_ASSERT_EQ(g_retry_count, 3, "should try max_attempts times");
}

static void test_retry_delay_calculation(void) {
    DCFRetryConfig cfg = {
        .strategy = DCF_RETRY_EXPONENTIAL,
        .base_delay_ms = 100,
        .max_delay_ms = 10000,
        .max_attempts = 10
    };
    
    TEST_ASSERT_EQ(dcf_retry_calc_delay(&cfg, 1), 100, "first delay should be base");
    TEST_ASSERT_EQ(dcf_retry_calc_delay(&cfg, 2), 200, "second delay should double");
    TEST_ASSERT_EQ(dcf_retry_calc_delay(&cfg, 3), 400, "third delay should double again");
    TEST_ASSERT_EQ(dcf_retry_calc_delay(&cfg, 10), 10000, "should cap at max");
}

/* ============================================================================
 * Time Functions Tests
 * ============================================================================ */

static void test_time_monotonic(void) {
    uint64_t t1 = dcf_time_monotonic_ms();
    dcf_sleep_ms(50);
    uint64_t t2 = dcf_time_monotonic_ms();
    
    TEST_ASSERT_GT(t2, t1, "time should advance");
    TEST_ASSERT_GT(t2 - t1, 40, "should sleep at least 40ms");
    TEST_ASSERT_LT(t2 - t1, 150, "should not sleep more than 150ms");
}

static void test_timer(void) {
    dcf_timer_t timer;
    dcf_timer_start(&timer);
    
    dcf_sleep_ms(50);
    
    dcf_timer_stop(&timer);
    
    double elapsed = dcf_timer_elapsed_ms(&timer);
    TEST_ASSERT_GT(elapsed, 40.0, "timer should measure at least 40ms");
    TEST_ASSERT_LT(elapsed, 150.0, "timer should not measure more than 150ms");
}

/* ============================================================================
 * Thread Safety Tests
 * ============================================================================ */

static dcf_atomic_int g_counter = 0;
static dcf_mutex_t g_counter_mutex;

static void* increment_thread(void* arg) {
    int count = *(int*)arg;
    
    for (int i = 0; i < count; i++) {
        dcf_atomic_fetch_add(&g_counter, 1);
    }
    
    return NULL;
}

static void test_atomic_operations(void) {
    dcf_atomic_init(&g_counter, 0);
    
    const int NUM_THREADS = 4;
    const int COUNT_PER_THREAD = 10000;
    
    dcf_thread_t threads[NUM_THREADS];
    int count = COUNT_PER_THREAD;
    
    for (int i = 0; i < NUM_THREADS; i++) {
        dcf_thread_create(&threads[i], increment_thread, &count);
    }
    
    for (int i = 0; i < NUM_THREADS; i++) {
        dcf_thread_join(threads[i], NULL);
    }
    
    int final = dcf_atomic_load(&g_counter);
    TEST_ASSERT_EQ(final, NUM_THREADS * COUNT_PER_THREAD, "atomic counter should be exact");
}

static void test_mutex(void) {
    dcf_mutex_init(&g_counter_mutex);
    
    /* Note: dcf uses recursive mutexes, so trylock succeeds when already held */
    TEST_ASSERT_EQ(dcf_mutex_lock(&g_counter_mutex), 0, "lock should succeed");
    /* Recursive mutex allows re-locking from same thread */
    TEST_ASSERT_EQ(dcf_mutex_trylock(&g_counter_mutex), 0, "recursive trylock should succeed");
    TEST_ASSERT_EQ(dcf_mutex_unlock(&g_counter_mutex), 0, "first unlock should succeed");
    TEST_ASSERT_EQ(dcf_mutex_unlock(&g_counter_mutex), 0, "second unlock should succeed");
    TEST_ASSERT_EQ(dcf_mutex_trylock(&g_counter_mutex), 0, "trylock should succeed when unlocked");
    TEST_ASSERT_EQ(dcf_mutex_unlock(&g_counter_mutex), 0, "unlock should succeed");
    
    dcf_mutex_destroy(&g_counter_mutex);
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

static void run_memory_tests(void) {
    int failed_before = g_failed;
    printf("\n=== Memory Tests ===\n");
    RUN_TEST(test_memory_basic);
    RUN_TEST(test_memory_calloc);
    RUN_TEST(test_memory_realloc);
    RUN_TEST(test_memory_strdup);
    RUN_TEST(test_memory_aligned);
    RUN_TEST(test_memory_null_free);
    RUN_TEST(test_memory_zero_malloc);
    RUN_TEST(test_secure_zero);
}

static void run_ringbuf_tests(void) {
    int failed_before = g_failed;
    printf("\n=== Ring Buffer Tests ===\n");
    RUN_TEST(test_ringbuf_create_destroy);
    RUN_TEST(test_ringbuf_fixed_size);
    RUN_TEST(test_ringbuf_variable_size);
    RUN_TEST(test_ringbuf_overflow_drop);
    RUN_TEST(test_ringbuf_peek);
    RUN_TEST(test_ringbuf_multithreaded);
}

static void run_connpool_tests(void) {
    int failed_before = g_failed;
    printf("\n=== Connection Pool Tests ===\n");
    RUN_TEST(test_connpool_create_destroy);
    RUN_TEST(test_connpool_acquire_release);
    RUN_TEST(test_connpool_circuit_breaker);
    RUN_TEST(test_connpool_max_connections);
    RUN_TEST(test_connpool_discard);
}

static void run_error_tests(void) {
    int failed_before = g_failed;
    printf("\n=== Error Handling Tests ===\n");
    RUN_TEST(test_error_set_get);
    RUN_TEST(test_error_wrap);
    RUN_TEST(test_error_categories);
    RUN_TEST(test_error_retriable);
}

static void run_retry_tests(void) {
    int failed_before = g_failed;
    printf("\n=== Retry Logic Tests ===\n");
    RUN_TEST(test_retry_success);
    RUN_TEST(test_retry_exhausted);
    RUN_TEST(test_retry_delay_calculation);
}

static void run_time_tests(void) {
    int failed_before = g_failed;
    printf("\n=== Time Function Tests ===\n");
    RUN_TEST(test_time_monotonic);
    RUN_TEST(test_timer);
}

static void run_thread_tests(void) {
    int failed_before = g_failed;
    printf("\n=== Thread Safety Tests ===\n");
    RUN_TEST(test_atomic_operations);
    RUN_TEST(test_mutex);
}

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;
    
    printf("DCF Test Suite v5.2.0\n");
    printf("====================\n");
    
    /* Initialize logging */
    DCFLoggerConfig log_cfg = DCF_LOGGER_CONFIG_DEFAULT;
    log_cfg.min_level = DCF_LOG_WARN;  /* Only show warnings and errors during tests */
    dcf_log_init(&log_cfg);
    
    run_memory_tests();
    run_ringbuf_tests();
    run_connpool_tests();
    run_error_tests();
    run_retry_tests();
    run_time_tests();
    run_thread_tests();
    
    printf("\n====================\n");
    printf("Results: %d passed, %d failed\n", g_passed, g_failed);
    
    /* Print memory stats */
    const dcf_mem_stats_t* stats = dcf_mem_get_stats();
    printf("\nMemory Stats:\n");
    printf("  Allocations: %llu\n", (unsigned long long)dcf_atomic_load(&stats->allocation_count));
    printf("  Frees: %llu\n", (unsigned long long)dcf_atomic_load(&stats->free_count));
    printf("  Current usage: %llu bytes\n", (unsigned long long)dcf_atomic_load(&stats->current_usage));
    printf("  Peak usage: %llu bytes\n", (unsigned long long)dcf_atomic_load(&stats->peak_usage));
    
    if (dcf_atomic_load(&stats->current_usage) > 0) {
        printf("  WARNING: Possible memory leak detected!\n");
    }
    
    dcf_log_shutdown();
    
    return g_failed > 0 ? 1 : 0;
}
