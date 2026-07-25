/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_regressions.c — pinned regression tests for the fixed crashers and
 * races (review items C4, H2, C6-class), designed to run under ASan and TSan:
 * the assertions catch functional regressions, the sanitizers catch the
 * use-after-free / data-race component the assertions can't see.
 *
 * Build: part of the CMake test suite (target test_regressions); run all
 * three sanitizer configs via -DDCF_SAN=address|thread.
 */
/* dcf_platform.h must come first: it defines _GNU_SOURCE/_POSIX_C_SOURCE,
 * which are ignored if a system header is seen before it. */
#include "dcf_platform.h"
#include "dcf_error.h"
#include "dcf_connpool.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int g_fails = 0;
#define CHECK(cond, msg)                                                     \
    do {                                                                     \
        if (cond) {                                                          \
            printf("PASS: %s\n", msg);                                      \
        } else {                                                             \
            printf("FAIL: %s (%s:%d)\n", msg, __FILE__, __LINE__);          \
            g_fails++;                                                       \
        }                                                                    \
    } while (0)

/* --- shared mock connection plumbing (mirrors test_suite.c) --------------- */

static DCFPooledConn* mock_factory(const char* peer, void* user_data) {
    (void)user_data;
    DCFPooledConn* conn = dcf_calloc(1, sizeof(DCFPooledConn));
    if (conn) {
        DCF_SAFE_STRCPY(conn->peer_id, peer, sizeof(conn->peer_id));
        conn->socket = DCF_INVALID_SOCKET;
        conn->state = DCF_CONN_CONNECTED;
    }
    return conn;
}

static void mock_destructor(DCFPooledConn* conn, void* user_data) {
    (void)user_data;
    dcf_free(conn);
}

static bool mock_validator(DCFPooledConn* conn, void* user_data) {
    (void)user_data;
    return conn && conn->state == DCF_CONN_CONNECTED;
}

/* --- C4: health-thread lifecycle ------------------------------------------ *
 * The original bug: stop() joined only the eviction thread, so destroy()
 * freed the pool while the health thread still read pool->running. With
 * test_on_idle=true (the default) this loop is exactly the reported path;
 * ASan flags the use-after-free if the join regresses. */
static void regress_c4_health_thread_lifecycle(void) {
    for (int i = 0; i < 25; i++) {
        DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
        cfg.factory = mock_factory;
        cfg.destructor = mock_destructor;
        cfg.validator = mock_validator;
        cfg.test_on_idle = true;
        cfg.enable_background_eviction = true;

        DCFConnPool* pool = dcf_connpool_create(&cfg);
        if (!pool) { g_fails++; return; }
        dcf_connpool_start(pool);

        DCFPooledConn* conn = dcf_connpool_acquire(pool, "peer-c4", 1000);
        if (conn) dcf_connpool_release(pool, conn, true);

        dcf_connpool_stop(pool);
        dcf_connpool_destroy(pool, true);
    }
    CHECK(1, "C4: 25x start/acquire/stop/destroy with health thread");
}

/* --- H2: global max_connections under contention -------------------------- *
 * The original bug: the cap was checked under the lock but the slot was taken
 * after unlocking for the factory, so two threads could both pass the check.
 * Eight threads hammer one peer while the main thread asserts the cap is
 * never exceeded in the pool's own accounting. */
#define H2_CAP 4
#define H2_THREADS 8

typedef struct {
    DCFConnPool* pool;
    DCFPooledConn* got;
} h2_arg_t;

static void* h2_worker(void* p) {
    h2_arg_t* a = (h2_arg_t*)p;
    a->got = dcf_connpool_acquire(a->pool, "peer-h2", 150);
    if (a->got) {
        dcf_sleep_ms(300); /* hold across the sampling window */
        dcf_connpool_release(a->pool, a->got, true);
    }
    return NULL;
}

static void regress_h2_max_connections_mt(void) {
    DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
    cfg.factory = mock_factory;
    cfg.destructor = mock_destructor;
    cfg.validator = mock_validator;
    cfg.max_connections = H2_CAP;
    cfg.max_per_peer = H2_THREADS; /* per-peer cap above global so global binds */

    DCFConnPool* pool = dcf_connpool_create(&cfg);
    if (!pool) { g_fails++; return; }
    dcf_connpool_start(pool);

    dcf_thread_t threads[H2_THREADS];
    h2_arg_t args[H2_THREADS];
    for (int i = 0; i < H2_THREADS; i++) {
        args[i].pool = pool;
        args[i].got = NULL;
        dcf_thread_create(&threads[i], h2_worker, &args[i]);
    }

    /* Sample the cap invariant while the workers hold connections. */
    size_t max_seen = 0;
    for (int s = 0; s < 10; s++) {
        dcf_sleep_ms(20);
        DCFConnPoolStats stats;
        dcf_connpool_get_stats(pool, &stats);
        if (stats.total_connections > max_seen) max_seen = stats.total_connections;
    }

    int successes = 0;
    for (int i = 0; i < H2_THREADS; i++) {
        dcf_thread_join(threads[i], NULL);
        if (args[i].got) successes++;
    }

    CHECK(max_seen <= H2_CAP, "H2: total_connections never exceeds max_connections");
    CHECK(successes >= 1 && successes <= H2_CAP,
          "H2: concurrent acquires bounded by the global cap");

    dcf_connpool_stop(pool);
    dcf_connpool_destroy(pool, true);
}

/* --- stats/drain concurrency smoke ---------------------------------------- *
 * get_stats and drain used to read the counters without the lock; under TSan
 * this loop is the regression tripwire. */
static void* churn_worker(void* p) {
    DCFConnPool* pool = (DCFConnPool*)p;
    for (int i = 0; i < 200; i++) {
        DCFPooledConn* c = dcf_connpool_acquire(pool, "peer-churn", 100);
        if (c) dcf_connpool_release(pool, c, true);
    }
    return NULL;
}

static void regress_stats_drain_concurrent(void) {
    DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
    cfg.factory = mock_factory;
    cfg.destructor = mock_destructor;
    cfg.validator = mock_validator;

    DCFConnPool* pool = dcf_connpool_create(&cfg);
    if (!pool) { g_fails++; return; }
    dcf_connpool_start(pool);

    dcf_thread_t threads[4];
    for (int i = 0; i < 4; i++) dcf_thread_create(&threads[i], churn_worker, pool);

    for (int i = 0; i < 100; i++) {
        DCFConnPoolStats stats;
        dcf_connpool_get_stats(pool, &stats);
        if (i == 50) dcf_connpool_reset_stats(pool);
    }

    for (int i = 0; i < 4; i++) dcf_thread_join(threads[i], NULL);

    CHECK(dcf_connpool_drain(pool, 1000) == DCF_SUCCESS,
          "drain completes once all connections are released");

    dcf_connpool_stop(pool);
    dcf_connpool_destroy(pool, true);
}

/* --- C6-class: serial receive must fail closed on read errors ------------- *
 * The plugin stored read()'s signed return in a size_t, so -1 became
 * SIZE_MAX and uninitialized heap was returned as a "received" buffer.
 * Compile the plugin into this TU and exercise the error and EOF paths. */
#include "../../plugins/serial_transport.c"

static void regress_c6_serial_read_error(void) {
    SerialTransport st;
    memset(&st, 0, sizeof(st));
    size_t size = 12345;

    /* read() error path: invalid fd -> read returns -1 -> must yield NULL */
    st.fd = -1;
    uint8_t* buf = serial_receive(&st, &size);
    CHECK(buf == NULL && size == 0, "C6: read error returns NULL, not SIZE_MAX junk");

    /* EOF path: empty pipe with closed write end -> read returns 0 -> NULL */
    int fds[2];
    if (pipe(fds) == 0) {
        close(fds[1]);
        st.fd = fds[0];
        size = 12345;
        buf = serial_receive(&st, &size);
        CHECK(buf == NULL && size == 0, "C6: EOF returns NULL");
        close(fds[0]);
    }

    /* happy path: data written into the pipe comes back verbatim */
    if (pipe(fds) == 0) {
        const uint8_t payload[5] = {0xD3, 1, 2, 3, 4};
        ssize_t w = write(fds[1], payload, sizeof(payload));
        close(fds[1]);
        st.fd = fds[0];
        buf = serial_receive(&st, &size);
        CHECK(w == (ssize_t)sizeof(payload) && buf && size == sizeof(payload) &&
              memcmp(buf, payload, sizeof(payload)) == 0,
              "C6: happy-path read returns the written bytes");
        free(buf);
        close(fds[0]);
    }
}

int main(void) {
    printf("=== DCF regression suite (C4 / H2 / C6-class / stats races) ===\n");
    regress_c4_health_thread_lifecycle();
    regress_h2_max_connections_mt();
    regress_stats_drain_concurrent();
    regress_c6_serial_read_error();
    if (g_fails == 0) printf("\nALL REGRESSIONS PINNED.\n");
    return g_fails ? 1 : 0;
}
