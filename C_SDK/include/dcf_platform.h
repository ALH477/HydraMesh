/**
 * @file dcf_platform.h
 * @brief Platform Abstraction Layer for DCF
 * @version 5.2.0
 *
 * Provides cross-platform abstractions for:
 * - Threading primitives (mutex, rwlock, condvar, atomics)
 * - Time and timing functions
 * - Memory allocation with tracking
 * - Socket operations
 * - Dynamic library loading
 * - Compiler intrinsics and hints
 *
 * Supported platforms:
 * - Linux (glibc 2.17+, musl)
 * - macOS (10.13+)
 * - Windows (Vista+, MSVC 2019+)
 * - FreeBSD (12+)
 */

#ifndef DCF_PLATFORM_H
#define DCF_PLATFORM_H

/* Feature test macros - must be first */
#if !defined(_WIN32) && !defined(_WIN64)
    #ifndef _GNU_SOURCE
        #define _GNU_SOURCE 1
    #endif
    #ifndef _POSIX_C_SOURCE
        #define _POSIX_C_SOURCE 200809L
    #endif
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <limits.h>

/* Ensure pthread is available on POSIX systems */
#if !defined(_WIN32) && !defined(_WIN64)
    #include <pthread.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Platform Detection
 * ============================================================================ */

#if defined(_WIN32) || defined(_WIN64)
    #define DCF_PLATFORM_WINDOWS 1
    #define DCF_PLATFORM_NAME "windows"
    #ifndef WIN32_LEAN_AND_MEAN
        #define WIN32_LEAN_AND_MEAN
    #endif
    #ifndef NOMINMAX
        #define NOMINMAX
    #endif
    #include <windows.h>
    #include <winsock2.h>
    #include <ws2tcpip.h>
    #pragma comment(lib, "ws2_32.lib")
#elif defined(__APPLE__)
    #define DCF_PLATFORM_MACOS 1
    #define DCF_PLATFORM_POSIX 1
    #define DCF_PLATFORM_NAME "macos"
    #include <pthread.h>
    #include <unistd.h>
    #include <sys/time.h>
    #include <mach/mach_time.h>
#elif defined(__FreeBSD__)
    #define DCF_PLATFORM_FREEBSD 1
    #define DCF_PLATFORM_POSIX 1
    #define DCF_PLATFORM_NAME "freebsd"
    #include <pthread.h>
    #include <unistd.h>
    #include <sys/time.h>
#else
    #define DCF_PLATFORM_LINUX 1
    #define DCF_PLATFORM_POSIX 1
    #define DCF_PLATFORM_NAME "linux"
    #include <pthread.h>
    #include <unistd.h>
    #include <sys/time.h>
    #include <sys/eventfd.h>
#endif

/* ============================================================================
 * Compiler Detection and Attributes
 * ============================================================================ */

#if defined(__GNUC__) || defined(__clang__)
    #define DCF_COMPILER_GCC_COMPAT 1
    #define DCF_LIKELY(x)       __builtin_expect(!!(x), 1)
    #define DCF_UNLIKELY(x)     __builtin_expect(!!(x), 0)
    #define DCF_UNUSED          __attribute__((unused))
    #define DCF_NOINLINE        __attribute__((noinline))
    #define DCF_ALWAYS_INLINE   __attribute__((always_inline)) inline
    #define DCF_PURE            __attribute__((pure))
    #define DCF_CONST           __attribute__((const))
    #define DCF_NORETURN        __attribute__((noreturn))
    #define DCF_PACKED          __attribute__((packed))
    #define DCF_ALIGNED(n)      __attribute__((aligned(n)))
    #define DCF_DEPRECATED(msg) __attribute__((deprecated(msg)))
    #define DCF_WARN_UNUSED     __attribute__((warn_unused_result))
    #define DCF_PRINTF(fmt, va) __attribute__((format(printf, fmt, va)))
    #define DCF_RESTRICT        __restrict__
    #define DCF_PREFETCH(addr)  __builtin_prefetch(addr)
    #define DCF_UNREACHABLE()   __builtin_unreachable()
#elif defined(_MSC_VER)
    #define DCF_COMPILER_MSVC 1
    #define DCF_LIKELY(x)       (x)
    #define DCF_UNLIKELY(x)     (x)
    #define DCF_UNUSED
    #define DCF_NOINLINE        __declspec(noinline)
    #define DCF_ALWAYS_INLINE   __forceinline
    #define DCF_PURE
    #define DCF_CONST
    #define DCF_NORETURN        __declspec(noreturn)
    #define DCF_PACKED
    #define DCF_ALIGNED(n)      __declspec(align(n))
    #define DCF_DEPRECATED(msg) __declspec(deprecated(msg))
    #define DCF_WARN_UNUSED     _Check_return_
    #define DCF_PRINTF(fmt, va)
    #define DCF_RESTRICT        __restrict
    #define DCF_PREFETCH(addr)
    #define DCF_UNREACHABLE()   __assume(0)
#else
    #define DCF_LIKELY(x)       (x)
    #define DCF_UNLIKELY(x)     (x)
    #define DCF_UNUSED
    #define DCF_NOINLINE
    #define DCF_ALWAYS_INLINE   inline
    #define DCF_PURE
    #define DCF_CONST
    #define DCF_NORETURN
    #define DCF_PACKED
    #define DCF_ALIGNED(n)
    #define DCF_DEPRECATED(msg)
    #define DCF_WARN_UNUSED
    #define DCF_PRINTF(fmt, va)
    #define DCF_RESTRICT
    #define DCF_PREFETCH(addr)
    #define DCF_UNREACHABLE()
#endif

/* Thread-local storage
 * 
 * NOTE: When building shared libraries (.so), the TLS model matters!
 * By default, GCC/Clang use "initial-exec" which generates R_X86_64_TPOFF32
 * relocations that cannot be used in shared objects.
 * 
 * Solution: Compile shared library code with -ftls-model=global-dynamic
 * This is handled automatically by CMakeLists.txt for dcf_shared target.
 * 
 * For manual builds, see Makefile CFLAGS_SHARED variable.
 */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_THREADS__)
    #define DCF_THREAD_LOCAL _Thread_local
#elif defined(DCF_COMPILER_GCC_COMPAT)
    #define DCF_THREAD_LOCAL __thread
#elif defined(_MSC_VER)
    #define DCF_THREAD_LOCAL __declspec(thread)
#else
    #define DCF_THREAD_LOCAL
    #warning "Thread-local storage not supported on this platform"
#endif

/* Export/Import macros */
#ifdef DCF_PLATFORM_WINDOWS
    #ifdef DCF_BUILD_SHARED
        #define DCF_API __declspec(dllexport)
    #elif defined(DCF_USE_SHARED)
        #define DCF_API __declspec(dllimport)
    #else
        #define DCF_API
    #endif
#else
    #ifdef DCF_BUILD_SHARED
        #define DCF_API __attribute__((visibility("default")))
    #else
        #define DCF_API
    #endif
#endif

#define DCF_INTERNAL __attribute__((visibility("hidden")))

/* ============================================================================
 * Atomic Operations
 * ============================================================================ */

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_ATOMICS__)
    #include <stdatomic.h>
    typedef atomic_int          dcf_atomic_int;
    typedef atomic_uint         dcf_atomic_uint;
    typedef atomic_llong        dcf_atomic_int64;
    typedef atomic_ullong       dcf_atomic_uint64;
    typedef atomic_intptr_t     dcf_atomic_ptr;
    typedef atomic_bool         dcf_atomic_bool;
    
    #define dcf_atomic_init(obj, val)           atomic_init(obj, val)
    #define dcf_atomic_load(obj)                atomic_load_explicit(obj, memory_order_acquire)
    #define dcf_atomic_store(obj, val)          atomic_store_explicit(obj, val, memory_order_release)
    #define dcf_atomic_exchange(obj, val)       atomic_exchange_explicit(obj, val, memory_order_acq_rel)
    #define dcf_atomic_fetch_add(obj, val)      atomic_fetch_add_explicit(obj, val, memory_order_acq_rel)
    #define dcf_atomic_fetch_sub(obj, val)      atomic_fetch_sub_explicit(obj, val, memory_order_acq_rel)
    #define dcf_atomic_fetch_or(obj, val)       atomic_fetch_or_explicit(obj, val, memory_order_acq_rel)
    #define dcf_atomic_fetch_and(obj, val)      atomic_fetch_and_explicit(obj, val, memory_order_acq_rel)
    #define dcf_atomic_compare_exchange(obj, expected, desired) \
        atomic_compare_exchange_strong_explicit(obj, expected, desired, \
            memory_order_acq_rel, memory_order_acquire)
    #define dcf_atomic_thread_fence()           atomic_thread_fence(memory_order_seq_cst)
    
#elif defined(DCF_COMPILER_GCC_COMPAT)
    typedef volatile int        dcf_atomic_int;
    typedef volatile unsigned   dcf_atomic_uint;
    typedef volatile long long  dcf_atomic_int64;
    typedef volatile unsigned long long dcf_atomic_uint64;
    typedef volatile intptr_t   dcf_atomic_ptr;
    typedef volatile int        dcf_atomic_bool;
    
    #define dcf_atomic_init(obj, val)           (*(obj) = (val))
    #define dcf_atomic_load(obj)                __atomic_load_n(obj, __ATOMIC_ACQUIRE)
    #define dcf_atomic_store(obj, val)          __atomic_store_n(obj, val, __ATOMIC_RELEASE)
    #define dcf_atomic_exchange(obj, val)       __atomic_exchange_n(obj, val, __ATOMIC_ACQ_REL)
    #define dcf_atomic_fetch_add(obj, val)      __atomic_fetch_add(obj, val, __ATOMIC_ACQ_REL)
    #define dcf_atomic_fetch_sub(obj, val)      __atomic_fetch_sub(obj, val, __ATOMIC_ACQ_REL)
    #define dcf_atomic_fetch_or(obj, val)       __atomic_fetch_or(obj, val, __ATOMIC_ACQ_REL)
    #define dcf_atomic_fetch_and(obj, val)      __atomic_fetch_and(obj, val, __ATOMIC_ACQ_REL)
    #define dcf_atomic_compare_exchange(obj, expected, desired) \
        __atomic_compare_exchange_n(obj, expected, desired, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)
    #define dcf_atomic_thread_fence()           __atomic_thread_fence(__ATOMIC_SEQ_CST)
    
#elif defined(_MSC_VER)
    #include <intrin.h>
    typedef volatile long       dcf_atomic_int;
    typedef volatile unsigned long dcf_atomic_uint;
    typedef volatile long long  dcf_atomic_int64;
    typedef volatile unsigned long long dcf_atomic_uint64;
    typedef volatile intptr_t   dcf_atomic_ptr;
    typedef volatile long       dcf_atomic_bool;
    
    #define dcf_atomic_init(obj, val)           (*(obj) = (val))
    #define dcf_atomic_load(obj)                (*(obj))
    #define dcf_atomic_store(obj, val)          _InterlockedExchange((long*)(obj), (long)(val))
    #define dcf_atomic_exchange(obj, val)       _InterlockedExchange((long*)(obj), (long)(val))
    #define dcf_atomic_fetch_add(obj, val)      _InterlockedExchangeAdd((long*)(obj), (long)(val))
    #define dcf_atomic_fetch_sub(obj, val)      _InterlockedExchangeAdd((long*)(obj), -(long)(val))
    #define dcf_atomic_fetch_or(obj, val)       _InterlockedOr((long*)(obj), (long)(val))
    #define dcf_atomic_fetch_and(obj, val)      _InterlockedAnd((long*)(obj), (long)(val))
    #define dcf_atomic_compare_exchange(obj, expected, desired) \
        (_InterlockedCompareExchange((long*)(obj), (long)(desired), *(long*)(expected)) == *(long*)(expected))
    #define dcf_atomic_thread_fence()           MemoryBarrier()
#endif

/* ============================================================================
 * Threading Primitives
 * ============================================================================ */

#ifdef DCF_PLATFORM_WINDOWS
    typedef CRITICAL_SECTION    dcf_mutex_t;
    typedef SRWLOCK             dcf_rwlock_t;
    typedef CONDITION_VARIABLE  dcf_cond_t;
    typedef HANDLE              dcf_thread_t;
    typedef DWORD               dcf_thread_id_t;
    typedef HANDLE              dcf_event_t;
#else
    typedef pthread_mutex_t     dcf_mutex_t;
    typedef pthread_rwlock_t    dcf_rwlock_t;
    typedef pthread_cond_t      dcf_cond_t;
    typedef pthread_t           dcf_thread_t;
    typedef pthread_t           dcf_thread_id_t;
    #ifdef DCF_PLATFORM_LINUX
        typedef int             dcf_event_t;  /* eventfd */
    #else
        typedef struct {
            pthread_mutex_t     mutex;
            pthread_cond_t      cond;
            bool                signaled;
        } dcf_event_t;
    #endif
#endif

/* Mutex operations */
DCF_API int dcf_mutex_init(dcf_mutex_t* mutex);
DCF_API int dcf_mutex_destroy(dcf_mutex_t* mutex);
DCF_API int dcf_mutex_lock(dcf_mutex_t* mutex);
DCF_API int dcf_mutex_trylock(dcf_mutex_t* mutex);
DCF_API int dcf_mutex_unlock(dcf_mutex_t* mutex);

/* Read-write lock operations */
DCF_API int dcf_rwlock_init(dcf_rwlock_t* lock);
DCF_API int dcf_rwlock_destroy(dcf_rwlock_t* lock);
DCF_API int dcf_rwlock_rdlock(dcf_rwlock_t* lock);
DCF_API int dcf_rwlock_tryrdlock(dcf_rwlock_t* lock);
DCF_API int dcf_rwlock_wrlock(dcf_rwlock_t* lock);
DCF_API int dcf_rwlock_trywrlock(dcf_rwlock_t* lock);
DCF_API int dcf_rwlock_unlock(dcf_rwlock_t* lock);

/* Condition variable operations */
DCF_API int dcf_cond_init(dcf_cond_t* cond);
DCF_API int dcf_cond_destroy(dcf_cond_t* cond);
DCF_API int dcf_cond_wait(dcf_cond_t* cond, dcf_mutex_t* mutex);
DCF_API int dcf_cond_timedwait(dcf_cond_t* cond, dcf_mutex_t* mutex, uint64_t timeout_ms);
DCF_API int dcf_cond_signal(dcf_cond_t* cond);
DCF_API int dcf_cond_broadcast(dcf_cond_t* cond);

/* Thread operations */
typedef void* (*dcf_thread_func_t)(void* arg);
DCF_API int dcf_thread_create(dcf_thread_t* thread, dcf_thread_func_t func, void* arg);
DCF_API int dcf_thread_join(dcf_thread_t thread, void** retval);
DCF_API int dcf_thread_detach(dcf_thread_t thread);
DCF_API dcf_thread_id_t dcf_thread_self(void);
DCF_API void dcf_thread_yield(void);

/* Event operations (for cancellation) */
DCF_API int dcf_event_init(dcf_event_t* event);
DCF_API int dcf_event_destroy(dcf_event_t* event);
DCF_API int dcf_event_signal(dcf_event_t* event);
DCF_API int dcf_event_reset(dcf_event_t* event);
DCF_API int dcf_event_wait(dcf_event_t* event, uint64_t timeout_ms);

/* ============================================================================
 * Time Functions
 * ============================================================================ */

/**
 * @brief Get monotonic timestamp in nanoseconds
 * @return Nanoseconds since an arbitrary point (boot time typically)
 */
DCF_API uint64_t dcf_time_monotonic_ns(void);

/**
 * @brief Get monotonic timestamp in milliseconds
 */
DCF_API uint64_t dcf_time_monotonic_ms(void);

/**
 * @brief Get wall clock time in microseconds since epoch
 */
DCF_API uint64_t dcf_time_realtime_us(void);

/**
 * @brief Sleep for specified milliseconds
 */
DCF_API void dcf_sleep_ms(uint32_t ms);

/**
 * @brief Sleep for specified microseconds
 */
DCF_API void dcf_sleep_us(uint32_t us);

/**
 * @brief High-resolution timer for benchmarking
 */
typedef struct dcf_timer {
    uint64_t start_ns;
    uint64_t elapsed_ns;
} dcf_timer_t;

DCF_API void dcf_timer_start(dcf_timer_t* timer);
DCF_API uint64_t dcf_timer_stop(dcf_timer_t* timer);
DCF_API uint64_t dcf_timer_elapsed_ns(const dcf_timer_t* timer);
DCF_API double dcf_timer_elapsed_ms(const dcf_timer_t* timer);

/* ============================================================================
 * Memory Management
 * ============================================================================ */

/**
 * @brief Memory allocation statistics
 */
typedef struct dcf_mem_stats {
    dcf_atomic_uint64 total_allocated;
    dcf_atomic_uint64 total_freed;
    dcf_atomic_uint64 current_usage;
    dcf_atomic_uint64 peak_usage;
    dcf_atomic_uint64 allocation_count;
    dcf_atomic_uint64 free_count;
    dcf_atomic_uint64 realloc_count;
} dcf_mem_stats_t;

/**
 * @brief Get global memory statistics
 */
DCF_API const dcf_mem_stats_t* dcf_mem_get_stats(void);

/**
 * @brief Reset memory statistics
 */
DCF_API void dcf_mem_reset_stats(void);

/**
 * @brief Tracked memory allocation
 */
DCF_API void* dcf_malloc(size_t size);
DCF_API void* dcf_calloc(size_t count, size_t size);
DCF_API void* dcf_realloc(void* ptr, size_t size);
DCF_API void dcf_free(void* ptr);
DCF_API char* dcf_strdup(const char* str);
DCF_API char* dcf_strndup(const char* str, size_t n);

/**
 * @brief Aligned memory allocation
 */
DCF_API void* dcf_aligned_alloc(size_t alignment, size_t size);
DCF_API void dcf_aligned_free(void* ptr);

/**
 * @brief Safe memory operations
 */
DCF_API void* dcf_memcpy_s(void* DCF_RESTRICT dest, size_t dest_size,
                           const void* DCF_RESTRICT src, size_t count);
DCF_API void* dcf_memmove_s(void* dest, size_t dest_size,
                            const void* src, size_t count);
DCF_API void* dcf_memset_s(void* dest, size_t dest_size, int ch, size_t count);

/**
 * @brief Secure memory wipe (prevents compiler optimization)
 */
DCF_API void dcf_secure_zero(void* ptr, size_t size);

/* ============================================================================
 * Socket Abstraction
 * ============================================================================ */

#ifdef DCF_PLATFORM_WINDOWS
    typedef SOCKET dcf_socket_t;
    #define DCF_INVALID_SOCKET INVALID_SOCKET
    #define DCF_SOCKET_ERROR   SOCKET_ERROR
#else
    typedef int dcf_socket_t;
    #define DCF_INVALID_SOCKET (-1)
    #define DCF_SOCKET_ERROR   (-1)
#endif

/**
 * @brief Initialize socket subsystem (required on Windows)
 */
DCF_API int dcf_socket_init(void);

/**
 * @brief Cleanup socket subsystem
 */
DCF_API void dcf_socket_cleanup(void);

/**
 * @brief Set socket to non-blocking mode
 */
DCF_API int dcf_socket_set_nonblocking(dcf_socket_t sock, bool nonblocking);

/**
 * @brief Set socket options (TCP_NODELAY, SO_KEEPALIVE, etc.)
 */
DCF_API int dcf_socket_set_nodelay(dcf_socket_t sock, bool nodelay);
DCF_API int dcf_socket_set_keepalive(dcf_socket_t sock, bool keepalive);
DCF_API int dcf_socket_set_reuseaddr(dcf_socket_t sock, bool reuse);
DCF_API int dcf_socket_set_rcvbuf(dcf_socket_t sock, int size);
DCF_API int dcf_socket_set_sndbuf(dcf_socket_t sock, int size);
DCF_API int dcf_socket_set_rcvtimeo(dcf_socket_t sock, uint32_t timeout_ms);
DCF_API int dcf_socket_set_sndtimeo(dcf_socket_t sock, uint32_t timeout_ms);

/**
 * @brief Close socket
 */
DCF_API int dcf_socket_close(dcf_socket_t sock);

/**
 * @brief Get last socket error
 */
DCF_API int dcf_socket_errno(void);
DCF_API const char* dcf_socket_strerror(int err);

/* ============================================================================
 * Dynamic Library Loading
 * ============================================================================ */

#ifdef DCF_PLATFORM_WINDOWS
    typedef HMODULE dcf_lib_t;
#else
    typedef void* dcf_lib_t;
#endif

#define DCF_INVALID_LIB NULL

/**
 * @brief Load dynamic library
 * @param path Path to library
 * @return Library handle or DCF_INVALID_LIB on failure
 */
DCF_API dcf_lib_t dcf_lib_open(const char* path);

/**
 * @brief Close dynamic library
 */
DCF_API void dcf_lib_close(dcf_lib_t lib);

/**
 * @brief Get symbol from library
 */
DCF_API void* dcf_lib_symbol(dcf_lib_t lib, const char* name);

/**
 * @brief Get last error message
 */
DCF_API const char* dcf_lib_error(void);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Get number of CPU cores
 */
DCF_API int dcf_cpu_count(void);

/**
 * @brief Get page size
 */
DCF_API size_t dcf_page_size(void);

/**
 * @brief Get hostname
 */
DCF_API int dcf_get_hostname(char* buffer, size_t size);

/**
 * @brief Generate random bytes (cryptographically secure when available)
 */
DCF_API int dcf_random_bytes(void* buffer, size_t size);

/**
 * @brief Generate random 32-bit integer
 */
DCF_API uint32_t dcf_random_u32(void);

/**
 * @brief Generate random 64-bit integer
 */
DCF_API uint64_t dcf_random_u64(void);

#ifdef __cplusplus
}
#endif

#endif /* DCF_PLATFORM_H */
