/**
 * @file dcf_platform.c
 * @brief Platform Abstraction Layer Implementation
 * @version 5.2.0
 */

#include "dcf_platform.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#ifdef DCF_PLATFORM_POSIX
    #include <pthread.h>
    #include <unistd.h>
    #include <sys/time.h>
    #include <sys/mman.h>
    #include <fcntl.h>
    #include <dlfcn.h>
    #include <netdb.h>
    #include <sys/socket.h>
    #include <netinet/in.h>
    #include <netinet/tcp.h>
    #include <arpa/inet.h>
    #ifdef DCF_PLATFORM_LINUX
        #include <sys/eventfd.h>
        #include <sys/random.h>
    #endif
    #ifdef DCF_PLATFORM_MACOS
        #include <mach/mach_time.h>
        #include <Security/Security.h>
    #endif
#endif

/* ============================================================================
 * Memory Tracking
 * ============================================================================ */

static dcf_mem_stats_t g_mem_stats = {0};
static dcf_mutex_t g_mem_mutex;
static bool g_mem_initialized = false;

/* Memory allocation header for tracking */
typedef struct {
    size_t size;
    uint32_t magic;
} dcf_mem_header_t;

#define DCF_MEM_MAGIC 0xDCF0A110
#define DCF_MEM_ALIGN_UP(x, align) (((x) + ((align) - 1)) & ~((align) - 1))
#define DCF_MEM_HEADER_SIZE DCF_MEM_ALIGN_UP(sizeof(dcf_mem_header_t), 16)

static void mem_init(void) {
    if (!g_mem_initialized) {
        dcf_mutex_init(&g_mem_mutex);
        g_mem_initialized = true;
    }
}

const dcf_mem_stats_t* dcf_mem_get_stats(void) {
    return &g_mem_stats;
}

void dcf_mem_reset_stats(void) {
    dcf_atomic_store(&g_mem_stats.total_allocated, 0);
    dcf_atomic_store(&g_mem_stats.total_freed, 0);
    dcf_atomic_store(&g_mem_stats.current_usage, 0);
    dcf_atomic_store(&g_mem_stats.peak_usage, 0);
    dcf_atomic_store(&g_mem_stats.allocation_count, 0);
    dcf_atomic_store(&g_mem_stats.free_count, 0);
    dcf_atomic_store(&g_mem_stats.realloc_count, 0);
}

void* dcf_malloc(size_t size) {
    if (size == 0) return NULL;
    
    mem_init();
    
    void* ptr = malloc(DCF_MEM_HEADER_SIZE + size);
    if (!ptr) return NULL;
    
    dcf_mem_header_t* header = (dcf_mem_header_t*)ptr;
    header->size = size;
    header->magic = DCF_MEM_MAGIC;
    
    dcf_atomic_fetch_add(&g_mem_stats.total_allocated, size);
    dcf_atomic_fetch_add(&g_mem_stats.allocation_count, 1);
    
    uint64_t current = dcf_atomic_fetch_add(&g_mem_stats.current_usage, size) + size;
    uint64_t peak = dcf_atomic_load(&g_mem_stats.peak_usage);
    while (current > peak) {
        if (dcf_atomic_compare_exchange(&g_mem_stats.peak_usage, &peak, current)) break;
        peak = dcf_atomic_load(&g_mem_stats.peak_usage);
    }
    
    return (char*)ptr + DCF_MEM_HEADER_SIZE;
}

void* dcf_calloc(size_t count, size_t size) {
    size_t total = count * size;
    if (count != 0 && total / count != size) return NULL; /* Overflow check */
    
    void* ptr = dcf_malloc(total);
    if (ptr) memset(ptr, 0, total);
    return ptr;
}

void* dcf_realloc(void* ptr, size_t size) {
    if (!ptr) return dcf_malloc(size);
    if (size == 0) { dcf_free(ptr); return NULL; }
    
    dcf_mem_header_t* old_header = (dcf_mem_header_t*)((char*)ptr - DCF_MEM_HEADER_SIZE);
    if (old_header->magic != DCF_MEM_MAGIC) {
        /* Not tracked, use normal realloc */
        return realloc(ptr, size);
    }
    
    size_t old_size = old_header->size;
    
    void* new_ptr = realloc(old_header, DCF_MEM_HEADER_SIZE + size);
    if (!new_ptr) return NULL;
    
    dcf_mem_header_t* new_header = (dcf_mem_header_t*)new_ptr;
    new_header->size = size;
    
    dcf_atomic_fetch_add(&g_mem_stats.realloc_count, 1);
    
    if (size > old_size) {
        dcf_atomic_fetch_add(&g_mem_stats.total_allocated, size - old_size);
        dcf_atomic_fetch_add(&g_mem_stats.current_usage, size - old_size);
    } else {
        dcf_atomic_fetch_sub(&g_mem_stats.current_usage, old_size - size);
        dcf_atomic_fetch_add(&g_mem_stats.total_freed, old_size - size);
    }
    
    return (char*)new_ptr + DCF_MEM_HEADER_SIZE;
}

void dcf_free(void* ptr) {
    if (!ptr) return;
    
    dcf_mem_header_t* header = (dcf_mem_header_t*)((char*)ptr - DCF_MEM_HEADER_SIZE);
    if (header->magic != DCF_MEM_MAGIC) {
        /* Not tracked, use normal free */
        free(ptr);
        return;
    }
    
    size_t size = header->size;
    header->magic = 0; /* Prevent double-free detection issues */
    
    dcf_atomic_fetch_add(&g_mem_stats.total_freed, size);
    dcf_atomic_fetch_sub(&g_mem_stats.current_usage, size);
    dcf_atomic_fetch_add(&g_mem_stats.free_count, 1);
    
    free(header);
}

char* dcf_strdup(const char* str) {
    if (!str) return NULL;
    size_t len = strlen(str) + 1;
    char* dup = dcf_malloc(len);
    if (dup) memcpy(dup, str, len);
    return dup;
}

char* dcf_strndup(const char* str, size_t n) {
    if (!str) return NULL;
    size_t len = strnlen(str, n);
    char* dup = dcf_malloc(len + 1);
    if (dup) {
        memcpy(dup, str, len);
        dup[len] = '\0';
    }
    return dup;
}

void* dcf_aligned_alloc(size_t alignment, size_t size) {
#ifdef DCF_PLATFORM_WINDOWS
    return _aligned_malloc(size, alignment);
#else
    void* ptr = NULL;
    if (posix_memalign(&ptr, alignment, size) != 0) return NULL;
    return ptr;
#endif
}

void dcf_aligned_free(void* ptr) {
#ifdef DCF_PLATFORM_WINDOWS
    _aligned_free(ptr);
#else
    free(ptr);
#endif
}

void* dcf_memcpy_s(void* DCF_RESTRICT dest, size_t dest_size,
                   const void* DCF_RESTRICT src, size_t count) {
    if (!dest || !src || count > dest_size) return NULL;
    return memcpy(dest, src, count);
}

void* dcf_memmove_s(void* dest, size_t dest_size,
                    const void* src, size_t count) {
    if (!dest || !src || count > dest_size) return NULL;
    return memmove(dest, src, count);
}

void* dcf_memset_s(void* dest, size_t dest_size, int ch, size_t count) {
    if (!dest || count > dest_size) return NULL;
    return memset(dest, ch, count);
}

void dcf_secure_zero(void* ptr, size_t size) {
    if (!ptr || size == 0) return;
    volatile unsigned char* p = (volatile unsigned char*)ptr;
    while (size--) *p++ = 0;
    dcf_atomic_thread_fence();
}

/* ============================================================================
 * Threading - POSIX
 * ============================================================================ */

#ifdef DCF_PLATFORM_POSIX

int dcf_mutex_init(dcf_mutex_t* mutex) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    int ret = pthread_mutex_init(mutex, &attr);
    pthread_mutexattr_destroy(&attr);
    return ret;
}

int dcf_mutex_destroy(dcf_mutex_t* mutex) {
    return pthread_mutex_destroy(mutex);
}

int dcf_mutex_lock(dcf_mutex_t* mutex) {
    return pthread_mutex_lock(mutex);
}

int dcf_mutex_trylock(dcf_mutex_t* mutex) {
    return pthread_mutex_trylock(mutex);
}

int dcf_mutex_unlock(dcf_mutex_t* mutex) {
    return pthread_mutex_unlock(mutex);
}

int dcf_rwlock_init(dcf_rwlock_t* lock) {
    return pthread_rwlock_init(lock, NULL);
}

int dcf_rwlock_destroy(dcf_rwlock_t* lock) {
    return pthread_rwlock_destroy(lock);
}

int dcf_rwlock_rdlock(dcf_rwlock_t* lock) {
    return pthread_rwlock_rdlock(lock);
}

int dcf_rwlock_tryrdlock(dcf_rwlock_t* lock) {
    return pthread_rwlock_tryrdlock(lock);
}

int dcf_rwlock_wrlock(dcf_rwlock_t* lock) {
    return pthread_rwlock_wrlock(lock);
}

int dcf_rwlock_trywrlock(dcf_rwlock_t* lock) {
    return pthread_rwlock_trywrlock(lock);
}

int dcf_rwlock_unlock(dcf_rwlock_t* lock) {
    return pthread_rwlock_unlock(lock);
}

int dcf_cond_init(dcf_cond_t* cond) {
    return pthread_cond_init(cond, NULL);
}

int dcf_cond_destroy(dcf_cond_t* cond) {
    return pthread_cond_destroy(cond);
}

int dcf_cond_wait(dcf_cond_t* cond, dcf_mutex_t* mutex) {
    return pthread_cond_wait(cond, mutex);
}

int dcf_cond_timedwait(dcf_cond_t* cond, dcf_mutex_t* mutex, uint64_t timeout_ms) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout_ms / 1000;
    ts.tv_nsec += (timeout_ms % 1000) * 1000000;
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000000000;
    }
    return pthread_cond_timedwait(cond, mutex, &ts);
}

int dcf_cond_signal(dcf_cond_t* cond) {
    return pthread_cond_signal(cond);
}

int dcf_cond_broadcast(dcf_cond_t* cond) {
    return pthread_cond_broadcast(cond);
}

int dcf_thread_create(dcf_thread_t* thread, dcf_thread_func_t func, void* arg) {
    return pthread_create(thread, NULL, func, arg);
}

int dcf_thread_join(dcf_thread_t thread, void** retval) {
    return pthread_join(thread, retval);
}

int dcf_thread_detach(dcf_thread_t thread) {
    return pthread_detach(thread);
}

dcf_thread_id_t dcf_thread_self(void) {
    return pthread_self();
}

void dcf_thread_yield(void) {
    sched_yield();
}

/* Event operations */
#ifdef DCF_PLATFORM_LINUX
int dcf_event_init(dcf_event_t* event) {
    *event = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
    return (*event < 0) ? -1 : 0;
}

int dcf_event_destroy(dcf_event_t* event) {
    return close(*event);
}

int dcf_event_signal(dcf_event_t* event) {
    uint64_t val = 1;
    return (write(*event, &val, sizeof(val)) == sizeof(val)) ? 0 : -1;
}

int dcf_event_reset(dcf_event_t* event) {
    uint64_t val;
    while (read(*event, &val, sizeof(val)) > 0);
    return 0;
}

int dcf_event_wait(dcf_event_t* event, uint64_t timeout_ms) {
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(*event, &fds);
    
    struct timeval tv;
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;
    
    int ret = select(*event + 1, &fds, NULL, NULL, timeout_ms == (uint64_t)-1 ? NULL : &tv);
    if (ret > 0) {
        dcf_event_reset(event);
        return 0;
    }
    return (ret == 0) ? ETIMEDOUT : -1;
}
#else
/* Non-Linux POSIX: use mutex+condvar */
int dcf_event_init(dcf_event_t* event) {
    int ret = pthread_mutex_init(&event->mutex, NULL);
    if (ret != 0) return ret;
    ret = pthread_cond_init(&event->cond, NULL);
    if (ret != 0) {
        pthread_mutex_destroy(&event->mutex);
        return ret;
    }
    event->signaled = false;
    return 0;
}

int dcf_event_destroy(dcf_event_t* event) {
    pthread_cond_destroy(&event->cond);
    pthread_mutex_destroy(&event->mutex);
    return 0;
}

int dcf_event_signal(dcf_event_t* event) {
    pthread_mutex_lock(&event->mutex);
    event->signaled = true;
    pthread_cond_signal(&event->cond);
    pthread_mutex_unlock(&event->mutex);
    return 0;
}

int dcf_event_reset(dcf_event_t* event) {
    pthread_mutex_lock(&event->mutex);
    event->signaled = false;
    pthread_mutex_unlock(&event->mutex);
    return 0;
}

int dcf_event_wait(dcf_event_t* event, uint64_t timeout_ms) {
    pthread_mutex_lock(&event->mutex);
    while (!event->signaled) {
        if (timeout_ms == (uint64_t)-1) {
            pthread_cond_wait(&event->cond, &event->mutex);
        } else {
            int ret = dcf_cond_timedwait(&event->cond, &event->mutex, timeout_ms);
            if (ret == ETIMEDOUT) {
                pthread_mutex_unlock(&event->mutex);
                return ETIMEDOUT;
            }
        }
    }
    event->signaled = false;
    pthread_mutex_unlock(&event->mutex);
    return 0;
}
#endif

#endif /* DCF_PLATFORM_POSIX */

/* ============================================================================
 * Threading - Windows
 * ============================================================================ */

#ifdef DCF_PLATFORM_WINDOWS

int dcf_mutex_init(dcf_mutex_t* mutex) {
    InitializeCriticalSection(mutex);
    return 0;
}

int dcf_mutex_destroy(dcf_mutex_t* mutex) {
    DeleteCriticalSection(mutex);
    return 0;
}

int dcf_mutex_lock(dcf_mutex_t* mutex) {
    EnterCriticalSection(mutex);
    return 0;
}

int dcf_mutex_trylock(dcf_mutex_t* mutex) {
    return TryEnterCriticalSection(mutex) ? 0 : EBUSY;
}

int dcf_mutex_unlock(dcf_mutex_t* mutex) {
    LeaveCriticalSection(mutex);
    return 0;
}

int dcf_rwlock_init(dcf_rwlock_t* lock) {
    InitializeSRWLock(lock);
    return 0;
}

int dcf_rwlock_destroy(dcf_rwlock_t* lock) {
    (void)lock;
    return 0;
}

int dcf_rwlock_rdlock(dcf_rwlock_t* lock) {
    AcquireSRWLockShared(lock);
    return 0;
}

int dcf_rwlock_tryrdlock(dcf_rwlock_t* lock) {
    return TryAcquireSRWLockShared(lock) ? 0 : EBUSY;
}

int dcf_rwlock_wrlock(dcf_rwlock_t* lock) {
    AcquireSRWLockExclusive(lock);
    return 0;
}

int dcf_rwlock_trywrlock(dcf_rwlock_t* lock) {
    return TryAcquireSRWLockExclusive(lock) ? 0 : EBUSY;
}

int dcf_rwlock_unlock(dcf_rwlock_t* lock) {
    /* SRWLock doesn't distinguish; try both */
    ReleaseSRWLockExclusive(lock);
    return 0;
}

int dcf_cond_init(dcf_cond_t* cond) {
    InitializeConditionVariable(cond);
    return 0;
}

int dcf_cond_destroy(dcf_cond_t* cond) {
    (void)cond;
    return 0;
}

int dcf_cond_wait(dcf_cond_t* cond, dcf_mutex_t* mutex) {
    SleepConditionVariableCS(cond, mutex, INFINITE);
    return 0;
}

int dcf_cond_timedwait(dcf_cond_t* cond, dcf_mutex_t* mutex, uint64_t timeout_ms) {
    if (!SleepConditionVariableCS(cond, mutex, (DWORD)timeout_ms)) {
        return (GetLastError() == ERROR_TIMEOUT) ? ETIMEDOUT : -1;
    }
    return 0;
}

int dcf_cond_signal(dcf_cond_t* cond) {
    WakeConditionVariable(cond);
    return 0;
}

int dcf_cond_broadcast(dcf_cond_t* cond) {
    WakeAllConditionVariable(cond);
    return 0;
}

static DWORD WINAPI win_thread_wrapper(void* arg) {
    struct { dcf_thread_func_t func; void* arg; }* ctx = arg;
    dcf_thread_func_t func = ctx->func;
    void* func_arg = ctx->arg;
    free(ctx);
    func(func_arg);
    return 0;
}

int dcf_thread_create(dcf_thread_t* thread, dcf_thread_func_t func, void* arg) {
    struct { dcf_thread_func_t func; void* arg; }* ctx = malloc(sizeof(*ctx));
    if (!ctx) return ENOMEM;
    ctx->func = func;
    ctx->arg = arg;
    *thread = CreateThread(NULL, 0, win_thread_wrapper, ctx, 0, NULL);
    return (*thread == NULL) ? -1 : 0;
}

int dcf_thread_join(dcf_thread_t thread, void** retval) {
    WaitForSingleObject(thread, INFINITE);
    CloseHandle(thread);
    if (retval) *retval = NULL;
    return 0;
}

int dcf_thread_detach(dcf_thread_t thread) {
    CloseHandle(thread);
    return 0;
}

dcf_thread_id_t dcf_thread_self(void) {
    return GetCurrentThreadId();
}

void dcf_thread_yield(void) {
    SwitchToThread();
}

int dcf_event_init(dcf_event_t* event) {
    *event = CreateEvent(NULL, TRUE, FALSE, NULL);
    return (*event == NULL) ? -1 : 0;
}

int dcf_event_destroy(dcf_event_t* event) {
    CloseHandle(*event);
    return 0;
}

int dcf_event_signal(dcf_event_t* event) {
    return SetEvent(*event) ? 0 : -1;
}

int dcf_event_reset(dcf_event_t* event) {
    return ResetEvent(*event) ? 0 : -1;
}

int dcf_event_wait(dcf_event_t* event, uint64_t timeout_ms) {
    DWORD ret = WaitForSingleObject(*event, 
        (timeout_ms == (uint64_t)-1) ? INFINITE : (DWORD)timeout_ms);
    return (ret == WAIT_OBJECT_0) ? 0 : (ret == WAIT_TIMEOUT) ? ETIMEDOUT : -1;
}

#endif /* DCF_PLATFORM_WINDOWS */

/* ============================================================================
 * Time Functions
 * ============================================================================ */

uint64_t dcf_time_monotonic_ns(void) {
#ifdef DCF_PLATFORM_LINUX
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#elif defined(DCF_PLATFORM_MACOS)
    static mach_timebase_info_data_t timebase = {0};
    if (timebase.denom == 0) mach_timebase_info(&timebase);
    return mach_absolute_time() * timebase.numer / timebase.denom;
#elif defined(DCF_PLATFORM_WINDOWS)
    static LARGE_INTEGER freq = {0};
    if (freq.QuadPart == 0) QueryPerformanceFrequency(&freq);
    LARGE_INTEGER now;
    QueryPerformanceCounter(&now);
    return (uint64_t)(now.QuadPart * 1000000000ULL / freq.QuadPart);
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)tv.tv_sec * 1000000000ULL + tv.tv_usec * 1000;
#endif
}

uint64_t dcf_time_monotonic_ms(void) {
    return dcf_time_monotonic_ns() / 1000000ULL;
}

uint64_t dcf_time_realtime_us(void) {
#ifdef DCF_PLATFORM_WINDOWS
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    ULARGE_INTEGER ul;
    ul.LowPart = ft.dwLowDateTime;
    ul.HighPart = ft.dwHighDateTime;
    return (ul.QuadPart - 116444736000000000ULL) / 10;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)tv.tv_sec * 1000000ULL + tv.tv_usec;
#endif
}

void dcf_sleep_ms(uint32_t ms) {
#ifdef DCF_PLATFORM_WINDOWS
    Sleep(ms);
#else
    usleep(ms * 1000);
#endif
}

void dcf_sleep_us(uint32_t us) {
#ifdef DCF_PLATFORM_WINDOWS
    if (us >= 1000) Sleep(us / 1000);
    else { /* Busy wait for sub-ms */ }
#else
    usleep(us);
#endif
}

void dcf_timer_start(dcf_timer_t* timer) {
    timer->start_ns = dcf_time_monotonic_ns();
    timer->elapsed_ns = 0;
}

uint64_t dcf_timer_stop(dcf_timer_t* timer) {
    timer->elapsed_ns = dcf_time_monotonic_ns() - timer->start_ns;
    return timer->elapsed_ns;
}

uint64_t dcf_timer_elapsed_ns(const dcf_timer_t* timer) {
    return timer->elapsed_ns;
}

double dcf_timer_elapsed_ms(const dcf_timer_t* timer) {
    return (double)timer->elapsed_ns / 1000000.0;
}

/* ============================================================================
 * Socket Functions
 * ============================================================================ */

int dcf_socket_init(void) {
#ifdef DCF_PLATFORM_WINDOWS
    WSADATA wsa;
    return WSAStartup(MAKEWORD(2, 2), &wsa);
#else
    return 0;
#endif
}

void dcf_socket_cleanup(void) {
#ifdef DCF_PLATFORM_WINDOWS
    WSACleanup();
#endif
}

int dcf_socket_set_nonblocking(dcf_socket_t sock, bool nonblocking) {
#ifdef DCF_PLATFORM_WINDOWS
    u_long mode = nonblocking ? 1 : 0;
    return ioctlsocket(sock, FIONBIO, &mode);
#else
    int flags = fcntl(sock, F_GETFL, 0);
    if (flags < 0) return -1;
    if (nonblocking) flags |= O_NONBLOCK;
    else flags &= ~O_NONBLOCK;
    return fcntl(sock, F_SETFL, flags);
#endif
}

int dcf_socket_set_nodelay(dcf_socket_t sock, bool nodelay) {
    int val = nodelay ? 1 : 0;
    return setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (const char*)&val, sizeof(val));
}

int dcf_socket_set_keepalive(dcf_socket_t sock, bool keepalive) {
    int val = keepalive ? 1 : 0;
    return setsockopt(sock, SOL_SOCKET, SO_KEEPALIVE, (const char*)&val, sizeof(val));
}

int dcf_socket_set_reuseaddr(dcf_socket_t sock, bool reuse) {
    int val = reuse ? 1 : 0;
    return setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const char*)&val, sizeof(val));
}

int dcf_socket_set_rcvbuf(dcf_socket_t sock, int size) {
    return setsockopt(sock, SOL_SOCKET, SO_RCVBUF, (const char*)&size, sizeof(size));
}

int dcf_socket_set_sndbuf(dcf_socket_t sock, int size) {
    return setsockopt(sock, SOL_SOCKET, SO_SNDBUF, (const char*)&size, sizeof(size));
}

int dcf_socket_set_rcvtimeo(dcf_socket_t sock, uint32_t timeout_ms) {
#ifdef DCF_PLATFORM_WINDOWS
    DWORD tv = timeout_ms;
    return setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, (const char*)&tv, sizeof(tv));
#else
    struct timeval tv;
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;
    return setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
#endif
}

int dcf_socket_set_sndtimeo(dcf_socket_t sock, uint32_t timeout_ms) {
#ifdef DCF_PLATFORM_WINDOWS
    DWORD tv = timeout_ms;
    return setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, (const char*)&tv, sizeof(tv));
#else
    struct timeval tv;
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;
    return setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
#endif
}

int dcf_socket_close(dcf_socket_t sock) {
#ifdef DCF_PLATFORM_WINDOWS
    return closesocket(sock);
#else
    return close(sock);
#endif
}

int dcf_socket_errno(void) {
#ifdef DCF_PLATFORM_WINDOWS
    return WSAGetLastError();
#else
    return errno;
#endif
}

const char* dcf_socket_strerror(int err) {
    static DCF_THREAD_LOCAL char buf[256];
#ifdef DCF_PLATFORM_WINDOWS
    FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, NULL, err, 0, buf, sizeof(buf), NULL);
#else
    strerror_r(err, buf, sizeof(buf));
#endif
    return buf;
}

/* ============================================================================
 * Dynamic Library Loading
 * ============================================================================ */

dcf_lib_t dcf_lib_open(const char* path) {
#ifdef DCF_PLATFORM_WINDOWS
    return LoadLibraryA(path);
#else
    return dlopen(path, RTLD_NOW | RTLD_LOCAL);
#endif
}

void dcf_lib_close(dcf_lib_t lib) {
    if (!lib) return;
#ifdef DCF_PLATFORM_WINDOWS
    FreeLibrary(lib);
#else
    dlclose(lib);
#endif
}

void* dcf_lib_symbol(dcf_lib_t lib, const char* name) {
#ifdef DCF_PLATFORM_WINDOWS
    return (void*)GetProcAddress(lib, name);
#else
    return dlsym(lib, name);
#endif
}

const char* dcf_lib_error(void) {
#ifdef DCF_PLATFORM_WINDOWS
    static DCF_THREAD_LOCAL char buf[256];
    FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(), 0, buf, sizeof(buf), NULL);
    return buf;
#else
    return dlerror();
#endif
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

int dcf_cpu_count(void) {
#ifdef DCF_PLATFORM_WINDOWS
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwNumberOfProcessors;
#elif defined(_SC_NPROCESSORS_ONLN)
    return (int)sysconf(_SC_NPROCESSORS_ONLN);
#else
    return 1;
#endif
}

size_t dcf_page_size(void) {
#ifdef DCF_PLATFORM_WINDOWS
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwPageSize;
#else
    return (size_t)sysconf(_SC_PAGESIZE);
#endif
}

int dcf_get_hostname(char* buffer, size_t size) {
#ifdef DCF_PLATFORM_WINDOWS
    DWORD sz = (DWORD)size;
    return GetComputerNameA(buffer, &sz) ? 0 : -1;
#else
    return gethostname(buffer, size);
#endif
}

int dcf_random_bytes(void* buffer, size_t size) {
    if (!buffer || size == 0) return 0;
    
#ifdef DCF_PLATFORM_LINUX
    ssize_t ret = getrandom(buffer, size, 0);
    return (ret == (ssize_t)size) ? 0 : -1;
#elif defined(DCF_PLATFORM_MACOS)
    return SecRandomCopyBytes(kSecRandomDefault, size, buffer) == errSecSuccess ? 0 : -1;
#elif defined(DCF_PLATFORM_WINDOWS)
    HCRYPTPROV hProv;
    if (!CryptAcquireContextA(&hProv, NULL, NULL, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT))
        return -1;
    BOOL ret = CryptGenRandom(hProv, (DWORD)size, buffer);
    CryptReleaseContext(hProv, 0);
    return ret ? 0 : -1;
#else
    /* Fallback: use /dev/urandom */
    FILE* f = fopen("/dev/urandom", "rb");
    if (!f) return -1;
    size_t n = fread(buffer, 1, size, f);
    fclose(f);
    return (n == size) ? 0 : -1;
#endif
}

uint32_t dcf_random_u32(void) {
    uint32_t val;
    if (dcf_random_bytes(&val, sizeof(val)) != 0) {
        /* Fallback to simple PRNG */
        static uint32_t seed = 0;
        if (seed == 0) seed = (uint32_t)dcf_time_monotonic_ns();
        seed = seed * 1103515245 + 12345;
        return seed;
    }
    return val;
}

uint64_t dcf_random_u64(void) {
    uint64_t val;
    if (dcf_random_bytes(&val, sizeof(val)) != 0) {
        return ((uint64_t)dcf_random_u32() << 32) | dcf_random_u32();
    }
    return val;
}
