#include <dcf_sdk/dcf_plugin_manager.h>
#include <dcf_sdk/dcf_jit_hash.h>
#include <pthread.h>

typedef struct {
    void* mem;
    HashFunc hash_func;
    uint32_t current_prefix;
    pthread_mutex_t mutex;
} JitHash;

bool setup(void* self, const char* config) {
    JitHash* jh = (JitHash*)self;
    pthread_mutex_init(&jh->mutex, NULL);
    pthread_mutex_lock(&jh->mutex);
    jh->mem = jit_alloc_exec(4096);
    if (!jh->mem) {
        pthread_mutex_unlock(&jh->mutex);
        return false;
    }
    jh->current_prefix = 0xC0A80000;  // 192.168.*
    jit_generate_ip_hash(jh->mem, jh->current_prefix);
    jh->hash_func = (HashFunc)jh->mem;
    pthread_mutex_unlock(&jh->mutex);
    return true;
}

HashFunc get_hash_func(void* self) {
    JitHash* jh = (JitHash*)self;
    pthread_mutex_lock(&jh->mutex);
    HashFunc func = jh->hash_func;
    pthread_mutex_unlock(&jh->mutex);
    return func;
}

void regenerate(void* self) {
    JitHash* jh = (JitHash*)self;
    pthread_mutex_lock(&jh->mutex);
    jh->current_prefix += 1;
    jit_generate_ip_hash(jh->mem, jh->current_prefix);
    pthread_mutex_unlock(&jh->mutex);
}

void destroy(void* self) {
    JitHash* jh = (JitHash*)self;
    pthread_mutex_lock(&jh->mutex);
    if (jh->mem) munmap(jh->mem, 4096);
    pthread_mutex_unlock(&jh->mutex);
    pthread_mutex_destroy(&jh->mutex);
    free(self);
}

IJitHash iface = {get_hash_func, regenerate, destroy};

void* create_plugin() {
    JitHash* jh = calloc(1, sizeof(JitHash));
    if (!jh) return NULL;
    return jh;
}

const char* get_plugin_version() { return "1.0.0"; }
