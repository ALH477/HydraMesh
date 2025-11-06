/*
 * StreamDB - A lightweight, thread-safe embedded database using reverse trie
 * 
 * Copyright (C) 2025 DeMoD LLC
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * USAGE:
 * ------
 * StreamDB is an in-memory key-value store with optional file persistence.
 * It uses a reverse trie structure for efficient suffix-based searches.
 * 
 * Basic Example:
 *   StreamDB* db = streamdb_init("mydb.dat", 5000);  // File backend, 5s flush
 *   streamdb_insert(db, (unsigned char*)"key", 3, "value", 6);
 *   
 *   size_t size;
 *   void* val = streamdb_get(db, (unsigned char*)"key", 3, &size);
 *   if (val) {
 *     // Use val...
 *     free(val);  // Important: get() returns a copy that must be freed
 *   }
 *   
 *   streamdb_delete(db, (unsigned char*)"key", 3);
 *   streamdb_free(db);  // Auto-flushes before cleanup
 * 
 * Suffix Search:
 *   Result* results = streamdb_prefix_search(db, (unsigned char*)"suffix", 6);
 *   for (Result* r = results; r; r = r->next) {
 *     // Process r->key, r->value...
 *   }
 *   streamdb_free_results(results);
 * 
 * Features:
 *   - Thread-safe: All operations use internal mutex locking
 *   - Binary keys: Supports arbitrary byte sequences (not just strings)
 *   - Auto-flush: Background thread periodically saves to disk
 *   - Cross-platform: Windows, Linux, macOS, Unix
 *   - Memory-only mode: Pass NULL as file_path to streamdb_init()
 * 
 * Thread Safety:
 *   All public functions are thread-safe. Values returned by streamdb_get()
 *   are copies and must be freed by the caller.
 * 
 * Contact: DeMoD LLC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#define MUTEX_TYPE CRITICAL_SECTION
#define MUTEX_INIT(m) (InitializeCriticalSection(m), 0)
#define MUTEX_LOCK(m) EnterCriticalSection(m)
#define MUTEX_UNLOCK(m) LeaveCriticalSection(m)
#define MUTEX_DESTROY(m) DeleteCriticalSection(m)
#define THREAD_TYPE HANDLE
#define THREAD_CREATE(t, f, a) (t = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)f, a, 0, NULL)) != NULL ? 0 : GetLastError()
#define THREAD_JOIN(t) WaitForSingleObject(t, INFINITE); CloseHandle(t)
#define SLEEP_MS(ms) Sleep(ms)
#elif defined(__unix__) || defined(__APPLE__) || defined(__linux__)
#include <pthread.h>
#include <unistd.h>
#define MUTEX_TYPE pthread_mutex_t
#define MUTEX_INIT(m) ({ pthread_mutexattr_t attr; int ret = pthread_mutexattr_init(&attr) || pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE) || pthread_mutex_init(m, &attr); pthread_mutexattr_destroy(&attr); ret; })
#define MUTEX_LOCK(m) pthread_mutex_lock(m)
#define MUTEX_UNLOCK(m) pthread_mutex_unlock(m)
#define MUTEX_DESTROY(m) pthread_mutex_destroy(m)
#define THREAD_TYPE pthread_t
#define THREAD_CREATE(t, f, a) pthread_create(&t, NULL, f, a)
#define THREAD_JOIN(t) pthread_join(t, NULL)
#define SLEEP_MS(ms) usleep((ms) * 1000)
#else
#define MUTEX_TYPE int
#define MUTEX_INIT(m) (*(m) = 0, 0)
#define MUTEX_LOCK(m) ((void)0)
#define MUTEX_UNLOCK(m) ((void)0)
#define MUTEX_DESTROY(m) ((void)0)
#define THREAD_TYPE int
#define THREAD_CREATE(t, f, a) (0)
#define THREAD_JOIN(t) ((void)0)
#define SLEEP_MS(ms) ((void)0)
#endif

/* Constants */
#define MAX_CHILDREN 256
#define MAX_KEY_LEN 1024
#define DEFAULT_FLUSH_INTERVAL_MS 5000

/* Trie node structure */
typedef struct TrieNode {
    struct TrieNode* children[MAX_CHILDREN];
    void* value;
    size_t value_size;
    int is_end;
} TrieNode;

/* Database structure */
typedef struct StreamDB {
    TrieNode* root;
    size_t total_size;
    MUTEX_TYPE mutex;
    char* file_path;
    int is_file_backend;
    volatile int dirty;
    volatile int running;
    THREAD_TYPE auto_thread;
    int auto_flush_interval_ms;
} StreamDB;

/* Result structure for searches */
typedef struct Result {
    unsigned char* key;
    size_t key_len;
    void* value;
    size_t value_size;
    struct Result* next;
} Result;

/* Forward declarations */
static TrieNode* create_node(void);
static void free_node(TrieNode* node);
static int serialize_node(FILE* fp, TrieNode* node);
static TrieNode* deserialize_node(FILE* fp);
static void* auto_flush_thread(void* arg);
static int internal_flush(StreamDB* db);

/* Initialize a new trie node */
static TrieNode* create_node(void) {
    TrieNode* node = (TrieNode*)calloc(1, sizeof(TrieNode));
    if (!node) return NULL;
    node->is_end = 0;
    node->value = NULL;
    node->value_size = 0;
    return node;
}

/* Initialize the database with optional file path and flush interval */
StreamDB* streamdb_init(const char* file_path, int flush_interval_ms) {
    StreamDB* db = (StreamDB*)malloc(sizeof(StreamDB));
    if (!db) return NULL;
    
    db->root = create_node();
    if (!db->root) {
        free(db);
        return NULL;
    }
    
    db->total_size = 0;
    db->file_path = file_path ? strdup(file_path) : NULL;
    db->is_file_backend = (file_path != NULL);
    db->dirty = 0;
    db->running = 1;
    db->auto_flush_interval_ms = flush_interval_ms > 0 ? flush_interval_ms : DEFAULT_FLUSH_INTERVAL_MS;
    
    if (MUTEX_INIT(&db->mutex) != 0) {
        free_node(db->root);
        free(db->file_path);
        free(db);
        return NULL;
    }

    /* Load from file if exists */
    if (db->is_file_backend) {
        FILE* fp = fopen(db->file_path, "rb");
        if (fp) {
            TrieNode* loaded = deserialize_node(fp);
            fclose(fp);
            if (loaded) {
                free_node(db->root);
                db->root = loaded;
                db->dirty = 0;
            } else {
                fprintf(stderr, "Warning: Failed to load from file: %s\n", db->file_path);
            }
        }
        
        /* Start auto-flush thread */
        if (db->auto_flush_interval_ms > 0) {
            if (THREAD_CREATE(db->auto_thread, auto_flush_thread, db) != 0) {
                fprintf(stderr, "Warning: Failed to start auto-flush thread\n");
                db->running = 0;
            }
        } else {
            db->running = 0;
        }
    } else {
        db->running = 0;
    }
    
    return db;
}

/* Insert a key-value pair into the reverse trie */
int streamdb_insert(StreamDB* db, const unsigned char* key, size_t key_len, const void* value, size_t value_size) {
    if (!db || !key || key_len == 0 || !value || value_size == 0) return 0;
    if (key_len > MAX_KEY_LEN) return 0;
    
    MUTEX_LOCK(&db->mutex);
    
    TrieNode* current = db->root;

    /* Traverse/create nodes in reverse order */
    for (int i = (int)key_len - 1; i >= 0; i--) {
        unsigned char c = key[i];
        if (!current->children[c]) {
            current->children[c] = create_node();
            if (!current->children[c]) {
                MUTEX_UNLOCK(&db->mutex);
                return 0;
            }
        }
        current = current->children[c];
    }

    /* Update value at leaf */
    if (current->is_end && current->value) {
        db->total_size -= current->value_size;
        free(current->value);
    }
    
    current->value = malloc(value_size);
    if (!current->value) {
        MUTEX_UNLOCK(&db->mutex);
        return 0;
    }
    
    memcpy(current->value, value, value_size);
    current->value_size = value_size;
    current->is_end = 1;
    db->total_size += value_size;
    db->dirty = 1;
    
    MUTEX_UNLOCK(&db->mutex);
    return 1;
}

/* Internal get without mutex (for use within locked sections) */
static void* internal_get(StreamDB* db, const unsigned char* key, size_t key_len, size_t* value_size) {
    if (!db || !key || key_len == 0 || !value_size) return NULL;
    if (key_len > MAX_KEY_LEN) return NULL;

    TrieNode* current = db->root;

    /* Traverse in reverse order */
    for (int i = (int)key_len - 1; i >= 0; i--) {
        unsigned char c = key[i];
        if (!current->children[c]) {
            return NULL;
        }
        current = current->children[c];
    }

    if (current->is_end) {
        *value_size = current->value_size;
        return current->value;
    }
    
    return NULL;
}

/* Retrieve a value by key (returns COPY for thread safety) */
void* streamdb_get(StreamDB* db, const unsigned char* key, size_t key_len, size_t* value_size) {
    if (!db || !key || key_len == 0 || !value_size) return NULL;
    
    MUTEX_LOCK(&db->mutex);
    
    void* internal_value = internal_get(db, key, key_len, value_size);
    void* result = NULL;
    
    if (internal_value) {
        result = malloc(*value_size);
        if (result) {
            memcpy(result, internal_value, *value_size);
        } else {
            *value_size = 0;
        }
    }
    
    MUTEX_UNLOCK(&db->mutex);
    return result;
}

/* Helper to check if a node has no children */
static int has_no_children(TrieNode* node) {
    for (int i = 0; i < MAX_CHILDREN; i++) {
        if (node->children[i]) return 0;
    }
    return 1;
}

/* Recursive helper for delete */
static TrieNode* remove_helper(TrieNode* node, const unsigned char* key, size_t key_len, size_t index, size_t* removed_size) {
    if (!node) return NULL;

    if (index == key_len) {
        if (node->is_end) {
            *removed_size = node->value_size;
            free(node->value);
            node->value = NULL;
            node->value_size = 0;
            node->is_end = 0;
        }
        if (has_no_children(node) && !node->is_end) {
            free(node);
            return NULL;
        }
        return node;
    }

    unsigned char c = key[key_len - 1 - index];
    node->children[c] = remove_helper(node->children[c], key, key_len, index + 1, removed_size);

    if (has_no_children(node) && !node->is_end) {
        free(node);
        return NULL;
    }
    return node;
}

/* Delete a key-value pair */
int streamdb_delete(StreamDB* db, const unsigned char* key, size_t key_len) {
    if (!db || !key || key_len == 0 || key_len > MAX_KEY_LEN) return 0;

    MUTEX_LOCK(&db->mutex);

    size_t removed_size = 0;
    db->root = remove_helper(db->root, key, key_len, 0, &removed_size);
    
    if (removed_size > 0) {
        db->total_size -= removed_size;
        db->dirty = 1;
    }

    MUTEX_UNLOCK(&db->mutex);
    return 1;
}

/* Helper for prefix search to collect results */
static void collect_results(TrieNode* node, unsigned char* extension, size_t ext_len,
                            const unsigned char* rev_prefix, size_t prefix_len, Result** results) {
    if (!node) return;

    if (node->is_end) {
        size_t full_len = prefix_len + ext_len;
        if (full_len > MAX_KEY_LEN) return;
        
        unsigned char* full_rev = (unsigned char*)malloc(full_len);
        if (!full_rev) return;
        memcpy(full_rev, rev_prefix, prefix_len);
        memcpy(full_rev + prefix_len, extension, ext_len);

        unsigned char* key = (unsigned char*)malloc(full_len);
        if (!key) {
            free(full_rev);
            return;
        }
        for (size_t j = 0; j < full_len; j++) {
            key[j] = full_rev[full_len - 1 - j];
        }

        Result* res = (Result*)malloc(sizeof(Result));
        if (!res) {
            free(full_rev);
            free(key);
            return;
        }
        res->key = key;
        res->key_len = full_len;
        res->value = malloc(node->value_size);
        if (!res->value) {
            free(full_rev);
            free(key);
            free(res);
            return;
        }
        memcpy(res->value, node->value, node->value_size);
        res->value_size = node->value_size;
        res->next = *results;
        *results = res;

        free(full_rev);
    }

    for (int i = 0; i < MAX_CHILDREN; i++) {
        if (node->children[i]) {
            if (ext_len >= MAX_KEY_LEN - prefix_len) continue;
            extension[ext_len] = (unsigned char)i;
            collect_results(node->children[i], extension, ext_len + 1, rev_prefix, prefix_len, results);
        }
    }
}

/* Search for all keys with the given suffix */
Result* streamdb_prefix_search(StreamDB* db, const unsigned char* prefix, size_t prefix_len) {
    if (!db || !prefix || prefix_len == 0 || prefix_len > MAX_KEY_LEN) return NULL;

    MUTEX_LOCK(&db->mutex);
    
    TrieNode* current = db->root;

    unsigned char* rev_prefix = (unsigned char*)malloc(prefix_len);
    if (!rev_prefix) {
        MUTEX_UNLOCK(&db->mutex);
        return NULL;
    }
    
    for (int i = (int)prefix_len - 1; i >= 0; i--) {
        unsigned char c = prefix[i];
        rev_prefix[prefix_len - 1 - i] = c;
        if (!current->children[c]) {
            free(rev_prefix);
            MUTEX_UNLOCK(&db->mutex);
            return NULL;
        }
        current = current->children[c];
    }

    Result* results = NULL;
    unsigned char* extension = (unsigned char*)malloc(MAX_KEY_LEN);
    if (extension) {
        collect_results(current, extension, 0, rev_prefix, prefix_len, &results);
        free(extension);
    }
    free(rev_prefix);
    
    MUTEX_UNLOCK(&db->mutex);
    return results;
}

/* Free a result list */
void streamdb_free_results(Result* results) {
    while (results) {
        Result* next = results->next;
        free(results->key);
        free(results->value);
        free(results);
        results = next;
    }
}

/* Serialize a node recursively to file */
static int serialize_node(FILE* fp, TrieNode* node) {
    if (!node) return 0;

    if (fwrite(&node->is_end, sizeof(int), 1, fp) != 1) return 0;
    if (fwrite(&node->value_size, sizeof(size_t), 1, fp) != 1) return 0;

    if (node->is_end && node->value) {
        if (fwrite(node->value, 1, node->value_size, fp) != node->value_size) return 0;
    }

    int child_count = 0;
    for (int i = 0; i < MAX_CHILDREN; i++) {
        if (node->children[i]) child_count++;
    }
    if (fwrite(&child_count, sizeof(int), 1, fp) != 1) return 0;

    for (int i = 0; i < MAX_CHILDREN; i++) {
        if (node->children[i]) {
            unsigned char byte = (unsigned char)i;
            if (fwrite(&byte, sizeof(unsigned char), 1, fp) != 1) return 0;
            if (!serialize_node(fp, node->children[i])) return 0;
        }
    }
    return 1;
}

/* Deserialize a node recursively from file */
static TrieNode* deserialize_node(FILE* fp) {
    TrieNode* node = create_node();
    if (!node) return NULL;

    if (fread(&node->is_end, sizeof(int), 1, fp) != 1) goto fail;
    if (fread(&node->value_size, sizeof(size_t), 1, fp) != 1) goto fail;

    if (node->is_end) {
        node->value = malloc(node->value_size);
        if (!node->value) goto fail;
        if (fread(node->value, 1, node->value_size, fp) != node->value_size) goto fail;
    }

    int child_count;
    if (fread(&child_count, sizeof(int), 1, fp) != 1) goto fail;

    for (int i = 0; i < child_count; i++) {
        unsigned char byte;
        if (fread(&byte, sizeof(unsigned char), 1, fp) != 1) goto fail;
        node->children[byte] = deserialize_node(fp);
        if (!node->children[byte]) goto fail;
    }
    return node;

fail:
    free_node(node);
    return NULL;
}

/* Internal flush without locking */
static int internal_flush(StreamDB* db) {
    if (!db->is_file_backend || !db->file_path) return 0;

    char temp_path[1024];
    snprintf(temp_path, sizeof(temp_path), "%s.tmp", db->file_path);
    
    FILE* fp = fopen(temp_path, "wb");
    if (!fp) return 0;
    
    int success = serialize_node(fp, db->root);
    fclose(fp);
    
    if (success) {
        /* Atomic rename */
        remove(db->file_path);
        if (rename(temp_path, db->file_path) != 0) {
            remove(temp_path);
            return 0;
        }
    } else {
        remove(temp_path);
    }
    
    return success;
}

/* Flush the database to file */
int streamdb_flush(StreamDB* db) {
    if (!db || !db->is_file_backend) return 0;

    MUTEX_LOCK(&db->mutex);
    int success = internal_flush(db);
    if (success) db->dirty = 0;
    MUTEX_UNLOCK(&db->mutex);
    return success;
}

/* Background auto-flush thread */
static void* auto_flush_thread(void* arg) {
    StreamDB* db = (StreamDB*)arg;
    while (db->running) {
        SLEEP_MS(db->auto_flush_interval_ms);
        MUTEX_LOCK(&db->mutex);
        if (db->dirty) {
            internal_flush(db);
            db->dirty = 0;
        }
        MUTEX_UNLOCK(&db->mutex);
    }
    return NULL;
}

/* Free a trie node and its children */
static void free_node(TrieNode* node) {
    if (!node) return;
    for (int i = 0; i < MAX_CHILDREN; i++) {
        free_node(node->children[i]);
    }
    if (node->is_end && node->value) {
        free(node->value);
    }
    free(node);
}

/* Free the entire database */
void streamdb_free(StreamDB* db) {
    if (!db) return;
    
    /* Stop and join auto-flush thread */
    if (db->is_file_backend && db->running) {
        db->running = 0;
        THREAD_JOIN(db->auto_thread);
    }

    /* Final flush if dirty */
    if (db->is_file_backend) {
        MUTEX_LOCK(&db->mutex);
        if (db->dirty) {
            internal_flush(db);
        }
        MUTEX_UNLOCK(&db->mutex);
    }

    MUTEX_LOCK(&db->mutex);
    free_node(db->root);
    MUTEX_UNLOCK(&db->mutex);
    
    MUTEX_DESTROY(&db->mutex);
    free(db->file_path);
    free(db);
}

/* Example usage */
int main(void) {
    StreamDB* db = streamdb_init("streamdb.dat", 10000);
    if (!db) {
        printf("Failed to initialize database\n");
        return 1;
    }

    /* Insert test data */
    streamdb_insert(db, (const unsigned char*)"cat", 3, "Hello, World!", 14);
    streamdb_insert(db, (const unsigned char*)"car", 3, "Fast car", 9);
    streamdb_insert(db, (const unsigned char*)"cart", 4, "Shopping cart", 14);

    /* Retrieve a value (now returns copy - must free) */
    size_t value_size;
    char* value = (char*)streamdb_get(db, (const unsigned char*)"cat", 3, &value_size);
    if (value) {
        printf("Key: cat, Value: %.*s\n", (int)value_size - 1, value);
        free(value);  /* Important: free the copy */
    }

    /* Suffix search */
    Result* results = streamdb_prefix_search(db, (const unsigned char*)"ar", 2);
    printf("\nKeys ending with 'ar':\n");
    for (Result* r = results; r; r = r->next) {
        printf("  Key: %.*s, Value: %.*s\n", 
               (int)r->key_len, (char*)r->key, 
               (int)r->value_size - 1, (char*)r->value);
    }
    streamdb_free_results(results);

    /* Test delete */
    streamdb_delete(db, (const unsigned char*)"car", 3);
    value = (char*)streamdb_get(db, (const unsigned char*)"car", 3, &value_size);
    if (!value) {
        printf("\nSuccessfully deleted 'car'\n");
    }

    streamdb_free(db);
    return 0;
}
