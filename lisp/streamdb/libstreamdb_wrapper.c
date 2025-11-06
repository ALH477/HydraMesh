/* libstreamdb_wrapper.c */
#include "streamdb.h"
#include <string.h>
#include <uuid/uuid.h>  // For GUID generation

/* Wrapper for streamdb_open_with_config */
StreamDB* streamdb_open_with_config(const char* path, const void* config) {
    // Parse config JSON for flush_interval_ms
    // For now, use default 5000ms
    int flush_ms = 5000;
    
    // TODO: Parse config if needed, extract flush interval
    return streamdb_init(path, flush_ms);
}

/* Wrapper for streamdb_write_document that returns GUID */
char* streamdb_write_document(StreamDB* db, const char* path, 
                               const void* data, size_t size) {
    // Insert the data
    int result = streamdb_insert(db, (const unsigned char*)path, strlen(path), 
                                 data, size);
    
    if (!result) return NULL;
    
    // Generate a GUID to return
    uuid_t uuid;
    uuid_generate(uuid);
    
    char* guid_str = malloc(37);  // UUID string is 36 chars + null
    if (!guid_str) return NULL;
    
    uuid_unparse(uuid, guid_str);
    return guid_str;
}

/* Wrapper for streamdb_search */
char** streamdb_search(StreamDB* db, const char* prefix, size_t* count) {
    Result* results = streamdb_prefix_search(db, 
                                             (const unsigned char*)prefix, 
                                             strlen(prefix));
    
    // Count results
    *count = 0;
    for (Result* r = results; r; r = r->next) (*count)++;
    
    if (*count == 0) {
        streamdb_free_results(results);
        return NULL;
    }
    
    // Allocate array of strings
    char** paths = malloc(sizeof(char*) * (*count));
    if (!paths) {
        streamdb_free_results(results);
        return NULL;
    }
    
    // Copy keys to array
    size_t i = 0;
    for (Result* r = results; r; r = r->next) {
        paths[i] = malloc(r->key_len + 1);
        if (paths[i]) {
            memcpy(paths[i], r->key, r->key_len);
            paths[i][r->key_len] = '\0';
            i++;
        }
    }
    
    streamdb_free_results(results);
    return paths;
}

/* Stub for quick mode (not supported in base StreamDB) */
void streamdb_set_quick_mode(StreamDB* db, int quick) {
    // No-op: base StreamDB doesn't have quick mode
    (void)db;
    (void)quick;
}

/* Async stubs - not supported in base StreamDB */
int streamdb_get_async(StreamDB* db, const char* path, 
                       void* callback, void* user_data) {
    // Not implemented in base StreamDB
    return -1;  // Error
}

int streamdb_begin_async_transaction(StreamDB* db, void* callback, 
                                      void* user_data) {
    return -1;  // Not supported
}

int streamdb_commit_async_transaction(StreamDB* db, void* callback, 
                                       void* user_data) {
    return -1;  // Not supported
}

int streamdb_rollback_async_transaction(StreamDB* db, void* callback, 
                                         void* user_data) {
    return -1;  // Not supported
}
