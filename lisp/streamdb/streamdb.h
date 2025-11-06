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

#ifndef STREAMDB_H
#define STREAMDB_H

#include <stddef.h>

/* Opaque database handle */
typedef struct StreamDB StreamDB;

/* Result structure for searches */
typedef struct Result {
    unsigned char* key;
    size_t key_len;
    void* value;
    size_t value_size;
    struct Result* next;
} Result;

/* Initialize the database with optional file path and flush interval */
StreamDB* streamdb_init(const char* file_path, int flush_interval_ms);

/* Insert a key-value pair into the reverse trie */
int streamdb_insert(StreamDB* db, const unsigned char* key, size_t key_len, const void* value, size_t value_size);

/* Retrieve a value by key (returns COPY for thread safety) */
void* streamdb_get(StreamDB* db, const unsigned char* key, size_t key_len, size_t* value_size);

/* Delete a key-value pair */
int streamdb_delete(StreamDB* db, const unsigned char* key, size_t key_len);

/* Search for all keys with the given suffix */
Result* streamdb_prefix_search(StreamDB* db, const unsigned char* prefix, size_t prefix_len);

/* Free a result list */
void streamdb_free_results(Result* results);

/* Flush the database to file */
int streamdb_flush(StreamDB* db);

/* Free the entire database */
void streamdb_free(StreamDB* db);

#endif /* STREAMDB_H */
