/* libstreamdb_wrapper.h */
/*
 * Wrapper functions for StreamDB library.
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
 * Contact: DeMoD LLC
 */

#ifndef LIBSTREAMDB_WRAPPER_H
#define LIBSTREAMDB_WRAPPER_H

#include "streamdb.h"
#include <stddef.h>

/* Wrapper for streamdb_open_with_config */
StreamDB* streamdb_open_with_config(const char* path, const void* config);

/* Wrapper for streamdb_write_document that returns GUID */
char* streamdb_write_document(StreamDB* db, const char* path, 
                              const void* data, size_t size);

/* Wrapper for streamdb_search */
char** streamdb_search(StreamDB* db, const char* prefix, size_t* count);

/* Stub for quick mode (not supported in base StreamDB) */
void streamdb_set_quick_mode(StreamDB* db, int quick);

/* Async stubs - not supported in base StreamDB */
int streamdb_get_async(StreamDB* db, const char* path, 
                       void* callback, void* user_data);

int streamdb_begin_async_transaction(StreamDB* db, void* callback, 
                                     void* user_data);

int streamdb_commit_async_transaction(StreamDB* db, void* callback, 
                                      void* user_data);

int streamdb_rollback_async_transaction(StreamDB* db, void* callback, 
                                        void* user_data);

#endif /* LIBSTREAMDB_WRAPPER_H */
