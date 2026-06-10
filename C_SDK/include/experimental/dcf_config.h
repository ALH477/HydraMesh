/**
 * @file dcf_config.h
 * @brief Thread-Safe Configuration System with Validation
 * @version 5.2.0
 */

#ifndef DCF_CONFIG_H
#define DCF_CONFIG_H

#include "dcf_platform.h"
#include "dcf_error.h"
#include "dcf_types.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum DCFConfigType {
    DCF_CONFIG_TYPE_NULL    = 0,
    DCF_CONFIG_TYPE_BOOL    = 1,
    DCF_CONFIG_TYPE_INT     = 2,
    DCF_CONFIG_TYPE_INT64   = 3,
    DCF_CONFIG_TYPE_DOUBLE  = 4,
    DCF_CONFIG_TYPE_STRING  = 5,
    DCF_CONFIG_TYPE_ARRAY   = 6,
    DCF_CONFIG_TYPE_OBJECT  = 7
} DCFConfigType;

typedef enum DCFConfigFlags {
    DCF_CONFIG_NONE         = 0,
    DCF_CONFIG_REQUIRED     = 1 << 0,
    DCF_CONFIG_SECRET       = 1 << 1,
    DCF_CONFIG_READONLY     = 1 << 2,
    DCF_CONFIG_DEPRECATED   = 1 << 3,
    DCF_CONFIG_ENV_OVERRIDE = 1 << 4
} DCFConfigFlags;

typedef struct DCFConfig DCFConfig;

typedef bool (*DCFConfigChangeCallback)(const char* key, const char* old_value,
                                         const char* new_value, void* user_data);

typedef bool (*DCFConfigValidator)(const char* key, const char* value,
                                    char* error_msg, size_t error_len, void* user_data);

typedef struct DCFConfigSchema {
    const char* key;
    DCFConfigType type;
    DCFConfigFlags flags;
    const char* default_value;
    const char* env_var;
    const char* description;
    DCFConfigValidator validator;
    void* validator_data;
    int64_t min_int;
    int64_t max_int;
    double min_double;
    double max_double;
    size_t min_length;
    size_t max_length;
    const char* pattern;
    size_t min_items;
    size_t max_items;
} DCFConfigSchema;

#define DCF_CONFIG_SCHEMA_END { NULL, 0, 0, NULL, NULL, NULL, NULL, NULL, 0, 0, 0.0, 0.0, 0, 0, NULL, 0, 0 }

/* Lifecycle */
DCF_API DCFConfig* dcf_config_create(void);
DCF_API DCFConfig* dcf_config_create_with_schema(const DCFConfigSchema* schema);
DCF_API void dcf_config_free(DCFConfig* config);
DCF_API DCFConfig* dcf_config_clone(const DCFConfig* config);

/* Loading */
DCF_API DCFError dcf_config_load_file(DCFConfig* config, const char* path);
DCF_API DCFError dcf_config_load_string(DCFConfig* config, const char* json);
DCF_API DCFError dcf_config_load_env(DCFConfig* config, const char* prefix);
DCF_API DCFError dcf_config_merge(DCFConfig* config, const DCFConfig* other);
DCF_API DCFError dcf_config_save_file(const DCFConfig* config, const char* path);
DCF_API DCFError dcf_config_save_string(const DCFConfig* config, char** json_out);
DCF_API DCFError dcf_config_reload(DCFConfig* config);

/* Validation */
DCF_API size_t dcf_config_validate(const DCFConfig* config, char** errors_out, size_t max_errors);
DCF_API bool dcf_config_validate_value(const DCFConfig* config, const char* key,
                                        const char* value, char* error_msg, size_t error_len);
DCF_API bool dcf_config_is_complete(const DCFConfig* config);

/* Getters */
DCF_API bool dcf_config_has(const DCFConfig* config, const char* key);
DCF_API DCFConfigType dcf_config_get_type(const DCFConfig* config, const char* key);
DCF_API const char* dcf_config_get_string(const DCFConfig* config, const char* key, const char* default_val);
DCF_API DCFError dcf_config_get_string_copy(const DCFConfig* config, const char* key, char* buf, size_t buf_len);
DCF_API bool dcf_config_get_bool(const DCFConfig* config, const char* key, bool default_val);
DCF_API int dcf_config_get_int(const DCFConfig* config, const char* key, int default_val);
DCF_API int64_t dcf_config_get_int64(const DCFConfig* config, const char* key, int64_t default_val);
DCF_API double dcf_config_get_double(const DCFConfig* config, const char* key, double default_val);
DCF_API DCFError dcf_config_get_string_array(const DCFConfig* config, const char* key, char*** values, size_t* count);
DCF_API DCFError dcf_config_get_int_array(const DCFConfig* config, const char* key, int** values, size_t* count);

/* Setters */
DCF_API DCFError dcf_config_set_string(DCFConfig* config, const char* key, const char* value);
DCF_API DCFError dcf_config_set_bool(DCFConfig* config, const char* key, bool value);
DCF_API DCFError dcf_config_set_int(DCFConfig* config, const char* key, int value);
DCF_API DCFError dcf_config_set_int64(DCFConfig* config, const char* key, int64_t value);
DCF_API DCFError dcf_config_set_double(DCFConfig* config, const char* key, double value);
DCF_API DCFError dcf_config_remove(DCFConfig* config, const char* key);
DCF_API void dcf_config_clear(DCFConfig* config);

/* Change notification */
DCF_API uint32_t dcf_config_on_change(DCFConfig* config, DCFConfigChangeCallback callback, void* user_data);
DCF_API uint32_t dcf_config_on_key_change(DCFConfig* config, const char* key,
                                           DCFConfigChangeCallback callback, void* user_data);
DCF_API void dcf_config_off_change(DCFConfig* config, uint32_t id);

/* Iteration */
typedef bool (*DCFConfigIterator)(const char* key, const char* value, DCFConfigType type, void* user_data);
DCF_API void dcf_config_foreach(const DCFConfig* config, DCFConfigIterator iterator, void* user_data);
DCF_API DCFError dcf_config_get_keys(const DCFConfig* config, const char* prefix, char*** keys, size_t* count);

/* Utility */
DCF_API void dcf_config_dump(const DCFConfig* config, bool include_secrets);
DCF_API const char* dcf_config_get_file_path(const DCFConfig* config);
DCF_API uint64_t dcf_config_get_file_mtime(const DCFConfig* config);
DCF_API bool dcf_config_file_changed(const DCFConfig* config);
DCF_API const DCFConfigSchema* dcf_config_default_schema(void);
DCF_API DCFConfig* dcf_config_load_default(void);

#ifdef __cplusplus
}
#endif

#endif /* DCF_CONFIG_H */
