/**
 * @file dcf_serialization.h
 * @brief DCF Serialization - Message serialization and deserialization
 * @version 5.1.0
 *
 * Provides thread-safe message serialization with validation,
 * supporting both text and binary message formats.
 */

#ifndef DCF_SERIALIZATION_H
#define DCF_SERIALIZATION_H

#include "dcf_types.h"
#include "dcf_error.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Text Message Serialization
 * ============================================================================ */

/**
 * @brief Serialize a text message
 * @param data Message data (null-terminated string)
 * @param sender Sender identifier
 * @param recipient Recipient identifier
 * @param out Output buffer (allocated, caller frees)
 * @param out_len Output for serialized length
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_serialize_text_message(const char* data, const char* sender,
                                     const char* recipient, uint8_t** out,
                                     size_t* out_len);

/**
 * @brief Serialize a message (alias for dcf_serialize_text_message)
 * @param data Message data (null-terminated string)
 * @param sender Sender identifier
 * @param recipient Recipient identifier
 * @param out Output buffer (allocated, caller frees)
 * @param out_len Output for serialized length
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_serialize_message(const char* data, const char* sender,
                                const char* recipient, uint8_t** out,
                                size_t* out_len);

/* ============================================================================
 * Binary Message Serialization
 * ============================================================================ */

/**
 * @brief Serialize a binary message
 * @param data Binary data
 * @param data_len Length of data
 * @param sender Sender identifier
 * @param recipient Recipient identifier
 * @param out Output buffer (allocated, caller frees)
 * @param out_len Output for serialized length
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_serialize_binary_message(const uint8_t* data, size_t data_len,
                                       const char* sender, const char* recipient,
                                       uint8_t** out, size_t* out_len);

/* ============================================================================
 * Health Request Serialization
 * ============================================================================ */

/**
 * @brief Serialize a health check request
 * @param peer Target peer identifier
 * @param out Output buffer (allocated, caller frees)
 * @param out_len Output for serialized length
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_serialize_health_request(const char* peer, uint8_t** out,
                                       size_t* out_len);

/* ============================================================================
 * Message Deserialization
 * ============================================================================ */

/**
 * @brief Deserialize a message
 * @param data Serialized data
 * @param len Length of serialized data
 * @param message_out Output for message content (allocated, caller frees)
 * @param sender_out Output for sender identifier (allocated, caller frees)
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_deserialize_message(const uint8_t* data, size_t len,
                                  char** message_out, char** sender_out);

/* ============================================================================
 * Message Validation
 * ============================================================================ */

/**
 * @brief Validate a message without full deserialization
 * @param data Serialized data
 * @param len Length of serialized data
 * @param valid_out Output for validation result
 * @return DCF_SUCCESS or error code
 *
 * @note This performs header validation only, not full message verification.
 */
DCFError dcf_validate_message(const uint8_t* data, size_t len, bool* valid_out);

/* ============================================================================
 * Size Estimation
 * ============================================================================ */

/**
 * @brief Estimate serialized message size
 * @param data_len Length of message data
 * @param sender Sender identifier
 * @param recipient Recipient identifier
 * @param size_out Output for estimated size
 * @return DCF_SUCCESS or error code
 *
 * @note Useful for pre-allocating buffers.
 */
DCFError dcf_estimate_message_size(size_t data_len, const char* sender,
                                    const char* recipient, size_t* size_out);

/* ============================================================================
 * Message Info Extraction
 * ============================================================================ */

/**
 * @brief Extract message metadata without full deserialization
 * @param data Serialized data
 * @param len Length of serialized data
 * @param sequence_out Output for sequence number (may be NULL)
 * @param timestamp_out Output for timestamp (may be NULL)
 * @param data_len_out Output for data length (may be NULL)
 * @return DCF_SUCCESS or error code
 */
DCFError dcf_get_message_info(const uint8_t* data, size_t len,
                               uint32_t* sequence_out, uint64_t* timestamp_out,
                               size_t* data_len_out);

#ifdef __cplusplus
}
#endif

#endif /* DCF_SERIALIZATION_H */
