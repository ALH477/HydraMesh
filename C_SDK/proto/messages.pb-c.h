#ifndef MESSAGES_PB_C_H
#define MESSAGES_PB_C_H
#include <protobuf-c/protobuf-c.h>

typedef struct _DCFMessage DCFMessage;
struct _DCFMessage {
    ProtobufCMessage base;
    char* sender;
    char* recipient;
    ProtobufCBinaryData data;
    int64_t timestamp;
    protobuf_c_boolean has_sync;
    protobuf_c_boolean sync;
    protobuf_c_boolean has_sequence;
    uint32_t sequence;
    char* redundancy_path;
    char* group_id;
};
#define DCF_MESSAGE__INIT { PROTOBUF_C_MESSAGE_INIT(&dcf_message__descriptor), NULL, NULL, {0, NULL}, 0, 0, 0, 0, 0, NULL, NULL }

typedef struct _HealthRequest HealthRequest;
struct _HealthRequest {
    ProtobufCMessage base;
    char* peer;
};
#define HEALTH_REQUEST__INIT { PROTOBUF_C_MESSAGE_INIT(&health_request__descriptor), NULL }

typedef struct _HealthResponse HealthResponse;
struct _HealthResponse {
    ProtobufCMessage base;
    protobuf_c_boolean healthy;
    char* status;
};
#define HEALTH_RESPONSE__INIT { PROTOBUF_C_MESSAGE_INIT(&health_response__descriptor), 0, NULL }

extern const ProtobufCMessageDescriptor dcf_message__descriptor;
extern const ProtobufCMessageDescriptor health_request__descriptor;
extern const ProtobufCMessageDescriptor health_response__descriptor;

size_t dcf_message__get_packed_size(const DCFMessage *message);
size_t dcf_message__pack(const DCFMessage *message, uint8_t *out);
DCFMessage *dcf_message__unpack(ProtobufCAllocator *allocator, size_t len, const uint8_t *data);
void dcf_message__free_unpacked(DCFMessage *message, ProtobufCAllocator *allocator);

size_t health_request__get_packed_size(const HealthRequest *message);
size_t health_request__pack(const HealthRequest *message, uint8_t *out);
HealthRequest *health_request__unpack(ProtobufCAllocator *allocator, size_t len, uint8_t *data);
void health_request__free_unpacked(HealthRequest *message, ProtobufCAllocator *allocator);

size_t health_response__get_packed_size(const HealthResponse *message);
size_t health_response__pack(const HealthResponse *message, uint8_t *out);
HealthResponse *health_response__unpack(ProtobufCAllocator *allocator, size_t len, const uint8_t *data);
void health_response__free_unpacked(HealthResponse *message, ProtobufCAllocator *allocator);

#endif
