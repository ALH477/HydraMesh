#include "dcf_serialization.h"
#include "messages.pb-c.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>

DCFError dcf_serialize_message(const char* data, const char* sender, const char* recipient, uint8_t** serialized_out, size_t* len_out) {
    if (!data || !sender || !recipient || !serialized_out || !len_out) return DCF_ERR_NULL_PTR;
    DCFMessage msg = DCF_MESSAGE__INIT;
    msg.sender = strdup(sender);
    msg.recipient = strdup(recipient);
    msg.data.data = (uint8_t*)strdup(data);
    msg.data.len = strlen(data);
    msg.timestamp = time(NULL);
    msg.has_sync = true;
    msg.sync = false;
    msg.has_sequence = true;
    msg.sequence = rand() % 1000;
    *len_out = dcf_message__get_packed_size(&msg);
    *serialized_out = calloc(1, *len_out);
    if (!*serialized_out) {
        free(msg.sender);
        free(msg.recipient);
        free(msg.data.data);
        return DCF_ERR_MALLOC_FAIL;
    }
    dcf_message__pack(&msg, *serialized_out);
    free(msg.sender);
    free(msg.recipient);
    free(msg.data.data);
    return DCF_SUCCESS;
}

DCFError dcf_serialize_health_request(const char* peer, uint8_t** serialized_out, size_t* len_out) {
    if (!peer || !serialized_out || !len_out) return DCF_ERR_NULL_PTR;
    HealthRequest req = HEALTH_REQUEST__INIT;
    req.peer = strdup(peer);
    *len_out = health_request__get_packed_size(&req);
    *serialized_out = calloc(1, *len_out);
    if (!*serialized_out) {
        free(req.peer);
        return DCF_ERR_MALLOC_FAIL;
    }
    health_request__pack(&req, *serialized_out);
    free(req.peer);
    return DCF_SUCCESS;
}

DCFError dcf_deserialize_message(const uint8_t* data, size_t len, char** message_out, char** sender_out) {
    if (!data || !message_out || !sender_out || len == 0) return DCF_ERR_NULL_PTR;
    DCFMessage* msg = dcf_message__unpack(NULL, len, data);
    if (!msg) return DCF_ERR_DESERIALIZATION_FAIL;
    *message_out = strdup((char*)msg->data.data);
    *sender_out = strdup(msg->sender);
    dcf_message__free_unpacked(msg, NULL);
    if (!*message_out || !*sender_out) {
        free(*message_out);
        free(*sender_out);
        return DCF_ERR_MALLOC_FAIL;
    }
    return DCF_SUCCESS;
}
