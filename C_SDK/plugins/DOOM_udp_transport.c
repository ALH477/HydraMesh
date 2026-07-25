// SPDX-License-Identifier: LGPL-3.0-only
#define _DEFAULT_SOURCE /* strndup under -std=c11 */
#include <dcf_sdk/dcf_plugin_manager.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

/* Maximum UDP payload over IPv4: 65535 - 20 (IP) - 8 (UDP). */
#define UDP_MAX_PAYLOAD 65507

typedef struct {
    int sock;
    struct sockaddr_in addr;
} UDPTransport;

bool udp_setup(void* self, const char* host, int port) {
    UDPTransport* t = (UDPTransport*)self;
    t->sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (t->sock < 0) return false;
    t->addr.sin_family = AF_INET;
    t->addr.sin_port = htons(port);
    if (inet_pton(AF_INET, host, &t->addr.sin_addr) != 1) {
        close(t->sock);
        t->sock = -1;
        return false;
    }
    return bind(t->sock, (struct sockaddr*)&t->addr, sizeof(t->addr)) == 0;
}

bool udp_send(void* self, const uint8_t* data, size_t size, const char* target) {
    if (size > UDP_MAX_PAYLOAD) return false;
    UDPTransport* t = (UDPTransport*)self;
    struct sockaddr_in dest;
    memset(&dest, 0, sizeof(dest));
    dest.sin_family = AF_INET;
    size_t colon = strcspn(target, ":");
    if (target[colon] != ':') return false;
    int port = atoi(target + colon + 1);
    if (port <= 0 || port > 65535) return false;
    char* addr = strndup(target, colon);
    if (!addr) return false;
    dest.sin_port = htons((uint16_t)port);
    int ok = inet_pton(AF_INET, addr, &dest.sin_addr);
    free(addr);
    if (ok != 1) return false;
    ssize_t sent = sendto(t->sock, data, size, 0, (struct sockaddr*)&dest, sizeof(dest));
    return sent == (ssize_t)size;
}

uint8_t* udp_receive(void* self, size_t* size) {
    UDPTransport* t = (UDPTransport*)self;
    *size = 0;
    uint8_t* buf = (uint8_t*)malloc(UDP_MAX_PAYLOAD);
    if (!buf) return NULL;
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    /* MSG_TRUNC makes recvfrom return the full datagram length even when it
     * exceeds the buffer, so an oversize datagram is detected and dropped
     * instead of silently clipped. */
    ssize_t len = recvfrom(t->sock, buf, UDP_MAX_PAYLOAD, MSG_TRUNC,
                           (struct sockaddr*)&sender, &sender_len);
    if (len <= 0 || len > UDP_MAX_PAYLOAD) {
        free(buf);
        return NULL;
    }
    *size = (size_t)len;
    return buf;
}

void udp_destroy(void* self) {
    UDPTransport* t = (UDPTransport*)self;
    if (t->sock >= 0) close(t->sock);
    free(self);
}

DCFTransportV1 iface = {udp_setup, udp_send, udp_receive, udp_destroy};

void* create_plugin() {
    return calloc(1, sizeof(UDPTransport));
}

const char* get_plugin_version() { return "1.0.0"; }
