// c_sdk/plugins/dcf_udp_transport.c
//
// Optimized UDP transport plugin for DCF.
// Implements ITransport interface for handshakeless, low-latency UDP communication.
// Supports P2P by binding locally and sending to parsed recipient addresses.
// No batching in this version; relies on DCF redundancy for reliability.
// Non-blocking receive with short timeout for responsiveness.
// Compile: gcc -shared -o libdcf_udp_transport.so dcf_udp_transport.c -I../include -fPIC

#include <dcf_sdk/dcf_plugin_manager.h>  // For ITransport
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>  // For sscanf in address parsing
#include <time.h>   // For timespec in select timeout

typedef struct {
    int sock;                  // UDP socket
    struct sockaddr_in local_addr;  // Local bind address
} DCFUdpTransport;

// Helper to parse "host:port" into sockaddr_in
static bool parse_address(const char* target, struct sockaddr_in* addr) {
    char host[256];
    int port;
    if (sscanf(target, "%255[^:]:%d", host, &port) != 2) {
        return false;
    }
    memset(addr, 0, sizeof(*addr));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);
    if (inet_pton(AF_INET, host, &addr->sin_addr) != 1) {
        return false;
    }
    return true;
}

bool udp_setup(void* self, const char* host, int port) {
    DCFUdpTransport* t = (DCFUdpTransport*)self;
    t->sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (t->sock < 0) {
        return false;
    }
    // Set non-blocking
    int flags = fcntl(t->sock, F_GETFL, 0);
    if (flags == -1 || fcntl(t->sock, F_SETFL, flags | O_NONBLOCK) == -1) {
        close(t->sock);
        return false;
    }
    // Bind locally for receiving (use "0.0.0.0" if host is localhost or similar)
    memset(&t->local_addr, 0, sizeof(t->local_addr));
    t->local_addr.sin_family = AF_INET;
    t->local_addr.sin_port = htons(port);
    t->local_addr.sin_addr.s_addr = INADDR_ANY;  // Bind to all interfaces
    if (bind(t->sock, (struct sockaddr*)&t->local_addr, sizeof(t->local_addr)) < 0) {
        close(t->sock);
        return false;
    }
    return true;
}

bool udp_send(void* self, const uint8_t* data, size_t size, const char* recipient) {
    DCFUdpTransport* t = (DCFUdpTransport*)self;
    struct sockaddr_in target_addr;
    if (!parse_address(recipient, &target_addr)) {
        return false;
    }
    // Minimal header: 4B sequence + 4B length (for basic integrity)
    uint8_t header[8];
    static uint32_t seq = 0;  // Per-instance sequence
    *(uint32_t*)header = seq++;
    *(uint32_t*)(header + 4) = (uint32_t)size;
    // Send header + data in one datagram
    struct iovec iov[2];
    iov[0].iov_base = header;
    iov[0].iov_len = 8;
    iov[1].iov_base = (void*)data;
    iov[1].iov_len = size;
    struct msghdr msg = {0};
    msg.msg_name = &target_addr;
    msg.msg_namelen = sizeof(target_addr);
    msg.msg_iov = iov;
    msg.msg_iovlen = 2;
    ssize_t sent = sendmsg(t->sock, &msg, 0);
    return sent == (ssize_t)(8 + size);
}

uint8_t* udp_receive(void* self, size_t* size) {
    DCFUdpTransport* t = (DCFUdpTransport*)self;
    uint8_t buf[65536];  // Max UDP size
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    // Use select for 1ms timeout to avoid blocking forever
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(t->sock, &readfds);
    struct timeval timeout = {0, 1000};  // 1ms
    int ready = select(t->sock + 1, &readfds, NULL, NULL, &timeout);
    if (ready <= 0) {
        *size = 0;
        return NULL;
    }
    ssize_t len = recvfrom(t->sock, buf, sizeof(buf), 0, (struct sockaddr*)&sender, &sender_len);
    if (len <= 8) {
        *size = 0;
        return NULL;
    }
    // Validate header
    uint32_t seq = *(uint32_t*)buf;  // Unused for now
    uint32_t msg_len = *(uint32_t*)(buf + 4);
    if ((size_t)len - 8 != msg_len) {
        *size = 0;
        return NULL;
    }
    uint8_t* data = (uint8_t*)malloc(msg_len);
    if (!data) {
        *size = 0;
        return NULL;
    }
    memcpy(data, buf + 8, msg_len);
    *size = msg_len;
    return data;
}

void udp_destroy(void* self) {
    DCFUdpTransport* t = (DCFUdpTransport*)self;
    if (t->sock >= 0) {
        close(t->sock);
    }
    free(t);
}

ITransport udp_iface = {
    .setup = udp_setup,
    .send = udp_send,
    .receive = udp_receive,
    .destroy = udp_destroy
};

void* create_plugin() {
    return calloc(1, sizeof(DCFUdpTransport));
}

const char* get_plugin_version() {
    return "1.0.0";
}
