#include <dcf_sdk/dcf_plugin_manager.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

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
    inet_pton(AF_INET, host, &t->addr.sin_addr);
    return bind(t->sock, (struct sockaddr*)&t->addr, sizeof(t->addr)) == 0;
}

bool udp_send(void* self, const uint8_t* data, size_t size, const char* target) {
    UDPTransport* t = (UDPTransport*)self;
    struct sockaddr_in dest;
    dest.sin_family = AF_INET;
    size_t colon = strcspn(target, ":");
    char* addr = strndup(target, colon);
    dest.sin_port = htons(atoi(target + colon + 1));
    inet_pton(AF_INET, addr, &dest.sin_addr);
    free(addr);
    return sendto(t->sock, data, size > 512 ? 512 : size, 0, (struct sockaddr*)&dest, sizeof(dest)) >= 0;
}

uint8_t* udp_receive(void* self, size_t* size) {
    UDPTransport* t = (UDPTransport*)self;
    uint8_t* buf = (uint8_t*)calloc(512, 1);
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    ssize_t len = recvfrom(t->sock, buf, 512, 0, (struct sockaddr*)&sender, &sender_len);
    *size = len > 0 ? len : 0;
    return len > 0 ? buf : (free(buf), NULL);
}

void udp_destroy(void* self) {
    UDPTransport* t = (UDPTransport*)self;
    if (t->sock >= 0) close(t->sock);
    free(self);
}

ITransport iface = {udp_setup, udp_send, udp_receive, udp_destroy};

void* create_plugin() {
    return calloc(1, sizeof(UDPTransport));
}

const char* get_plugin_version() { return "1.0.0"; }
