// SPDX-License-Identifier: LGPL-3.0-only
#include <dcf_sdk/dcf_plugin_manager.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

/* Cap on a wire-declared message length; anything larger is a framing error. */
#define UNIXSOCKET_MAX_MSG (1u << 20)

/* recv() on SOCK_STREAM may return short; loop until exactly n bytes or error. */
static bool recv_full(int fd, void* buf, size_t n) {
    uint8_t* p = (uint8_t*)buf;
    while (n > 0) {
        ssize_t r = recv(fd, p, n, 0);
        if (r <= 0) return false;
        p += r;
        n -= (size_t)r;
    }
    return true;
}

typedef struct {
    int sock;
    struct sockaddr_un addr;
} UnixSocketTransport;

bool unixsocket_setup(void* self, const char* path, int unused) {
    UnixSocketTransport* ust = (UnixSocketTransport*)self;
    ust->sock = socket(AF_UNIX, SOCK_STREAM, 0);
    if (ust->sock < 0) return false;
    memset(&ust->addr, 0, sizeof(ust->addr));
    ust->addr.sun_family = AF_UNIX;
    strncpy(ust->addr.sun_path, path, sizeof(ust->addr.sun_path) - 1);
    unlink(path);
    if (bind(ust->sock, (struct sockaddr*)&ust->addr, sizeof(ust->addr)) < 0) {
        close(ust->sock);
        return false;
    }
    listen(ust->sock, 5);
    return true;
}

bool unixsocket_send(void* self, const uint8_t* data, size_t size, const char* target) {
    (void)self;
    if (size == 0 || size > UNIXSOCKET_MAX_MSG) return false;
    int client_sock = socket(AF_UNIX, SOCK_STREAM, 0);
    if (client_sock < 0) return false;
    struct sockaddr_un target_addr;
    memset(&target_addr, 0, sizeof(target_addr));
    target_addr.sun_family = AF_UNIX;
    strncpy(target_addr.sun_path, target, sizeof(target_addr.sun_path) - 1);
    if (connect(client_sock, (struct sockaddr*)&target_addr, sizeof(target_addr)) < 0) {
        close(client_sock);
        return false;
    }
    uint32_t len = htonl((uint32_t)size);
    bool ok = send(client_sock, &len, sizeof(len), 0) == (ssize_t)sizeof(len);
    ok = ok && send(client_sock, data, size, 0) == (ssize_t)size;
    close(client_sock);
    return ok;
}

uint8_t* unixsocket_receive(void* self, size_t* size) {
    UnixSocketTransport* ust = (UnixSocketTransport*)self;
    *size = 0;
    int client_sock = accept(ust->sock, NULL, NULL);
    if (client_sock < 0) return NULL;
    uint32_t len;
    if (!recv_full(client_sock, &len, sizeof(len))) {
        close(client_sock);
        return NULL;
    }
    len = ntohl(len);
    /* The length is wire-supplied: cap it before trusting it with malloc. */
    if (len == 0 || len > UNIXSOCKET_MAX_MSG) {
        close(client_sock);
        return NULL;
    }
    uint8_t* buf = malloc(len);
    if (!buf) {
        close(client_sock);
        return NULL;
    }
    bool ok = recv_full(client_sock, buf, len);
    close(client_sock);
    if (!ok) { free(buf); return NULL; }
    *size = len;
    return buf;
}

void unixsocket_destroy(void* self) {
    UnixSocketTransport* ust = (UnixSocketTransport*)self;
    unlink(ust->addr.sun_path);
    close(ust->sock);
    free(self);
}

DCFTransportV1 iface = {unixsocket_setup, unixsocket_send, unixsocket_receive, unixsocket_destroy};

void* create_plugin() { return calloc(1, sizeof(UnixSocketTransport)); }

const char* get_plugin_version() { return "1.0"; }
