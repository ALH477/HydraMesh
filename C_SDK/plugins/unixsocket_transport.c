#include <dcf_sdk/dcf_plugin_manager.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

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
    UnixSocketTransport* ust = (UnixSocketTransport*)self;
    int client_sock = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un target_addr;
    memset(&target_addr, 0, sizeof(target_addr));
    target_addr.sun_family = AF_UNIX;
    strncpy(target_addr.sun_path, target, sizeof(target_addr.sun_path) - 1);
    if (connect(client_sock, (struct sockaddr*)&target_addr, sizeof(target_addr)) < 0) {
        close(client_sock);
        return false;
    }
    uint32_t len = htonl(size);
    send(client_sock, &len, sizeof(len), 0);
    ssize_t sent = send(client_sock, data, size, 0);
    close(client_sock);
    return sent == size;
}

uint8_t* unixsocket_receive(void* self, size_t* size) {
    UnixSocketTransport* ust = (UnixSocketTransport*)self;
    int client_sock = accept(ust->sock, NULL, NULL);
    if (client_sock < 0) return NULL;
    uint32_t len;
    recv(client_sock, &len, sizeof(len), 0);
    len = ntohl(len);
    uint8_t* buf = malloc(len);
    *size = recv(client_sock, buf, len, 0);
    close(client_sock);
    if (*size != len) { free(buf); return NULL; }
    return buf;
}

void unixsocket_destroy(void* self) {
    UnixSocketTransport* ust = (UnixSocketTransport*)self;
    unlink(ust->addr.sun_path);
    close(ust->sock);
    free(self);
}

ITransport iface = {unixsocket_setup, unixsocket_send, unixsocket_receive, unixsocket_destroy};

void* create_plugin() { return calloc(1, sizeof(UnixSocketTransport)); }

const char* get_plugin_version() { return "1.0"; }
