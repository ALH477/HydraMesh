#include <dcf_sdk/dcf_plugin_manager.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

typedef struct {
    int read_fd;
    int write_fd;
    const char* path;
    uint32_t seq;
} NamedPipeTransport;

bool namedpipe_setup(void* self, const char* path, int unused) {
    NamedPipeTransport* npt = (NamedPipeTransport*)self;
    npt->path = strdup(path);
    unlink(path);
    if (mkfifo(path, 0666) < 0) return false;
    npt->read_fd = open(path, O_RDWR | O_NONBLOCK);
    npt->write_fd = open(path, O_RDWR | O_NONBLOCK);
    npt->seq = 0;
    return npt->read_fd >= 0 && npt->write_fd >= 0;
}

bool namedpipe_send(void* self, const uint8_t* data, size_t size, const char* target) {
    NamedPipeTransport* npt = (NamedPipeTransport*)self;
    uint32_t header[2] = {htonl(npt->seq++), htonl(size)};
    write(npt->write_fd, header, sizeof(header));
    ssize_t sent = write(npt->write_fd, data, size);
    return sent == size;
}

uint8_t* namedpipe_receive(void* self, size_t* size) {
    NamedPipeTransport* npt = (NamedPipeTransport*)self;
    uint32_t header[2];
    ssize_t read_bytes = read(npt->read_fd, header, sizeof(header));
    if (read_bytes != sizeof(header)) return NULL;
    uint32_t seq = ntohl(header[0]);
    uint32_t len = ntohl(header[1]);
    uint8_t* buf = malloc(len);
    *size = read(npt->read_fd, buf, len);
    if (*size != len) { free(buf); return NULL; }
    return buf;
}

void namedpipe_destroy(void* self) {
    NamedPipeTransport* npt = (NamedPipeTransport*)self;
    close(npt->read_fd);
    close(npt->write_fd);
    unlink(npt->path);
    free((void*)npt->path);
    free(self);
}

ITransport iface = {namedpipe_setup, namedpipe_send, namedpipe_receive, namedpipe_destroy};

void* create_plugin() { return calloc(1, sizeof(NamedPipeTransport)); }

const char* get_plugin_version() { return "1.0"; }
