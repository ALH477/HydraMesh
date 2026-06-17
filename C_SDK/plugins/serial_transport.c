// SPDX-License-Identifier: LGPL-3.0-only
#include <dcf_sdk/dcf_plugin_manager.h>
#include <termios.h>
#include <fcntl.h>
#include <unistd.h>

typedef struct {
    int fd;
    struct termios old_tio;
    int baud;
} SerialTransport;

bool serial_setup(void* self, const char* device, int baud) {
    SerialTransport* st = (SerialTransport*)self;
    st->baud = baud ? baud : 9600;
    st->fd = open(device, O_RDWR | O_NOCTTY | O_NDELAY);
    if (st->fd < 0) return false;
    tcgetattr(st->fd, &st->old_tio);
    struct termios new_tio = st->old_tio;
    new_tio.c_cflag = st->baud | CS8 | CLOCAL | CREAD;
    new_tio.c_iflag = IGNPAR;
    new_tio.c_oflag = 0;
    new_tio.c_lflag = 0;
    tcflush(st->fd, TCIFLUSH);
    tcsetattr(st->fd, TCSANOW, &new_tio);
    return true;
}

bool serial_send(void* self, const uint8_t* data, size_t size, const char* target) {
    (void)target;
    SerialTransport* st = (SerialTransport*)self;
    ssize_t sent = write(st->fd, data, size);
    return sent == (ssize_t)size;
}

uint8_t* serial_receive(void* self, size_t* size) {
    SerialTransport* st = (SerialTransport*)self;
    uint8_t* buf = malloc(1024);
    *size = read(st->fd, buf, 1024);
    if (*size <= 0) { free(buf); return NULL; }
    // COBS decode (simplified)
    return buf;
}

void serial_destroy(void* self) {
    SerialTransport* st = (SerialTransport*)self;
    tcsetattr(st->fd, TCSANOW, &st->old_tio);
    close(st->fd);
    free(self);
}

DCFTransportV1 iface = {serial_setup, serial_send, serial_receive, serial_destroy};

void* create_plugin() { return calloc(1, sizeof(SerialTransport)); }

const char* get_plugin_version() { return "1.0"; }
