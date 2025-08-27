#include <dcf_sdk/dcf_plugin_manager.h>
#include <linux/can.h>
#include <linux/can/raw.h>
#include <net/if.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

typedef struct {
    int sock;
} CanTransport;

bool can_setup(void* self, const char* iface, int unused) {
    CanTransport* ct = (CanTransport*)self;
    struct ifreq ifr;
    struct sockaddr_can addr;
    ct->sock = socket(PF_CAN, SOCK_RAW, CAN_RAW);
    if (ct->sock < 0) return false;
    strcpy(ifr.ifr_name, iface);
    ioctl(ct->sock, SIOCGIFINDEX, &ifr);
    addr.can_family = AF_CAN;
    addr.can_ifindex = ifr.ifr_ifindex;
    return bind(ct->sock, (struct sockaddr*)&addr, sizeof(addr)) >= 0;
}

bool can_send(void* self, const uint8_t* data, size_t size, const char* target) {
    CanTransport* ct = (CanTransport*)self;
    struct can_frame frame = {0};
    frame.can_id = atoi(target);
    frame.can_dlc = size > 8 ? 8 : size;
    memcpy(frame.data, data, frame.can_dlc);
    return send(ct->sock, &frame, sizeof(frame), 0) > 0;
}

uint8_t* can_receive(void* self, size_t* size) {
    CanTransport* ct = (CanTransport*)self;
    struct can_frame frame;
    *size = recv(ct->sock, &frame, sizeof(frame), 0);
    if (*size <= 0) return NULL;
    uint8_t* buf = malloc(frame.can_dlc);
    memcpy(buf, frame.data, frame.can_dlc);
    *size = frame.can_dlc;
    return buf;
}

void can_destroy(void* self) {
    CanTransport* ct = (CanTransport*)self;
    close(ct->sock);
    free(self);
}

ITransport iface = {can_setup, can_send, can_receive, can_destroy};

void* create_plugin() { return calloc(1, sizeof(CanTransport)); }

const char* get_plugin_version() { return "1.0"; }
