#include <dcf_sdk/dcf_plugin_manager.h>
#include <xbee.h>

typedef struct {
    struct xbee *xbee;
    struct xbee_con *con;
} ZigbeeTransport;

bool zigbee_setup(void* self, const char* device, int baud) {
    ZigbeeTransport* zt = (ZigbeeTransport*)self;
    xbee_setup(&zt->xbee, "xbeeZB", device, baud);
    struct xbee_conAddress addr = {0};
    xbee_conNew(zt->xbee, &zt->con, "Data", &addr);
    return true;
}

bool zigbee_send(void* self, const uint8_t* data, size_t size, const char* target) {
    ZigbeeTransport* zt = (ZigbeeTransport*)self;
    struct xbee_pkt *pkt;
    xbee_pktAlloc(&pkt, size);
    memcpy(pkt->data, data, size);
    pkt->dataLen = size;
    return xbee_conTx(zt->con, NULL, pkt) == XBEE_ENONE;
}

uint8_t* zigbee_receive(void* self, size_t* size) {
    ZigbeeTransport* zt = (ZigbeeTransport*)self;
    struct xbee_pkt *pkt;
    if (xbee_conRx(zt->con, &pkt, NULL) != XBEE_ENONE) return NULL;
    uint8_t* buf = malloc(pkt->dataLen);
    memcpy(buf, pkt->data, pkt->dataLen);
    *size = pkt->dataLen;
    xbee_pktFree(pkt);
    return buf;
}

void zigbee_destroy(void* self) {
    ZigbeeTransport* zt = (ZigbeeTransport*)self;
    xbee_conEnd(zt->con);
    xbee_shutdown(zt->xbee);
    free(self);
}

ITransport iface = {zigbee_setup, zigbee_send, zigbee_receive, zigbee_destroy};

void* create_plugin() { return calloc(1, sizeof(ZigbeeTransport)); }

const char* get_plugin_version() { return "1.0"; }
