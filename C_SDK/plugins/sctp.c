#include <dcf_sdk/dcf_plugin_manager.h>
#include <netinet/sctp.h>

typedef struct {
    int sctp_sock;
    struct sctp_status status;
} SctpTransport;

bool sctp_setup(void* self, const char* host, int port) {
    SctpTransport* st = (SctpTransport*)self;
    st->sctp_sock = socket(AF_INET, SOCK_STREAM, IPPROTO_SCTP);
    if (st->sctp_sock < 0) return false;
    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    inet_pton(AF_INET, host, &addr.sin_addr);
    if (connect(st->sctp_sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        close(st->sctp_sock);
        return false;
    }
    return true;
}

bool sctp_send(void* self, const uint8_t* data, size_t size, const char* target) {
    SctpTransport* st = (SctpTransport*)self;
    ssize_t sent = sctp_sendmsg(st->sctp_sock, data, size, NULL, 0, 0, 0, 0, 0, 0);
    if (sent < 0) return false;
    socklen_t len = sizeof(st->status);
    getsockopt(st->sctp_sock, IPPROTO_SCTP, SCTP_STATUS, &st->status, &len);
    return true;
}

uint8_t* sctp_receive(void* self, size_t* size) {
    SctpTransport* st = (SctpTransport*)self;
    uint8_t* buf = malloc(1024);
    struct sockaddr_in from;
    socklen_t fromlen = sizeof(from);
    int flags = 0;
    struct sctp_sndrcvinfo sinfo;
    *size = sctp_recvmsg(st->sctp_sock, buf, 1024, (struct sockaddr*)&from, &fromlen, &sinfo, &flags);
    if (*size < 0) { free(buf); return NULL; }
    return buf;
}

void sctp_destroy(void* self) {
    SctpTransport* st = (SctpTransport*)self;
    close(st->sctp_sock);
    free(self);
}

ITransport iface = {sctp_setup, sctp_send, sctp_receive, sctp_destroy};

void* create_plugin() { return calloc(1, sizeof(SctpTransport)); }

const char* get_plugin_version() { return "1.0"; }
