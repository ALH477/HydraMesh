#include <dcf_sdk/dcf_plugin_manager.h>
#include <msquic.h>

typedef struct {
    HQUIC quic_conn;
    int last_rtt_ms;
} QuicTransport;

bool quic_setup(void* self, const char* host, int port) {
    QuicTransport* qt = (QuicTransport*)self;
    MSQUIC_STATUS status = MsQuicOpen2(&qt->quic_conn);
    if (QUIC_FAILED(status)) return false;
    QUIC_SETTINGS settings = {0};
    settings.Disable1RttEncryption = TRUE;
    MsQuicSetParam(qt->quic_conn, QUIC_PARAM_CONN_SETTINGS, sizeof(settings), &settings);
    QUIC_ADDR addr = {0};
    QuicAddrSetFamily(&addr, QUIC_ADDRESS_FAMILY_UNSPEC);
    QuicAddrSetPort(&addr, port);
    status = MsQuicConnectionOpen(qt->quic_conn, NULL, NULL, &qt->quic_conn);
    if (QUIC_FAILED(status)) return false;
    status = MsQuicConnectionStart(qt->quic_conn, NULL, &addr, 0, host);
    return QUIC_SUCCEEDED(status);
}

bool quic_send(void* self, const uint8_t* data, size_t size, const char* target) {
    QuicTransport* qt = (QuicTransport*)self;
    HQUIC stream;
    MSQUIC_STATUS status = MsQuicStreamOpen(qt->quic_conn, QUIC_STREAM_OPEN_FLAG_NONE, NULL, NULL, &stream);
    if (QUIC_FAILED(status)) return false;
    status = MsQuicStreamStart(stream, QUIC_STREAM_START_FLAG_NONE);
    if (QUIC_FAILED(status)) { MsQuicStreamClose(stream); return false; }
    status = MsQuicStreamSend(stream, data, size, QUIC_SEND_FLAG_FIN, NULL);
    MsQuicStreamClose(stream);
    QUIC_STATISTICS stats;
    MsQuicConnectionStatistics(qt->quic_conn, &stats);
    qt->last_rtt_ms = stats.Rtt;
    return QUIC_SUCCEEDED(status);
}

uint8_t* quic_receive(void* self, size_t* size) {
    QuicTransport* qt = (QuicTransport*)self;
    uint8_t* buf = malloc(1024);
    *size = 0;  // Simplified; use event loop
    return buf;
}

void quic_destroy(void* self) {
    QuicTransport* qt = (QuicTransport*)self;
    if (qt->quic_conn) MsQuicConnectionClose(qt->quic_conn);
    free(self);
}

ITransport iface = {quic_setup, quic_send, quic_receive, quic_destroy};

void* create_plugin() { return calloc(1, sizeof(QuicTransport)); }

const char* get_plugin_version() { return "1.0"; }
