#include <dcf_sdk/dcf_plugin_manager.h>
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>
#include <bluetooth/rfcomm.h>
#include <sys/time.h>

typedef struct {
    int dd;
    int sock;
    struct sockaddr_rc local_addr;
    struct sockaddr_rc remote_addr;
    struct {
        char* address;
        int rtt_ms;
    }* peers;
    int peer_count;
    char* mode;
} BluetoothTransport;

bool bluetooth_setup(void* self, const char* dev, int unused) {
    BluetoothTransport* bt = (BluetoothTransport*)self;
    bt->dd = hci_open_dev(hci_get_route(NULL));
    if (bt->dd < 0) return false;
    bt->sock = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);
    bt->local_addr.rc_family = AF_BLUETOOTH;
    bt->local_addr.rc_bdaddr = BDADDR_ANY;
    bt->local_addr.rc_channel = 1;
    bind(bt->sock, (struct sockaddr*)&bt->local_addr, sizeof(bt->local_addr));
    listen(bt->sock, 5);
    bt->mode = strdup("p2p");
    bt->peers = calloc(10, sizeof(*bt->peers));
    bt->peer_count = 0;
    hci_le_set_scan_enable(bt->dd, 0x01, 0, 1000);
    return true;
}

static int measure_rtt(BluetoothTransport* bt, const char* target) {
    struct timeval start, end;
    gettimeofday(&start, NULL);
    uint8_t ping[] = {0x01};
    if (!bluetooth_send(bt, ping, sizeof(ping), target)) return -1;
    gettimeofday(&end, NULL);
    int rtt_ms = (end.tv_sec - start.tv_sec) * 1000 + (end.tv_usec - start.tv_usec) / 1000;
    for (int i = 0; i < bt->peer_count; i++) {
        if (strcmp(bt->peers[i].address, target) == 0) {
            bt->peers[i].rtt_ms = rtt_ms < 50 ? rtt_ms : 50;
            break;
        }
    }
    return rtt_ms;
}

bool bluetooth_send(void* self, const uint8_t* data, size_t size, const char* target) {
    BluetoothTransport* bt = (BluetoothTransport*)self;
    int client_sock = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);
    bt->remote_addr.rc_family = AF_BLUETOOTH;
    str2ba(target, &bt->remote_addr.rc_bdaddr);
    bt->remote_addr.rc_channel = 1;
    if (connect(client_sock, (struct sockaddr*)&bt->remote_addr, sizeof(bt->remote_addr)) < 0) {
        close(client_sock);
        for (int i = 0; i < bt->peer_count; i++) {
            if (bt->peers[i].rtt_ms < 50 && strcmp(bt->peers[i].address, target) != 0) {
                return bluetooth_send(bt, data, size, bt->peers[i].address);
            }
        }
        return false;
    }
    uint64_t len = htobe64(size);
    write(client_sock, &len, sizeof(len));
    ssize_t sent = write(client_sock, data, size);
    close(client_sock);
    for (int i = 0; i < bt->peer_count; i++) {
        if (strcmp(bt->peers[i].address, target) == 0) return sent == size;
    }
    bt->peers[bt->peer_count].address = strdup(target);
    bt->peers[bt->peer_count].rtt_ms = measure_rtt(bt, target);
    bt->peer_count++;
    return sent == size;
}

uint8_t* bluetooth_receive(void* self, size_t* size) {
    BluetoothTransport* bt = (BluetoothTransport*)self;
    int client_sock = accept(bt->sock, NULL, NULL);
    if (client_sock < 0) return NULL;
    uint64_t len;
    if (read(client_sock, &len, sizeof(len)) != sizeof(len)) {
        close(client_sock);
        return NULL;
    }
    len = be64toh(len);
    uint8_t* buf = malloc(len);
    *size = read(client_sock, buf, len);
    close(client_sock);
    if (*size != len) { free(buf); return NULL; }
    DCFMessage msg;
    if (dcf_parse_message(buf, *size, &msg) == DCF_SUCCESS && msg.sync && strcmp(bt->mode, "auto") == 0) {
        char* cmd = (char*)msg.data;
        if (strncmp(cmd, "set_role:", 9) == 0) {
            free(bt->mode);
            bt->mode = strdup(cmd + 9);
        }
    }
    return buf;
}

void bluetooth_destroy(void* self) {
    BluetoothTransport* bt = (BluetoothTransport*)self;
    for (int i = 0; i < bt->peer_count; i++) {
        free(bt->peers[i].address);
    }
    free(bt->peers);
    free(bt->mode);
    close(bt->sock);
    hci_close_dev(bt->dd);
    free(self);
}

ITransport iface = {bluetooth_setup, bluetooth_send, bluetooth_receive, bluetooth_destroy};

void* create_plugin() { return calloc(1, sizeof(BluetoothTransport)); }

const char* get_plugin_version() { return "1.0"; }
