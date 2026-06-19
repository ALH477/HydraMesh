/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * dcfnode — a real DCF mesh node in C, the analogue of the Rust `dcf` and Go
 * `dcfnode` binaries. It carries the certified 17-byte DeModFrame quantum over a
 * UDP transport using the binary ProtoMessage envelope (dcf_proto.h), so it
 * meshes with the Go/Rust nodes; the certified DCF-Text / DCF-Game adapters
 * (codec/demod_text.h, demod_game.h) fragment messages into frames.
 *
 *   dcfnode version
 *   dcfnode start        [--bind 0.0.0.0:7777]
 *   dcfnode send-text     --peer host:port --channel NAME --text "hi"
 *   dcfnode send-game     --peer host:port --channel-id N --hex DEADBEEF [--type 2]
 *   dcfnode send-position --peer host:port --x 1 --y 2 --z 3
 *
 * Build: gcc -std=c11 -I codec -I C_SDK/node dcfnode.c -o dcfnode  (POSIX sockets)
 */
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "demod_frame.h"
#include "demod_game.h"
#include "demod_text.h"
#include "dcf_proto.h"

#define DCF_NODE_DEFAULT_PORT 7777

static uint64_t now_us(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)tv.tv_sec * 1000000ull + (uint64_t)tv.tv_usec;
}

static int parse_hostport(const char *s, char *host, size_t hostlen, int *port) {
    const char *colon = strrchr(s, ':');
    if (!colon) return -1;
    size_t hl = (size_t)(colon - s);
    if (hl == 0 || hl >= hostlen) return -1;
    memcpy(host, s, hl);
    host[hl] = '\0';
    *port = atoi(colon + 1);
    return (*port > 0) ? 0 : -1;
}

static int udp_socket_bound(const char *host, int port) {
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) return -1;
    struct sockaddr_in a;
    memset(&a, 0, sizeof(a));
    a.sin_family = AF_INET;
    a.sin_port = htons((uint16_t)port);
    a.sin_addr.s_addr = (host && *host) ? inet_addr(host) : INADDR_ANY;
    if (bind(fd, (struct sockaddr *)&a, sizeof(a)) < 0) { close(fd); return -1; }
    return fd;
}

/* Send one ProtoMessage(msg_type, payload) to host:port from an ephemeral socket. */
static int send_proto(const char *host, int port, uint8_t msg_type,
                      const uint8_t *payload, uint32_t plen) {
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) return -1;
    struct sockaddr_in dst;
    memset(&dst, 0, sizeof(dst));
    dst.sin_family = AF_INET;
    dst.sin_port = htons((uint16_t)port);
    if (inet_pton(AF_INET, host, &dst.sin_addr) != 1) { close(fd); return -1; }
    uint8_t buf[DCF_PROTO_HEADER_LEN + 4096];
    size_t n = dcf_proto_serialize(msg_type, 0, now_us(), payload, plen, buf);
    ssize_t sent = sendto(fd, buf, n, 0, (struct sockaddr *)&dst, sizeof(dst));
    close(fd);
    return (sent == (ssize_t)n) ? 0 : -1;
}

/* Ship each DeModFrame of a packetized message as its own ProtoMessage. */
static int send_frames(const char *host, int port, uint8_t msg_type,
                       uint8_t frames[][DCF_FRAME_SIZE], size_t n) {
    for (size_t i = 0; i < n; i++)
        if (send_proto(host, port, msg_type, frames[i], DCF_FRAME_SIZE) != 0) return -1;
    return 0;
}

static int cmd_version(void) {
    printf("dcfnode 0.3.0 — DCF C SDK node (17-byte DeModFrame, ProtoMessage/UDP)\n");
    printf("  CRC anchors: CRC(\"123456789\")=0x%04X  CRC(0^15)=0x%04X\n",
           dcf_crc16((const uint8_t *)"123456789", 9), dcf_crc16((const uint8_t[15]){0}, 15));
    return 0;
}

static int cmd_start(int argc, char **argv) {
    char host[64] = "0.0.0.0";
    int port = DCF_NODE_DEFAULT_PORT;
    for (int i = 0; i < argc; i++)
        if (!strcmp(argv[i], "--bind") && i + 1 < argc) parse_hostport(argv[++i], host, sizeof host, &port);

    int fd = udp_socket_bound(host, port);
    if (fd < 0) { fprintf(stderr, "bind %s:%d failed\n", host, port); return 1; }

    dcf_text_reasm_t tr; dcf_text_reasm_init(&tr);
    dcf_game_reasm_t gr; dcf_game_reasm_init(&gr);
    fprintf(stderr, "dcfnode listening on %s:%d (Ctrl-C to stop)\n", host, port);

    for (;;) {
        uint8_t dg[2048];
        struct sockaddr_in from;
        socklen_t fl = sizeof(from);
        ssize_t r = recvfrom(fd, dg, sizeof(dg), 0, (struct sockaddr *)&from, &fl);
        if (r < 0) break;
        char fip[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &from.sin_addr, fip, sizeof fip);

        uint8_t mt; uint32_t seq, plen; uint64_t ts; const uint8_t *p;
        if (!dcf_proto_deserialize(dg, (size_t)r, &mt, &seq, &ts, &p, &plen)) continue;

        if (mt == DCF_MSG_TEXT_DCF && plen == DCF_FRAME_SIZE) {
            dcf_text_packet_t msg;
            if (dcf_text_reasm_push(&tr, p, &msg) == DCF_TEXT_REASM_MESSAGE)
                printf("text from %s on ch 0x%04X: %.*s\n", fip, msg.dst, msg.payload_len, msg.payload);
        } else if (mt == DCF_MSG_GAME_DCF && plen == DCF_FRAME_SIZE) {
            dcf_game_packet_t pkt;
            if (dcf_game_reasm_push(&gr, p, &pkt) == DCF_GAME_REASM_PACKET)
                printf("game from %s: type=%u %u bytes (packet %u)\n", fip, pkt.msg_type_id, pkt.payload_len, pkt.packet_id);
        } else if (mt == DCF_MSG_POSITION && plen == 12) {
            uint32_t xi = ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | p[3];
            uint32_t yi = ((uint32_t)p[4] << 24) | ((uint32_t)p[5] << 16) | ((uint32_t)p[6] << 8) | p[7];
            uint32_t zi = ((uint32_t)p[8] << 24) | ((uint32_t)p[9] << 16) | ((uint32_t)p[10] << 8) | p[11];
            float x, y, z;
            memcpy(&x, &xi, 4); memcpy(&y, &yi, 4); memcpy(&z, &zi, 4);
            printf("position from %s: (%.2f, %.2f, %.2f)\n", fip, x, y, z);
        }
        fflush(stdout);
    }
    close(fd);
    return 0;
}

static const char *opt(int argc, char **argv, const char *key, const char *def) {
    for (int i = 0; i < argc; i++)
        if (!strcmp(argv[i], key) && i + 1 < argc) return argv[i + 1];
    return def;
}

static size_t unhex(const char *s, uint8_t *out, size_t max) {
    size_t n = 0;
    for (; s[0] && s[1] && n < max; s += 2) {
        unsigned v;
        if (sscanf(s, "%2x", &v) != 1) break;
        out[n++] = (uint8_t)v;
    }
    return n;
}

static int cmd_send_text(int argc, char **argv) {
    const char *peer = opt(argc, argv, "--peer", NULL);
    const char *chan = opt(argc, argv, "--channel", "lobby");
    const char *text = opt(argc, argv, "--text", "hello over DeModFrame");
    if (!peer) { fprintf(stderr, "--peer host:port required\n"); return 2; }
    char host[64]; int port;
    if (parse_hostport(peer, host, sizeof host, &port) != 0) { fprintf(stderr, "bad --peer\n"); return 2; }

    uint16_t dst = dcf_text_channel_id(chan);
    uint8_t frames[DCF_TEXT_MAX_FRAMES][DCF_FRAME_SIZE];
    size_t nf = 0;
    if (!dcf_text_packetize((const uint8_t *)text, strlen(text), 1, (uint32_t)now_us(),
                            1, dst, 0, frames, DCF_TEXT_MAX_FRAMES, &nf)) {
        fprintf(stderr, "packetize failed\n"); return 1;
    }
    if (send_frames(host, port, DCF_MSG_TEXT_DCF, frames, nf) != 0) { fprintf(stderr, "send failed\n"); return 1; }
    printf("sent %zu text frame(s) on ch 0x%04X to %s\n", nf, dst, peer);
    return 0;
}

static int cmd_send_game(int argc, char **argv) {
    const char *peer = opt(argc, argv, "--peer", NULL);
    uint8_t type = (uint8_t)atoi(opt(argc, argv, "--type", "2"));
    const char *hx = opt(argc, argv, "--hex", "deadbeef");
    uint16_t chan = (uint16_t)strtol(opt(argc, argv, "--channel-id", "1"), NULL, 0);
    if (!peer) { fprintf(stderr, "--peer host:port required\n"); return 2; }
    char host[64]; int port;
    if (parse_hostport(peer, host, sizeof host, &port) != 0) { fprintf(stderr, "bad --peer\n"); return 2; }

    uint8_t body[DCF_GAME_MAX_PAYLOAD];
    size_t blen = unhex(hx, body, sizeof body);
    uint8_t frames[DCF_GAME_MAX_FRAMES][DCF_FRAME_SIZE];
    size_t nf = 0;
    if (!dcf_game_packetize(type, body, blen, 1, (uint32_t)now_us(), 1, chan, 0,
                            frames, DCF_GAME_MAX_FRAMES, &nf)) {
        fprintf(stderr, "packetize failed\n"); return 1;
    }
    if (send_frames(host, port, DCF_MSG_GAME_DCF, frames, nf) != 0) { fprintf(stderr, "send failed\n"); return 1; }
    printf("sent %zu game frame(s) (type %u) to %s\n", nf, type, peer);
    return 0;
}

static void put_f32_be(uint8_t *out, float v) {
    uint32_t u;
    memcpy(&u, &v, 4);
    out[0] = (uint8_t)(u >> 24); out[1] = (uint8_t)(u >> 16);
    out[2] = (uint8_t)(u >> 8);  out[3] = (uint8_t)u;
}

static int cmd_send_position(int argc, char **argv) {
    const char *peer = opt(argc, argv, "--peer", NULL);
    if (!peer) { fprintf(stderr, "--peer host:port required\n"); return 2; }
    char host[64]; int port;
    if (parse_hostport(peer, host, sizeof host, &port) != 0) { fprintf(stderr, "bad --peer\n"); return 2; }
    float x = (float)atof(opt(argc, argv, "--x", "0"));
    float y = (float)atof(opt(argc, argv, "--y", "0"));
    float z = (float)atof(opt(argc, argv, "--z", "0"));
    uint8_t payload[12];
    put_f32_be(payload, x); put_f32_be(payload + 4, y); put_f32_be(payload + 8, z);
    if (send_proto(host, port, DCF_MSG_POSITION, payload, 12) != 0) { fprintf(stderr, "send failed\n"); return 1; }
    printf("sent position (%.2f, %.2f, %.2f) to %s\n", x, y, z, peer);
    return 0;
}

static void usage(void) {
    fprintf(stderr,
        "dcfnode — DCF C SDK node\n\n"
        "  version\n"
        "  start         [--bind host:port]\n"
        "  send-text     --peer host:port --channel NAME --text S\n"
        "  send-game     --peer host:port --channel-id N --hex BYTES [--type T]\n"
        "  send-position --peer host:port --x X --y Y --z Z\n");
}

int main(int argc, char **argv) {
    if (argc < 2) { usage(); return 2; }
    const char *cmd = argv[1];
    int rest = argc - 2;
    char **ra = argv + 2;
    if (!strcmp(cmd, "version") || !strcmp(cmd, "-v") || !strcmp(cmd, "--version")) return cmd_version();
    if (!strcmp(cmd, "start")) return cmd_start(rest, ra);
    if (!strcmp(cmd, "send-text")) return cmd_send_text(rest, ra);
    if (!strcmp(cmd, "send-game")) return cmd_send_game(rest, ra);
    if (!strcmp(cmd, "send-position")) return cmd_send_position(rest, ra);
    usage();
    return 2;
}
