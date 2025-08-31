// c_sdk/plugins/irc_transport.c
// IRC Transport Plugin for DCF C SDK
// This plugin implements IRC as a custom transport. It connects to an IRC server specified by host:port,
// generates a unique nick using UUID, registers with the server, and handles send/receive of DCF messages.
// Messages are base64-encoded and prefixed with "DCF:" in PRIVMSG to the target nick (recipient).
// Receive blocks until a relevant message is received, handling PING/PONG in the loop.
// Assumptions:
// - All nodes connect to the same IRC server.
// - Peers/recipients are IRC nicks (configured as strings in config.json).
// - No channel joining; uses private messages.
// - Basic error handling; expands on custom_transport example.
// - Includes simple base64 encode/decode functions.
// Dependencies: Standard C libs + libuuid (already in C SDK deps).
// Build: Compile as shared lib, e.g., gcc -shared -fPIC -o libirc_transport.so irc_transport.c -luuid
// Usage: In config.json, "plugins": {"transport": "libirc_transport.so"}
// Limitations: Blocking receive; no advanced IRC features; minimal error recovery.

#include <dcf_sdk/dcf_plugin_manager.h>  // Assumes this defines ITransport
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <uuid/uuid.h>
#include <ctype.h>  // For base64 validation

// Simple base64 encoding/decoding (public domain snippet adapted)
static const char base64_chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static char* base64_encode(const uint8_t* data, size_t size) {
    size_t out_len = 4 * ((size + 2) / 3) + 1;
    char* encoded = malloc(out_len);
    if (!encoded) return NULL;
    size_t i = 0, j = 0;
    while (size--) {
        uint32_t octet_a = i < size ? *data++ : 0;
        uint32_t octet_b = i < size ? *data++ : 0;
        uint32_t octet_c = i < size ? *data++ : 0;
        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;
        encoded[j++] = base64_chars[(triple >> 3 * 6) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 2 * 6) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 1 * 6) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 0 * 6) & 0x3F];
        i += 3;
    }
    for (i = 0; i < (3 - size % 3) % 3; i++) encoded[j - 1 - i] = '=';
    encoded[j] = '\0';
    return encoded;
}

static uint8_t* base64_decode(const char* encoded, size_t* out_size) {
    size_t len = strlen(encoded);
    if (len % 4 != 0) return NULL;
    *out_size = len / 4 * 3;
    if (encoded[len - 1] == '=') (*out_size)--;
    if (encoded[len - 2] == '=') (*out_size)--;
    uint8_t* decoded = malloc(*out_size);
    if (!decoded) return NULL;
    size_t i = 0, j = 0;
    while (len-- && (encoded[j] != '=') && isalnum(encoded[j]) || encoded[j] == '+' || encoded[j] == '/') {
        uint32_t sextet_a = encoded[j] == '=' ? 0 : strchr(base64_chars, encoded[j++]) - base64_chars;
        uint32_t sextet_b = encoded[j] == '=' ? 0 : strchr(base64_chars, encoded[j++]) - base64_chars;
        uint32_t sextet_c = encoded[j] == '=' ? 0 : strchr(base64_chars, encoded[j++]) - base64_chars;
        uint32_t sextet_d = encoded[j] == '=' ? 0 : strchr(base64_chars, encoded[j++]) - base64_chars;
        uint32_t triple = (sextet_a << 3 * 6) + (sextet_b << 2 * 6) + (sextet_c << 1 * 6) + (sextet_d << 0 * 6);
        if (i < *out_size) decoded[i++] = (triple >> 2 * 8) & 0xFF;
        if (i < *out_size) decoded[i++] = (triple >> 1 * 8) & 0xFF;
        if (i < *out_size) decoded[i++] = (triple >> 0 * 8) & 0xFF;
    }
    return decoded;
}

// IRCTransport struct
typedef struct {
    int sock;
    char nick[37];  // UUID string length
} IRCTransport;

// Helper: Send raw IRC command
static bool irc_send(IRCTransport* self, const char* fmt, ...) {
    char buf[512];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    strcat(buf, "\r\n");
    return send(self->sock, buf, strlen(buf), 0) > 0;
}

// Helper: Read a line from socket (blocking, simple buffer)
static char* irc_read_line(IRCTransport* self) {
    static char buf[512];
    int pos = 0;
    while (pos < sizeof(buf) - 1) {
        char c;
        if (recv(self->sock, &c, 1, 0) <= 0) return NULL;
        if (c == '\r') continue;
        if (c == '\n') break;
        buf[pos++] = c;
    }
    buf[pos] = '\0';
    return buf;
}

bool irc_setup(void* instance, const char* host, int port) {
    IRCTransport* self = (IRCTransport*)instance;

    // Generate unique nick: DCF_<uuid>
    uuid_t uuid;
    uuid_generate_random(uuid);
    char uuid_str[37];
    uuid_unparse_lower(uuid, uuid_str);
    snprintf(self->nick, sizeof(self->nick), "DCF_%s", uuid_str);

    // Create socket
    self->sock = socket(AF_INET, SOCK_STREAM, 0);
    if (self->sock < 0) return false;

    // Connect to IRC server
    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    inet_pton(AF_INET, host, &addr.sin_addr);
    if (connect(self->sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        close(self->sock);
        return false;
    }

    // Register with IRC server
    irc_send(self, "NICK %s", self->nick);
    irc_send(self, "USER dcf 0 * :DCF Node");

    // Wait for welcome (001) or error
    bool welcomed = false;
    while (!welcomed) {
        char* line = irc_read_line(self);
        if (!line) {
            close(self->sock);
            return false;
        }
        if (strstr(line, "001")) welcomed = true;
        else if (strstr(line, "PING")) {
            char* ping_token = strchr(line, ':') + 1;
            irc_send(self, "PONG :%s", ping_token);
        }
    }
    return true;
}

bool irc_send(void* instance, const uint8_t* data, size_t size, const char* target) {
    IRCTransport* self = (IRCTransport*)instance;
    char* b64 = base64_encode(data, size);
    if (!b64) return false;
    bool sent = irc_send(self, "PRIVMSG %s :DCF:%s", target, b64);
    free(b64);
    return sent;
}

uint8_t* irc_receive(void* instance, size_t* size) {
    IRCTransport* self = (IRCTransport*)instance;
    *size = 0;
    while (true) {
        char* line = irc_read_line(self);
        if (!line) return NULL;

        // Handle PING
        if (strncmp(line, "PING :", 6) == 0) {
            char* token = line + 6;
            irc_send(self, "PONG :%s", token);
            continue;
        }

        // Parse PRIVMSG
        char* msg_start = strstr(line, "PRIVMSG ");
        if (!msg_start) continue;
        char* content_start = strchr(msg_start, ':');
        if (!content_start || strncmp(content_start + 1, "DCF:", 4) != 0) continue;

        // Decode base64 after "DCF:"
        char* b64 = content_start + 5;
        return base64_decode(b64, size);
    }
}

void irc_destroy(void* instance) {
    IRCTransport* self = (IRCTransport*)instance;
    if (self->sock >= 0) {
        irc_send(self, "QUIT :DCF Shutdown");
        close(self->sock);
    }
    free(self);
}

ITransport iface = {irc_setup, irc_send, irc_receive, irc_destroy};

void* create_plugin() {
    return calloc(1, sizeof(IRCTransport));
}

const char* get_plugin_version() {
    return "1.0.0";
}
