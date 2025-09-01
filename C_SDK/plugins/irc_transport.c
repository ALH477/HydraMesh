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
// Enhancements: Added robust error handling with checks and returns, logging using fprintf(stderr) with levels (configurable?).

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
#include <stdarg.h> // For logging

// Log levels: 0 debug, 1 info, 2 error
typedef enum { LOG_DEBUG = 0, LOG_INFO, LOG_ERROR } LogLevel;

// IRCTransport struct
typedef struct {
    int sock;
    char nick[37];  // UUID string length
    LogLevel log_level;  // Default to INFO
} IRCTransport;

// Logging function
static void irc_log(IRCTransport* self, LogLevel level, const char* fmt, ...) {
    if (level < self->log_level) return;
    const char* prefix = level == LOG_DEBUG ? "DEBUG" : level == LOG_INFO ? "INFO" : "ERROR";
    fprintf(stderr, "[%s] IRCTransport: ", prefix);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
}

// Simple base64 encoding/decoding (public domain snippet adapted)
static const char base64_chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static char* base64_encode(const uint8_t* data, size_t size) {
    size_t out_len = 4 * ((size + 2) / 3) + 1;
    char* encoded = malloc(out_len);
    if (!encoded) return NULL;
    size_t i = 0, j = 0;
    while (size > 0) {
        uint32_t octet_a = i < size ? *data++ : 0;
        uint32_t octet_b = i < size ? *data++ : 0;
        uint32_t octet_c = i < size ? *data++ : 0;
        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;
        encoded[j++] = base64_chars[(triple >> 3 * 6) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 2 * 6) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 1 * 6) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 0 * 6) & 0x3F];
        i += 3;
        size -= size > 3 ? 3 : size;
    }
    size_t pad = (3 - size % 3) % 3;
    for (size_t k = 0; k < pad; k++) encoded[j - 1 - k] = '=';
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
    while (len-- && (encoded[j] != '=') && (isalnum(encoded[j]) || encoded[j] == '+' || encoded[j] == '/')) {
        uint32_t sextet_a = encoded[j] == '=' ? 0 : (uint32_t)(strchr(base64_chars, encoded[j++]) - base64_chars);
        uint32_t sextet_b = encoded[j] == '=' ? 0 : (uint32_t)(strchr(base64_chars, encoded[j++]) - base64_chars);
        uint32_t sextet_c = encoded[j] == '=' ? 0 : (uint32_t)(strchr(base64_chars, encoded[j++]) - base64_chars);
        uint32_t sextet_d = encoded[j] == '=' ? 0 : (uint32_t)(strchr(base64_chars, encoded[j++]) - base64_chars);
        uint32_t triple = (sextet_a << 3 * 6) + (sextet_b << 2 * 6) + (sextet_c << 1 * 6) + (sextet_d << 0 * 6);
        if (i < *out_size) decoded[i++] = (triple >> 2 * 8) & 0xFF;
        if (i < *out_size) decoded[i++] = (triple >> 1 * 8) & 0xFF;
        if (i < *out_size) decoded[i++] = (triple >> 0 * 8) & 0xFF;
    }
    return decoded;
}

// Helper: Send raw IRC command
static bool irc_send(IRCTransport* self, const char* fmt, ...) {
    char buf[512];
    va_list args;
    va_start(args, fmt);
    int len = vsnprintf(buf, sizeof(buf) - 2, fmt, args);
    va_end(args);
    if (len < 0 || len >= (int)sizeof(buf) - 2) {
        irc_log(self, LOG_ERROR, "irc_send: Buffer overflow");
        return false;
    }
    strcat(buf, "\r\n");
    ssize_t sent = send(self->sock, buf, strlen(buf), 0);
    if (sent <= 0) {
        irc_log(self, LOG_ERROR, "irc_send: Send failed");
        return false;
    }
    return true;
}

// Helper: Read a line from socket (blocking, simple buffer)
static char* irc_read_line(IRCTransport* self) {
    static char buf[512];
    int pos = 0;
    while (pos < (int)sizeof(buf) - 1) {
        char c;
        ssize_t recvd = recv(self->sock, &c, 1, 0);
        if (recvd <= 0) {
            irc_log(self, LOG_ERROR, "irc_read_line: Recv failed");
            return NULL;
        }
        if (c == '\r') continue;
        if (c == '\n') break;
        buf[pos++] = c;
    }
    buf[pos] = '\0';
    return strlen(buf) > 0 ? buf : NULL;
}

bool irc_setup(void* instance, const char* host, int port) {
    IRCTransport* self = (IRCTransport*)instance;
    self->log_level = LOG_INFO;  // Default

    // Generate unique nick: DCF_<uuid>
    uuid_t uuid;
    uuid_generate_random(uuid);
    uuid_unparse_lower(uuid, self->nick);
    char full_nick[41];
    snprintf(full_nick, sizeof(full_nick), "DCF_%s", self->nick);

    // Create socket
    self->sock = socket(AF_INET, SOCK_STREAM, 0);
    if (self->sock < 0) {
        irc_log(self, LOG_ERROR, "Setup: Socket creation failed");
        return false;
    }

    // Connect to IRC server
    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    if (inet_pton(AF_INET, host, &addr.sin_addr) <= 0) {
        irc_log(self, LOG_ERROR, "Setup: Invalid address");
        close(self->sock);
        return false;
    }
    if (connect(self->sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        irc_log(self, LOG_ERROR, "Setup: Connect failed");
        close(self->sock);
        return false;
    }

    // Register with IRC server
    if (!irc_send(self, "NICK %s", full_nick)) {
        close(self->sock);
        return false;
    }
    if (!irc_send(self, "USER dcf 0 * :DCF Node")) {
        close(self->sock);
        return false;
    }

    // Wait for welcome (001) or error
    bool welcomed = false;
    while (!welcomed) {
        char* line = irc_read_line(self);
        if (!line) {
            irc_log(self, LOG_ERROR, "Setup: Read failed during registration");
            close(self->sock);
            return false;
        }
        irc_log(self, LOG_DEBUG, "Received: %s", line);
        if (strstr(line, "001")) {
            welcomed = true;
            irc_log(self, LOG_INFO, "IRC setup successful with nick %s", full_nick);
        } else if (strstr(line, "PING")) {
            char* ping_token = strchr(line, ':') ? strchr(line, ':') + 1 : "";
            if (!irc_send(self, "PONG :%s", ping_token)) {
                close(self->sock);
                return false;
            }
            irc_log(self, LOG_INFO, "Sent PONG to %s", ping_token);
        }
    }
    return true;
}

bool irc_send_msg(void* instance, const uint8_t* data, size_t size, const char* target) {
    IRCTransport* self = (IRCTransport*)instance;
    char* b64 = base64_encode(data, size);
    if (!b64) {
        irc_log(self, LOG_ERROR, "Send: Base64 encode failed");
        return false;
    }
    bool sent = irc_send(self, "PRIVMSG %s :DCF:%s", target, b64);
    free(b64);
    if (sent) {
        irc_log(self, LOG_INFO, "Sent message to %s", target);
    } else {
        irc_log(self, LOG_ERROR, "Send: Failed to send PRIVMSG");
    }
    return sent;
}

uint8_t* irc_receive(void* instance, size_t* size) {
    IRCTransport* self = (IRCTransport*)instance;
    *size = 0;
    while (true) {
        char* line = irc_read_line(self);
        if (!line) {
            irc_log(self, LOG_ERROR, "Receive: Read failed");
            return NULL;
        }
        irc_log(self, LOG_DEBUG, "Received: %s", line);

        // Handle PING
        if (strncmp(line, "PING :", 6) == 0) {
            char* token = line + 6;
            if (!irc_send(self, "PONG :%s", token)) {
                return NULL;
            }
            irc_log(self, LOG_INFO, "Sent PONG to %s", token);
            continue;
        }

        // Parse PRIVMSG
        char* msg_start = strstr(line, "PRIVMSG ");
        if (!msg_start) continue;
        char* content_start = strchr(msg_start, ':');
        if (!content_start || strncmp(content_start + 1, "DCF:", 4) != 0) continue;

        // Decode base64 after "DCF:"
        char* b64 = content_start + 5;
        uint8_t* decoded = base64_decode(b64, size);
        if (!decoded) {
            irc_log(self, LOG_ERROR, "Receive: Base64 decode failed");
            continue;
        }
        irc_log(self, LOG_INFO, "Received and decoded DCF message");
        return decoded;
    }
}

void irc_destroy(void* instance) {
    IRCTransport* self = (IRCTransport*)instance;
    if (self->sock >= 0) {
        irc_send(self, "QUIT :DCF Shutdown");
        close(self->sock);
        self->sock = -1;
        irc_log(self, LOG_INFO, "IRC connection closed");
    } else {
        irc_log(self, LOG_ERROR, "Destroy: No socket to close");
    }
    free(self);
}

ITransport iface = {irc_setup, irc_send_msg, irc_receive, irc_destroy};

void* create_plugin() {
    IRCTransport* self = calloc(1, sizeof(IRCTransport));
    if (!self) {
        fprintf(stderr, "[ERROR] IRCTransport: Failed to allocate memory for plugin\n");
        return NULL;
    }
    self->sock = -1;
    return self;
}

const char* get_plugin_version() {
    return "1.0.0";
}
