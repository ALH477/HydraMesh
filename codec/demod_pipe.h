/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * demod_pipe.h — DCF-Pipe control-plane codec (C), byte-identical to
 * python/MCP/pipelab_core.py and codec/src/pipe.rs, pinned by
 * Documentation/pipe_vectors.json.
 *
 * DCF frames carry a small control vocabulary (OPEN/CREDIT/SACK/NACK/DONE/
 * ABORT) that steers a dumb UDP data lane of numbered chunks; the intelligence
 * (flow control, loss recovery, completion) is in these messages. Loss is
 * healed forward by DCF-FEC (demod_fec.h) and, past its budget, by NACK. The
 * transfer completes only when the whole-object FNV-1a checksum verifies.
 * Header-only; no allocation. All multi-byte fields big-endian. See
 * Documentation/DCF_PIPE_SPEC.md. The wire certificate is untouched.
 */
#ifndef DCF_DEMOD_PIPE_H
#define DCF_DEMOD_PIPE_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define DCF_PIPE_VERSION 1

enum {
    DCF_PIPE_OPEN = 0,
    DCF_PIPE_CREDIT = 1,
    DCF_PIPE_SACK = 2,
    DCF_PIPE_NACK = 3,
    DCF_PIPE_DONE = 4,
    DCF_PIPE_ABORT = 5
};

enum {
    DCF_PIPE_ABORT_CHECKSUM = 0,
    DCF_PIPE_ABORT_TIMEOUT = 1,
    DCF_PIPE_ABORT_POLICY = 2,
    DCF_PIPE_ABORT_PEER = 3
};

#define DCF_PIPE_CHUNK_HDR_LEN 6

/* FNV-1a 32-bit — the whole-object checksum carried in OPEN. */
static inline uint32_t dcf_pipe_fnv1a32(const uint8_t *data, size_t len) {
    uint32_t h = 0x811C9DC5u;
    for (size_t i = 0; i < len; i++) {
        h ^= data[i];
        h *= 0x01000193u;
    }
    return h;
}

static inline void dcf_pipe__be16(uint8_t *o, uint16_t v) {
    o[0] = (uint8_t)(v >> 8); o[1] = (uint8_t)v;
}
static inline void dcf_pipe__be32(uint8_t *o, uint32_t v) {
    o[0] = (uint8_t)(v >> 24); o[1] = (uint8_t)(v >> 16);
    o[2] = (uint8_t)(v >> 8);  o[3] = (uint8_t)v;
}
static inline uint16_t dcf_pipe__rd16(const uint8_t *b) {
    return (uint16_t)((b[0] << 8) | b[1]);
}
static inline uint32_t dcf_pipe__rd32(const uint8_t *b) {
    return ((uint32_t)b[0] << 24) | ((uint32_t)b[1] << 16) |
           ((uint32_t)b[2] << 8) | (uint32_t)b[3];
}

/* Message type of a control payload, or -1 if empty. */
static inline int dcf_pipe_msg_type(const uint8_t *buf, size_t len) {
    return len ? buf[0] : -1;
}

/* ── OPEN ──────────────────────────────────────────────────────────────── */
static inline size_t dcf_pipe_pack_open(uint8_t *out, uint16_t session_id,
                                        uint32_t total_len, uint16_t chunk_size,
                                        uint32_t checksum) {
    out[0] = DCF_PIPE_OPEN; out[1] = DCF_PIPE_VERSION;
    dcf_pipe__be16(out + 2, session_id);
    dcf_pipe__be32(out + 4, total_len);
    dcf_pipe__be16(out + 8, chunk_size);
    dcf_pipe__be32(out + 10, checksum);
    return 14;
}
static inline int dcf_pipe_unpack_open(const uint8_t *b, size_t len,
                                       uint16_t *session_id, uint32_t *total_len,
                                       uint16_t *chunk_size, uint32_t *checksum) {
    if (len < 14 || b[0] != DCF_PIPE_OPEN || b[1] != DCF_PIPE_VERSION) return -1;
    *session_id = dcf_pipe__rd16(b + 2);
    *total_len = dcf_pipe__rd32(b + 4);
    *chunk_size = dcf_pipe__rd16(b + 8);
    *checksum = dcf_pipe__rd32(b + 10);
    return 0;
}

/* ── CREDIT ────────────────────────────────────────────────────────────── */
static inline size_t dcf_pipe_pack_credit(uint8_t *out, uint16_t session_id, uint32_t credit) {
    out[0] = DCF_PIPE_CREDIT; out[1] = DCF_PIPE_VERSION;
    dcf_pipe__be16(out + 2, session_id);
    dcf_pipe__be32(out + 4, credit);
    return 8;
}
static inline int dcf_pipe_unpack_credit(const uint8_t *b, size_t len,
                                         uint16_t *session_id, uint32_t *credit) {
    if (len < 8 || b[0] != DCF_PIPE_CREDIT || b[1] != DCF_PIPE_VERSION) return -1;
    *session_id = dcf_pipe__rd16(b + 2);
    *credit = dcf_pipe__rd32(b + 4);
    return 0;
}

/* ── SACK ──────────────────────────────────────────────────────────────── */
static inline size_t dcf_pipe_pack_sack(uint8_t *out, uint16_t session_id, uint32_t base,
                                        const uint8_t *bitmap, uint8_t nbytes) {
    out[0] = DCF_PIPE_SACK; out[1] = DCF_PIPE_VERSION;
    dcf_pipe__be16(out + 2, session_id);
    dcf_pipe__be32(out + 4, base);
    out[8] = nbytes;
    if (nbytes) memcpy(out + 9, bitmap, nbytes);
    return (size_t)9 + nbytes;
}
/* Returns bitmap length, or -1. Sets session_id/base; points bitmap into b. */
static inline int dcf_pipe_unpack_sack(const uint8_t *b, size_t len, uint16_t *session_id,
                                       uint32_t *base, const uint8_t **bitmap) {
    if (len < 9 || b[0] != DCF_PIPE_SACK || b[1] != DCF_PIPE_VERSION) return -1;
    uint8_t nbytes = b[8];
    if (len < (size_t)9 + nbytes) return -1;
    *session_id = dcf_pipe__rd16(b + 2);
    *base = dcf_pipe__rd32(b + 4);
    *bitmap = b + 9;
    return nbytes;
}
static inline int dcf_pipe_sack_has(const uint8_t *bitmap, int nbytes, uint32_t base, uint32_t seq) {
    if (seq < base) return 1;
    uint32_t off = seq - base;
    uint32_t byte = off / 8, bit = off % 8;
    return byte < (uint32_t)nbytes && (bitmap[byte] & (1u << bit)) != 0;
}

/* ── NACK ──────────────────────────────────────────────────────────────── */
static inline size_t dcf_pipe_pack_nack(uint8_t *out, uint16_t session_id,
                                        const uint32_t *missing, uint8_t count) {
    out[0] = DCF_PIPE_NACK; out[1] = DCF_PIPE_VERSION;
    dcf_pipe__be16(out + 2, session_id);
    out[4] = count;
    for (uint8_t i = 0; i < count; i++) dcf_pipe__be32(out + 5 + 4 * i, missing[i]);
    return (size_t)5 + 4 * count;
}
/* Returns entry count, or -1. Writes up to `cap` entries into out_missing. */
static inline int dcf_pipe_unpack_nack(const uint8_t *b, size_t len, uint16_t *session_id,
                                       uint32_t *out_missing, int cap) {
    if (len < 5 || b[0] != DCF_PIPE_NACK || b[1] != DCF_PIPE_VERSION) return -1;
    int n = b[4];
    if (len < (size_t)5 + 4 * (size_t)n) return -1;
    *session_id = dcf_pipe__rd16(b + 2);
    for (int i = 0; i < n && i < cap; i++) out_missing[i] = dcf_pipe__rd32(b + 5 + 4 * i);
    return n;
}

/* ── DONE / ABORT ──────────────────────────────────────────────────────── */
static inline size_t dcf_pipe_pack_done(uint8_t *out, uint16_t session_id) {
    out[0] = DCF_PIPE_DONE; out[1] = DCF_PIPE_VERSION;
    dcf_pipe__be16(out + 2, session_id);
    return 4;
}
static inline int dcf_pipe_unpack_done(const uint8_t *b, size_t len, uint16_t *session_id) {
    if (len < 4 || b[0] != DCF_PIPE_DONE || b[1] != DCF_PIPE_VERSION) return -1;
    *session_id = dcf_pipe__rd16(b + 2);
    return 0;
}
static inline size_t dcf_pipe_pack_abort(uint8_t *out, uint16_t session_id, uint8_t reason) {
    out[0] = DCF_PIPE_ABORT; out[1] = DCF_PIPE_VERSION;
    dcf_pipe__be16(out + 2, session_id);
    out[4] = reason;
    return 5;
}
static inline int dcf_pipe_unpack_abort(const uint8_t *b, size_t len,
                                        uint16_t *session_id, uint8_t *reason) {
    if (len < 5 || b[0] != DCF_PIPE_ABORT || b[1] != DCF_PIPE_VERSION) return -1;
    *session_id = dcf_pipe__rd16(b + 2);
    *reason = b[4];
    return 0;
}

/* ── data-plane chunk header ───────────────────────────────────────────── */
static inline size_t dcf_pipe_pack_chunk(uint8_t *out, uint16_t session_id, uint32_t chunk_seq,
                                         const uint8_t *payload, size_t payload_len) {
    dcf_pipe__be16(out, session_id);
    dcf_pipe__be32(out + 2, chunk_seq);
    if (payload_len) memcpy(out + DCF_PIPE_CHUNK_HDR_LEN, payload, payload_len);
    return DCF_PIPE_CHUNK_HDR_LEN + payload_len;
}
static inline int dcf_pipe_unpack_chunk(const uint8_t *b, size_t len, uint16_t *session_id,
                                        uint32_t *chunk_seq, const uint8_t **payload,
                                        size_t *payload_len) {
    if (len < DCF_PIPE_CHUNK_HDR_LEN) return -1;
    *session_id = dcf_pipe__rd16(b);
    *chunk_seq = dcf_pipe__rd32(b + 2);
    *payload = b + DCF_PIPE_CHUNK_HDR_LEN;
    *payload_len = len - DCF_PIPE_CHUNK_HDR_LEN;
    return 0;
}

/* Number of chunks a `total_len`-byte object splits into. */
static inline uint32_t dcf_pipe_num_chunks(uint32_t total_len, uint16_t chunk_size) {
    if (chunk_size == 0) return 0;
    return (total_len + chunk_size - 1u) / chunk_size;
}

#endif /* DCF_DEMOD_PIPE_H */
