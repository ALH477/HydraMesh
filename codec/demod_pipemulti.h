// SPDX-License-Identifier: LGPL-3.0-only
/*
 * demod_pipemulti.h — DCF-Pipe Multi-Control: parallel control bus (C)
 * DeMoD LLC | LGPL-3.0
 *
 * Packs up to 3 steady-state DCF-Pipe commands into a single 4-byte DeModFrame
 * payload. Byte-aligned layout:
 *
 *   Byte 0: [magic:2 | count:2 | flags:4]  = 0xC0 | (count<<4) | flags
 *   Byte 1: cmd[0] = (local_idx<<6) | (opcode<<3) | (param_lsb<<2)  [bits 7-2, 1-0 pad]
 *   Byte 2: cmd[1]
 *   Byte 3: cmd[2]
 *   count 1..3; higher cmd bytes zero when count<3; opcode 111 rejected.
 *
 * Magic = 11b in bits 7-6 -> byte0 >= 0xC0 (clean discriminator from classic
 * Pipe control msgs, byte0 0..5, and audio CTRL descriptors, byte0 <= 124).
 *
 * Byte-certified across C/Rust/Python by Documentation/pipemulti_vectors.json.
 * The 246-vector wire certificate and pipe_vectors.json are untouched.
 */
#ifndef DCF_DEMOD_PIPEMULTI_H
#define DCF_DEMOD_PIPEMULTI_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define MC_VERSION          1u
#define MC_PAYLOAD_LEN       4u
#define MC_MAGIC_MASK        0xC0u
#define MC_MAGIC             0xC0u
#define MC_MAX_COUNT         3u

/* Opcodes */
enum {
    MC_OP_NOP            = 0,
    MC_OP_CREDIT_DELTA   = 1,
    MC_OP_ACK_CUMUL      = 2,
    MC_OP_ACK_SELECTIVE  = 3,
    MC_OP_NACK_ONE       = 4,
    MC_OP_DONE_HINT      = 5,
    MC_OP_ABORT_HINT     = 6,
    MC_OP_RESERVED       = 7,  /* must NOT be sent in v0.1; rejected on pack & unpack */
};

/* Error codes */
enum {
    MC_OK                  =  0,
    MC_ERR_BAD_MAGIC       = -1,
    MC_ERR_BAD_COUNT       = -2,
    MC_ERR_RESERVED_FLAGS  = -3,
    MC_ERR_RESERVED_OPCODE = -4,
    MC_ERR_BAD_PAD_BITS    = -5,
    MC_ERR_UNUSED_SLOT     = -6,
    MC_ERR_BAD_LOCAL_IDX   = -7,
    MC_ERR_BAD_PARAM       = -8,
};

typedef struct {
    uint8_t local_idx;
    uint8_t opcode;
    uint8_t param_lsb;
} mc_cmd_t;

/* ── Discriminators ────────────────────────────────────────────────────────── */
static inline bool mc_is_multicontrol(const uint8_t *buf, size_t len) {
    return len >= 1 && (buf[0] & MC_MAGIC_MASK) == MC_MAGIC;
}

static inline bool mc_is_classic_pipe(const uint8_t *buf, size_t len) {
    return len >= 1 && buf[0] <= 5;
}

/* ── Pack ──────────────────────────────────────────────────────────────────── */
/*
 * Pack `n_cmds` commands (1..3) into a 4-byte buffer.
 * Returns MC_OK on success, negative error code on failure.
 * `out` must be at least 4 bytes.
 */
static inline int mc_pack(uint8_t *out, const mc_cmd_t *cmds, size_t n_cmds, uint8_t flags) {
    if (out == NULL) return MC_ERR_BAD_COUNT;
    if (n_cmds == 0 || n_cmds > MC_MAX_COUNT) return MC_ERR_BAD_COUNT;
    if (flags != 0) return MC_ERR_RESERVED_FLAGS;
    memset(out, 0, MC_PAYLOAD_LEN);
    out[0] = MC_MAGIC | (uint8_t)((n_cmds & 0x03) << 4) | (flags & 0x0F);
    for (size_t i = 0; i < n_cmds; i++) {
        const mc_cmd_t *c = &cmds[i];
        if (c->local_idx > 3) return MC_ERR_BAD_LOCAL_IDX;
        if (c->opcode > 6) return MC_ERR_RESERVED_OPCODE;
        if (c->param_lsb > 1) return MC_ERR_BAD_PARAM;
        out[1 + i] = (uint8_t)((c->local_idx << 6) | (c->opcode << 3) | (c->param_lsb << 2));
    }
    return MC_OK;
}

/* ── Unpack ────────────────────────────────────────────────────────────────── */
/*
 * Unpack a 4-byte buffer into count + flags + cmds.
 * Returns MC_OK on success (fills *out_count and out_cmds[0..*out_count-1]),
 * negative error code on failure.
 * `buf` must be at least 4 bytes.
 */
static inline int mc_unpack(const uint8_t *buf, size_t len, uint8_t *out_count,
                            uint8_t *out_flags, mc_cmd_t *out_cmds) {
    if (buf == NULL || len < MC_PAYLOAD_LEN) return MC_ERR_BAD_COUNT;
    uint8_t b0 = buf[0];
    if ((b0 & MC_MAGIC_MASK) != MC_MAGIC) return MC_ERR_BAD_MAGIC;
    uint8_t count = (b0 >> 4) & 0x03;
    uint8_t flags = b0 & 0x0F;
    if (count == 0 || count > MC_MAX_COUNT) return MC_ERR_BAD_COUNT;
    if (flags != 0) return MC_ERR_RESERVED_FLAGS;
    for (uint8_t i = 0; i < count; i++) {
        uint8_t cb = buf[1 + i];
        uint8_t opcode = (cb >> 3) & 0x07;
        if (opcode == MC_OP_RESERVED) return MC_ERR_RESERVED_OPCODE;
        if (cb & 0x03) return MC_ERR_BAD_PAD_BITS;
        out_cmds[i].local_idx = (cb >> 6) & 0x03;
        out_cmds[i].opcode = opcode;
        out_cmds[i].param_lsb = (cb >> 2) & 0x01;
    }
    /* unused slots must be zero */
    for (uint8_t i = count; i < MC_MAX_COUNT; i++) {
        if (buf[1 + i] != 0) return MC_ERR_UNUSED_SLOT;
    }
    if (out_count) *out_count = count;
    if (out_flags) *out_flags = flags;
    return MC_OK;
}

#endif /* DCF_DEMOD_PIPEMULTI_H */