// SPDX-License-Identifier: LGPL-3.0-only
/*
 * snake_l2.h — DCF-Snake raw-L2 Ethernet transport (cat5e), DeMoD LLC | LGPL-3.0
 *
 * A transport *beneath* the wire quantum (like UDP / HydraModem): it carries a batch of
 * opaque 17-byte DeModFrames over raw Ethernet (AF_PACKET, a custom EtherType per plane,
 * no IP/UDP), so the 246-vector wire certificate is untouched.  Frames are batched as
 * 32-byte SuperPacks (codec/demod_superpack.h) into one Ethernet payload to cut the
 * datagram/frame count — one lost Ethernet frame ≈ one lost QSS packet.
 *
 * The batch/unbatch codec is header-only and pure (unit-testable without a socket); the
 * AF_PACKET socket helpers live in snake_l2.c (Linux only).  See DCF_SNAKE_SPEC.md.
 *
 * Ethernet payload layout:  [n_frames u16 BE][ SuperPack * ceil(n/2) ]
 *   Each SuperPack packs two DeModFrames; an odd trailing frame is paired with a canonical
 *   zero DATA filler frame that the receiver discards using n_frames.
 */
#pragma once
#include "../../codec/demod_frame.h"
#include "../../codec/demod_superpack.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* Custom EtherTypes (IEEE local/experimental range; no clash with real AVB 0x22F0). */
#define DCF_SNAKE_ETHERTYPE_RECORD 0x88B5u   /* wire A: quanta record plane */
#define DCF_SNAKE_ETHERTYPE_CUE    0x88B6u   /* wire B: PCM cue plane        */

/* Batch-container overhead and the per-Ethernet-frame frame capacity for a given MTU. */
#define DCF_SNAKE_L2_HDR 2u                  /* the n_frames u16 */
static inline size_t dcf_snake_l2_capacity(size_t mtu) {
    if (mtu < DCF_SNAKE_L2_HDR) return 0;
    return ((mtu - DCF_SNAKE_L2_HDR) / DCF_SUPER_LEN) * 2u;   /* 2 frames per SuperPack */
}

/* The canonical zero filler frame (a valid DATA DeModFrame with all application fields 0). */
static inline void dcf__snake_l2_filler(uint8_t out[DCF_FRAME_SIZE]) {
    dcf_frame_t z;
    memset(&z, 0, sizeof z);
    z.version = 1u;
    z.type    = DCF_TYPE_DATA;
    dcf_frame_encode(&z, out);
}

/*
 * Batch `n` DeModFrames into one Ethernet payload.  Returns false if `out_cap` is too small.
 * On success *out_len holds the payload length (DCF_SNAKE_L2_HDR + ceil(n/2)*DCF_SUPER_LEN).
 */
static inline bool dcf_snake_l2_batch(const uint8_t (*frames)[DCF_FRAME_SIZE], size_t n,
                                      uint8_t *out, size_t out_cap, size_t *out_len) {
    if (n > 0xFFFFu) return false;
    size_t npairs = (n + 1u) / 2u;
    size_t need   = DCF_SNAKE_L2_HDR + npairs * DCF_SUPER_LEN;
    if (need > out_cap) return false;

    out[0] = (uint8_t)((n >> 8) & 0xFFu);
    out[1] = (uint8_t)(n & 0xFFu);

    uint8_t filler[DCF_FRAME_SIZE];
    dcf__snake_l2_filler(filler);

    size_t off = DCF_SNAKE_L2_HDR;
    for (size_t i = 0; i < n; i += 2) {
        const uint8_t *a = frames[i];
        const uint8_t *b = (i + 1 < n) ? frames[i + 1] : filler;
        if (!dcf_superpack_pack(a, b, out + off)) return false;
        off += DCF_SUPER_LEN;
    }
    *out_len = off;
    return true;
}

/*
 * Unbatch an Ethernet payload back into up to `max` DeModFrames.  Returns false on a short
 * buffer, an oversize count, or a SuperPack integrity failure.  *out_n holds the frame count.
 */
static inline bool dcf_snake_l2_unbatch(const uint8_t *buf, size_t len,
                                        uint8_t (*frames)[DCF_FRAME_SIZE], size_t max,
                                        size_t *out_n) {
    if (len < DCF_SNAKE_L2_HDR) return false;
    size_t n = ((size_t)buf[0] << 8) | buf[1];
    if (n > max) return false;
    size_t npairs = (n + 1u) / 2u;
    if (len < DCF_SNAKE_L2_HDR + npairs * DCF_SUPER_LEN) return false;

    size_t off = DCF_SNAKE_L2_HDR, got = 0;
    for (size_t p = 0; p < npairs; p++) {
        uint8_t a[DCF_FRAME_SIZE], b[DCF_FRAME_SIZE];
        if (!dcf_superpack_unpack(buf + off, a, b)) return false;
        off += DCF_SUPER_LEN;
        memcpy(frames[got++], a, DCF_FRAME_SIZE);
        if (got < n) memcpy(frames[got++], b, DCF_FRAME_SIZE);
    }
    *out_n = n;
    return true;
}

/* ── AF_PACKET socket helpers (Linux; defined in snake_l2.c) ─────────────────────────────
 * All return -1 / false on error with errno set; callers degrade gracefully on EPERM
 * (needs CAP_NET_RAW: `setcap cap_net_raw+ep <binary>`).                                  */
typedef struct {
    int      fd;
    int      ifindex;
    uint16_t ethertype;
    uint8_t  mac[6];        /* this interface's hardware address */
} dcf_snake_l2_sock_t;

/* Open an AF_PACKET/SOCK_DGRAM socket bound to `ifname`, filtered to `ethertype`. */
int  dcf_snake_l2_open(dcf_snake_l2_sock_t *s, const char *ifname, uint16_t ethertype);
/* Send one Ethernet payload to `dst_mac` (NULL => broadcast).  Returns bytes sent or -1. */
long dcf_snake_l2_send(const dcf_snake_l2_sock_t *s, const uint8_t dst_mac[6],
                       const uint8_t *payload, size_t len);
/* Receive one Ethernet payload (blocking).  Returns bytes received or -1. */
long dcf_snake_l2_recv(const dcf_snake_l2_sock_t *s, uint8_t *buf, size_t cap);
void dcf_snake_l2_close(dcf_snake_l2_sock_t *s);
