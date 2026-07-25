/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_pipe_certify.c — certifies the C DCF-Pipe control codec against the
 * generated vectors (codec/pipe_vectors.gen.h), so it is byte-identical to the
 * Python reference and the Rust port.
 *
 * Build: gcc -std=c11 -I codec C_SDK/tests/test_pipe_certify.c -o /tmp/pc && /tmp/pc
 */
#include <stdio.h>
#include <string.h>
#include "demod_pipe.h"
#include "pipe_vectors.gen.h"

static int fails = 0;
#define CHECK(cond, msg) do { if (cond) {} else { printf("FAIL: %s\n", msg); fails++; } } while (0)

int main(void) {
    uint8_t buf[64];

    for (int i = 0; i < PIPE_OPEN_N; i++) {
        const pipe_open_t *v = &PIPE_OPEN[i];
        /* re-derive fields from the golden bytes, then re-pack and compare */
        uint16_t sid, cs; uint32_t total, ck;
        CHECK(dcf_pipe_unpack_open(v->bytes, v->n, &sid, &total, &cs, &ck) == 0, "open unpack");
        size_t n = dcf_pipe_pack_open(buf, sid, total, cs, ck);
        CHECK(n == (size_t)v->n && memcmp(buf, v->bytes, n) == 0, "open pack bytes");
    }
    for (int i = 0; i < PIPE_CREDIT_N; i++) {
        const pipe_credit_t *v = &PIPE_CREDIT[i];
        uint16_t sid; uint32_t c;
        CHECK(dcf_pipe_unpack_credit(v->bytes, v->n, &sid, &c) == 0, "credit unpack");
        size_t n = dcf_pipe_pack_credit(buf, sid, c);
        CHECK(n == (size_t)v->n && memcmp(buf, v->bytes, n) == 0, "credit pack bytes");
    }
    for (int i = 0; i < PIPE_SACK_N; i++) {
        const pipe_sack_t *v = &PIPE_SACK[i];
        uint16_t sid; uint32_t base; const uint8_t *bm;
        int nb = dcf_pipe_unpack_sack(v->bytes, v->n, &sid, &base, &bm);
        CHECK(nb >= 0, "sack unpack");
        size_t n = dcf_pipe_pack_sack(buf, sid, base, bm, (uint8_t)nb);
        CHECK(n == (size_t)v->n && memcmp(buf, v->bytes, n) == 0, "sack pack bytes");
    }
    for (int i = 0; i < PIPE_NACK_N; i++) {
        const pipe_nack_t *v = &PIPE_NACK[i];
        uint16_t sid; uint32_t miss[64];
        int cnt = dcf_pipe_unpack_nack(v->bytes, v->n, &sid, miss, 64);
        CHECK(cnt >= 0, "nack unpack");
        size_t n = dcf_pipe_pack_nack(buf, sid, miss, (uint8_t)cnt);
        CHECK(n == (size_t)v->n && memcmp(buf, v->bytes, n) == 0, "nack pack bytes");
    }
    for (int i = 0; i < PIPE_DONE_N; i++) {
        const pipe_done_t *v = &PIPE_DONE[i];
        uint16_t sid;
        CHECK(dcf_pipe_unpack_done(v->bytes, v->n, &sid) == 0, "done unpack");
        size_t n = dcf_pipe_pack_done(buf, sid);
        CHECK(n == (size_t)v->n && memcmp(buf, v->bytes, n) == 0, "done pack bytes");
    }
    for (int i = 0; i < PIPE_ABORT_N; i++) {
        const pipe_abort_t *v = &PIPE_ABORT[i];
        uint16_t sid; uint8_t reason;
        CHECK(dcf_pipe_unpack_abort(v->bytes, v->n, &sid, &reason) == 0, "abort unpack");
        size_t n = dcf_pipe_pack_abort(buf, sid, reason);
        CHECK(n == (size_t)v->n && memcmp(buf, v->bytes, n) == 0, "abort pack bytes");
    }
    for (int i = 0; i < PIPE_CHUNK_N; i++) {
        const pipe_chunk_t *v = &PIPE_CHUNK[i];
        uint16_t sid; uint32_t seq; const uint8_t *pl; size_t pll;
        CHECK(dcf_pipe_unpack_chunk(v->bytes, v->n, &sid, &seq, &pl, &pll) == 0, "chunk unpack");
        size_t n = dcf_pipe_pack_chunk(buf, sid, seq, pl, pll);
        CHECK(n == (size_t)v->n && memcmp(buf, v->bytes, n) == 0, "chunk pack bytes");
    }
    for (int i = 0; i < PIPE_FNV_N; i++) {
        const pipe_fnv_t *v = &PIPE_FNV[i];
        CHECK(dcf_pipe_fnv1a32(v->data, (size_t)v->n) == v->checksum, "fnv checksum");
    }
    for (int i = 0; i < PIPE_COUNT_N; i++) {
        const pipe_count_t *v = &PIPE_COUNT[i];
        CHECK(dcf_pipe_num_chunks(v->total_len, v->chunk_size) == v->n, "num_chunks");
    }

    if (fails == 0)
        printf("ALL PIPE VECTORS HOLD — C DCF-Pipe control codec is cemented.\n");
    return fails ? 1 : 0;
}
