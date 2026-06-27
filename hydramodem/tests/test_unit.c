/* tests/test_unit.c -- per-module unit tests (known-answer + roundtrip).
 *
 * Covers the pieces the end-to-end loopback can't isolate: CRC against a
 * standard vector, the convolutional codec's error correction, interleaver
 * bijection, bit/byte/symbol packing inverses, frame build<->soft-decode, and
 * WAV I/O. Returns non-zero if any check fails; run under ASan/UBSan for memory
 * safety (see `make asan`).
 */
#include "../src/hydra_crc.h"
#include "../src/hydra_conv.h"
#include "../src/hydra_interleave.h"
#include "../src/hydra_fec.h"
#include "../src/hydra_frame.h"
#include "../src/hydra_profile.h"
#include "../src/wav.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static int g_fail = 0;
#define CHECK(cond, msg) do { \
    if (cond) { printf("  ok   %s\n", msg); } \
    else      { printf("  FAIL %s\n", msg); ++g_fail; } } while (0)

static uint64_t rng = 0xC0FFEEULL;
static unsigned ubit(void){ rng^=rng<<13; rng^=rng>>7; rng^=rng<<17; return (rng>>40)&1u; }
static unsigned ubyte(void){ rng^=rng<<13; rng^=rng>>7; rng^=rng<<17; return (rng>>33)&0xFFu; }

/* ------------------------------------------------------------------ CRC ---- */
static void test_crc(void)
{
    /* CRC-16/CCITT-FALSE("123456789") = 0x29B1 (standard check value) */
    const uint8_t v[] = "123456789";
    uint16_t c = hydra_crc16_ccitt(v, 9);
    printf("[CRC]\n");
    CHECK(c == 0x29B1u, "CCITT-FALSE check value 0x29B1");
    {
        uint8_t a[4] = {0xDE,0xAD,0xBE,0xEF};
        uint16_t c1 = hydra_crc16_ccitt(a, 4);
        a[2] ^= 0x01;                                 /* flip one bit */
        CHECK(hydra_crc16_ccitt(a, 4) != c1, "CRC changes on single-bit error");
    }
}

/* ------------------------------------------------------ convolutional code -- */
static void test_conv(void)
{
    const size_t N = 152;
    uint8_t msg[152], dec[152];
    size_t  nc = hydra_conv_coded_len(N), i;
    uint8_t *coded = malloc(nc);
    float   *soft  = malloc(nc * sizeof *soft);
    printf("[conv K=7 r=1/2]\n");
    CHECK(nc == 2u*(N+6u), "coded length = 2*(n+6)");

    for (i = 0; i < N; ++i) msg[i] = (uint8_t)ubit();
    hydra_conv_encode(msg, N, coded);
    for (i = 0; i < nc; ++i) soft[i] = coded[i] ? 1.0f : -1.0f;
    CHECK(hydra_conv_decode_soft(soft, nc, N, dec) == N &&
          memcmp(msg, dec, N) == 0, "noiseless soft roundtrip exact");

    /* inject a few hard bit flips; the code must correct them */
    {
        int flips[] = {3, 40, 41, 100, 211}; size_t k;
        for (k = 0; k < sizeof flips/sizeof*flips; ++k)
            soft[flips[k]] = -soft[flips[k]];
        CHECK(hydra_conv_decode_soft(soft, nc, N, dec) == N &&
              memcmp(msg, dec, N) == 0, "corrects 5 scattered bit errors");
    }
    free(coded); free(soft);
}

/* -------------------------------------------------------------- interleaver -- */
static void test_interleave(void)
{
    size_t n = 316; int st = hydra_interleave_stride(n);
    uint8_t *in = malloc(n), *seen = calloc(n,1);
    uint8_t *out = malloc(n); float *sf = malloc(n*sizeof*sf); float *de = malloc(n*sizeof*de);
    size_t i; int bij = 1, rt = 1;
    printf("[interleaver]\n");
    for (i = 0; i < n; ++i) { size_t k=(i*(size_t)st)%n; if (seen[k]) bij=0; seen[k]=1; }
    CHECK(bij, "permutation is a bijection");
    for (i = 0; i < n; ++i) in[i] = (uint8_t)ubit();
    hydra_interleave_bits(in, out, n, st);
    for (i = 0; i < n; ++i) sf[i] = out[i] ? 1.0f : -1.0f;
    hydra_deinterleave_soft(sf, de, n, st);
    for (i = 0; i < n; ++i) if ((de[i] > 0) != (in[i] != 0)) rt = 0;
    CHECK(rt, "interleave -> soft deinterleave restores order");
    free(in); free(seen); free(out); free(sf); free(de);
}

/* ----------------------------------------------------------------- packing -- */
static void test_packing(void)
{
    uint8_t bytes[19], back[19], bits[152];
    size_t i; int ok = 1;
    printf("[bit/byte/symbol packing]\n");
    for (i = 0; i < 19; ++i) bytes[i] = (uint8_t)ubyte();
    hydra_bytes_to_bits(bytes, 19, bits);
    hydra_bits_to_bytes(bits, 152, back);
    CHECK(memcmp(bytes, back, 19) == 0, "bytes -> bits -> bytes");

    /* symbols, 2 bits/symbol (4-FSK style) */
    {
        uint8_t syms[76], bits2[152];
        size_t ns = hydra_bits_to_symbols(bits, 152, 2, syms);
        hydra_symbols_to_bits(syms, ns, 2, bits2);
        for (i = 0; i < 152; ++i) if (bits[i] != bits2[i]) ok = 0;
        CHECK(ns == 76 && ok, "bits -> 2b symbols -> bits");
    }
}

/* --------------------------------------------------- frame build/decode ---- */
static void test_frame(hydra_fec_mode mode, int interleave, const char *name)
{
    hydra_profile p;
    uint8_t payload[HYDRA_DCF_BYTES], out[HYDRA_DCF_BYTES];
    uint8_t *syms, *cbits; float *soft;
    size_t nsym = 0, i;
    hydra_profile_default(&p); p.fec_mode = mode; p.interleave = interleave;
    hydra_profile_init(&p);

    for (i = 0; i < HYDRA_DCF_BYTES; ++i) payload[i] = (uint8_t)ubyte();
    syms  = malloc(p.total_syms);
    cbits = malloc(p.data_syms * (size_t)p.bits_per_symbol);
    soft  = malloc(p.coded_bits * sizeof *soft);

    if (hydra_frame_build(&p, payload, syms, p.total_syms, &nsym) != 0) { ++g_fail; printf("  FAIL %s build\n", name); goto out; }

    /* data symbols -> coded bits -> ideal soft (TX order), then decode */
    hydra_symbols_to_bits(syms + p.preamble_syms + p.sync_syms, p.data_syms,
                          p.bits_per_symbol, cbits);
    for (i = 0; i < p.coded_bits; ++i) soft[i] = cbits[i] ? 1.0f : -1.0f;
    CHECK(hydra_frame_decode_soft(&p, soft, out) == 0 &&
          memcmp(payload, out, HYDRA_DCF_BYTES) == 0, name);
out:
    free(syms); free(cbits); free(soft);
}

/* --------------------------------------------------------------------- WAV -- */
static void test_wav(void)
{
    const char *path = "/tmp/hydra_unit_test.wav";
    size_t n = 4096, i; int sr = 0;
    float *w = malloc(n*sizeof*w), *r = NULL; size_t rn = 0;
    int ok = 1;
    printf("[WAV I/O]\n");
    for (i = 0; i < n; ++i) w[i] = (float)sin(2.0*M_PI*440.0*(double)i/48000.0) * 0.5f;
    CHECK(hydra_wav_write(path, w, n, 48000) == 0, "write 16-bit WAV");
    CHECK(hydra_wav_read(path, &r, &rn, &sr) == 0 && rn == n && sr == 48000, "read back, dims match");
    if (r) {
        for (i = 0; i < n; ++i) if (fabsf(w[i]-r[i]) > 1.0f/32000.0f) ok = 0;
        CHECK(ok, "samples match within 16-bit quantization");
        free(r);
    }
    remove(path);
    free(w);
}

int main(void)
{
    printf("HydraModem unit tests\n\n");
    test_crc();
    test_conv();
    test_interleave();
    test_packing();
    printf("[frame build/decode roundtrip]\n");
    test_frame(HYDRA_FEC_NONE, 0, "none");
    test_frame(HYDRA_FEC_REP3, 0, "rep3");
    test_frame(HYDRA_FEC_CONV, 0, "conv");
    test_frame(HYDRA_FEC_CONV, 1, "conv+interleave");
    test_wav();
    printf("\n%s (%d failure%s)\n", g_fail ? "UNIT TESTS FAILED" : "ALL UNIT TESTS PASSED",
           g_fail, g_fail == 1 ? "" : "s");
    return g_fail ? 1 : 0;
}
