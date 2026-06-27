/* hydra_frame.c */
#include "hydra_frame.h"
#include "hydra_crc.h"
#include "hydra_fec.h"
#include "hydra_conv.h"
#include "hydra_interleave.h"
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ helpers */

void hydra_bytes_to_bits(const uint8_t *bytes, size_t nbytes, uint8_t *bits)
{
    size_t i; int b;
    for (i = 0; i < nbytes; ++i)
        for (b = 0; b < 8; ++b)
            bits[8*i + b] = (uint8_t)((bytes[i] >> (7 - b)) & 1u);
}

void hydra_bits_to_bytes(const uint8_t *bits, size_t nbits, uint8_t *bytes)
{
    size_t nbytes = nbits / 8u, i; int b;
    for (i = 0; i < nbytes; ++i) {
        uint8_t v = 0;
        for (b = 0; b < 8; ++b)
            v = (uint8_t)((v << 1) | (bits[8*i + b] & 1u));
        bytes[i] = v;
    }
}

size_t hydra_bits_to_symbols(const uint8_t *bits, size_t nbits,
                             int bits_per_symbol, uint8_t *symbols)
{
    size_t nsym = (nbits + (size_t)bits_per_symbol - 1u) / (size_t)bits_per_symbol;
    size_t s; int b;
    for (s = 0; s < nsym; ++s) {
        uint8_t v = 0;
        for (b = 0; b < bits_per_symbol; ++b) {
            size_t idx = s * (size_t)bits_per_symbol + (size_t)b;
            uint8_t bit = (idx < nbits) ? (bits[idx] & 1u) : 0u;  /* zero-pad */
            v = (uint8_t)((v << 1) | bit);
        }
        symbols[s] = v;
    }
    return nsym;
}

void hydra_symbols_to_bits(const uint8_t *symbols, size_t nsym,
                           int bits_per_symbol, uint8_t *bits)
{
    size_t s; int b;
    for (s = 0; s < nsym; ++s)
        for (b = 0; b < bits_per_symbol; ++b)
            bits[s*(size_t)bits_per_symbol + (size_t)b] =
                (uint8_t)((symbols[s] >> (bits_per_symbol - 1 - b)) & 1u);
}

void hydra_frame_sync_bits(const hydra_profile *p, uint8_t sync_bits[HYDRA_SYNC_BITS])
{
    uint8_t sb[2];
    sb[0] = (uint8_t)(p->sync_word >> 8);
    sb[1] = (uint8_t)(p->sync_word & 0xFFu);
    hydra_bytes_to_bits(sb, 2, sync_bits);
}

/* assemble payload(17) + CRC16(2) -> data_bits (152) */
static void build_data_bits(const uint8_t payload[HYDRA_DCF_BYTES], uint8_t *data_bits)
{
    uint8_t field[HYDRA_DCF_BYTES + HYDRA_CRC_BYTES];
    uint16_t crc = hydra_crc16_ccitt(payload, HYDRA_DCF_BYTES);
    memcpy(field, payload, HYDRA_DCF_BYTES);
    field[HYDRA_DCF_BYTES + 0] = (uint8_t)(crc >> 8);
    field[HYDRA_DCF_BYTES + 1] = (uint8_t)(crc & 0xFFu);
    hydra_bytes_to_bits(field, sizeof field, data_bits);
}

/* FEC encode data_bits -> coded_bits (mode-dependent length = p->coded_bits) */
static int fec_encode(const hydra_profile *p, const uint8_t *data_bits, uint8_t *coded)
{
    switch (p->fec_mode) {
        case HYDRA_FEC_NONE: memcpy(coded, data_bits, p->data_bits);            return 0;
        case HYDRA_FEC_REP3: hydra_fec_rep3_encode(data_bits, p->data_bits, coded); return 0;
        case HYDRA_FEC_CONV: hydra_conv_encode(data_bits, p->data_bits, coded);  return 0;
        default: return -1;
    }
}

/* ------------------------------------------------------------------- build  */

int hydra_frame_build(const hydra_profile *p, const uint8_t payload[HYDRA_DCF_BYTES],
                      uint8_t *symbols_out, size_t cap, size_t *nsym_out)
{
    uint8_t  data_bits[HYDRA_DCF_BITS + HYDRA_CRC_BITS];     /* 152 */
    uint8_t *coded = NULL, *coded_il = NULL;
    uint8_t  sync_bits[HYDRA_SYNC_BITS];
    size_t   w = 0, k;
    int      rc = -1;

    if (!p || !payload || !symbols_out || !nsym_out) return -1;
    if (cap < p->total_syms) return -1;

    coded = (uint8_t *)malloc(p->coded_bits);
    if (!coded) return -2;

    build_data_bits(payload, data_bits);
    if (fec_encode(p, data_bits, coded) != 0) { free(coded); return -1; }

    if (p->interleave) {
        coded_il = (uint8_t *)malloc(p->coded_bits);
        if (!coded_il) { free(coded); return -2; }
        hydra_interleave_bits(coded, coded_il, p->coded_bits, p->interleave_stride);
    } else {
        coded_il = coded;
    }

    /* 1) preamble: alternate tone 0 and tone (N-1) */
    for (k = 0; k < (size_t)p->preamble_syms; ++k)
        symbols_out[w++] = (k & 1u) ? (uint8_t)(p->n_tones - 1) : 0u;

    /* 2) sync word -> symbols */
    hydra_frame_sync_bits(p, sync_bits);
    w += hydra_bits_to_symbols(sync_bits, HYDRA_SYNC_BITS, p->bits_per_symbol,
                               symbols_out + w);

    /* 3) coded (+interleaved) data -> symbols */
    w += hydra_bits_to_symbols(coded_il, p->coded_bits, p->bits_per_symbol,
                               symbols_out + w);

    *nsym_out = w;
    rc = (w == p->total_syms) ? 0 : -2;

    if (coded_il != coded) free(coded_il);
    free(coded);
    return rc;
}

/* ------------------------------------------------------------- soft decode  */

int hydra_frame_decode_soft(const hydra_profile *p, const float *soft_coded,
                            uint8_t payload_out[HYDRA_DCF_BYTES])
{
    float   *soft_lin = NULL;
    uint8_t  data_bits[HYDRA_DCF_BITS + HYDRA_CRC_BITS];
    uint8_t  field[HYDRA_DCF_BYTES + HYDRA_CRC_BYTES];
    uint16_t crc_rx, crc_calc;
    const float *soft;
    int      rc = -1;

    if (!p || !soft_coded || !payload_out) return -1;

    /* deinterleave soft metrics back to code order */
    if (p->interleave) {
        soft_lin = (float *)malloc(p->coded_bits * sizeof *soft_lin);
        if (!soft_lin) return -2;
        hydra_deinterleave_soft(soft_coded, soft_lin, p->coded_bits, p->interleave_stride);
        soft = soft_lin;
    } else {
        soft = soft_coded;
    }

    switch (p->fec_mode) {
        case HYDRA_FEC_NONE: {
            size_t i;
            for (i = 0; i < p->data_bits; ++i) data_bits[i] = (soft[i] > 0.0f);
            rc = 0;
            break;
        }
        case HYDRA_FEC_REP3: {
            uint8_t *hard = (uint8_t *)malloc(p->coded_bits);
            if (!hard) { rc = -2; break; }
            { size_t i; for (i = 0; i < p->coded_bits; ++i) hard[i] = (soft[i] > 0.0f); }
            hydra_fec_rep3_decode(hard, p->coded_bits, data_bits);
            free(hard);
            rc = 0;
            break;
        }
        case HYDRA_FEC_CONV: {
            size_t got = hydra_conv_decode_soft(soft, p->coded_bits, p->data_bits, data_bits);
            rc = (got == p->data_bits) ? 0 : -1;
            break;
        }
        default: rc = -1;
    }

    free(soft_lin);
    if (rc != 0) return rc;

    hydra_bits_to_bytes(data_bits, p->data_bits, field);
    memcpy(payload_out, field, HYDRA_DCF_BYTES);
    crc_rx   = (uint16_t)((field[HYDRA_DCF_BYTES] << 8) | field[HYDRA_DCF_BYTES + 1]);
    crc_calc = hydra_crc16_ccitt(payload_out, HYDRA_DCF_BYTES);

    return (crc_rx == crc_calc) ? 0 : -3;   /* -3 == CRC mismatch */
}
