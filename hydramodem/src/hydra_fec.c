/* hydra_fec.c -- repetition-3 FEC. */
#include "hydra_fec.h"

size_t hydra_fec_rep3_encode(const uint8_t *in_bits, size_t n, uint8_t *out_bits)
{
    size_t i;
    for (i = 0; i < n; ++i) {
        uint8_t b = in_bits[i] ? 1u : 0u;
        out_bits[3*i + 0] = b;
        out_bits[3*i + 1] = b;
        out_bits[3*i + 2] = b;
    }
    return 3u * n;
}

size_t hydra_fec_rep3_decode(const uint8_t *coded_bits, size_t n_coded, uint8_t *out_bits)
{
    size_t n = n_coded / 3u;
    size_t i;
    for (i = 0; i < n; ++i) {
        int s = (coded_bits[3*i+0] ? 1 : 0)
              + (coded_bits[3*i+1] ? 1 : 0)
              + (coded_bits[3*i+2] ? 1 : 0);
        out_bits[i] = (s >= 2) ? 1u : 0u;   /* majority of 3 */
    }
    return n;
}
