/* hydra_interleave.c */
#include "hydra_interleave.h"

static size_t gcd_sz(size_t a, size_t b)
{
    while (b) { size_t t = a % b; a = b; b = t; }
    return a;
}

int hydra_interleave_stride(size_t n)
{
    size_t s;
    if (n < 3) return 1;
    /* start near sqrt(n), step up to the next value coprime with n */
    s = 1; while (s * s < n) ++s;          /* ceil-ish sqrt */
    if (s >= n) s = n - 1;
    while (s < n && gcd_sz(s, n) != 1) ++s;
    if (s >= n) {                          /* fallback: scan from 2 */
        for (s = 2; s < n; ++s) if (gcd_sz(s, n) == 1) break;
        if (s >= n) s = 1;
    }
    return (int)s;
}

void hydra_interleave_bits(const uint8_t *in, uint8_t *out, size_t n, int stride)
{
    size_t i;
    size_t st = (size_t)stride;
    for (i = 0; i < n; ++i)
        out[i] = in[(i * st) % n];
}

void hydra_deinterleave_soft(const float *in, float *out, size_t n, int stride)
{
    size_t i;
    size_t st = (size_t)stride;
    for (i = 0; i < n; ++i)
        out[(i * st) % n] = in[i];
}
