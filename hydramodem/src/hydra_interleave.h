/* hydra_interleave.h -- coprime-stride block interleaver.
 *
 * Convolutional codes fail on bursts (a room reverb fade knocks out several
 * consecutive symbols). Interleaving spreads adjacent coded bits apart so a
 * burst becomes scattered single-bit errors the Viterbi decoder can clean up.
 *
 * Permutation: perm(i) = (i*stride) mod N, with gcd(stride,N)=1 so it is a
 * bijection. stride ~ sqrt(N) gives a burst-spreading distance of ~sqrt(N).
 * Interleave and deinterleave use the same stride and are exact inverses, so
 * the soft deinterleaver (RX) mirrors the hard interleaver (TX) bit-for-bit.
 */
#ifndef HYDRA_INTERLEAVE_H
#define HYDRA_INTERLEAVE_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

/* Deterministic stride for length n (both ends compute the same value). */
int hydra_interleave_stride(size_t n);

/* TX: out[i] = in[(i*stride)%n]  (gather). */
void hydra_interleave_bits(const uint8_t *in, uint8_t *out, size_t n, int stride);

/* RX: out[(i*stride)%n] = in[i]  (scatter); exact inverse of the gather. */
void hydra_deinterleave_soft(const float *in, float *out, size_t n, int stride);

#ifdef __cplusplus
}
#endif
#endif
