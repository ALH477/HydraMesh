/* hydra_conv.h -- rate-1/2, constraint-length K=7 convolutional code with a
 * soft-decision Viterbi decoder. This is the production FEC: paired with the
 * soft metrics the FSK correlator already produces, it delivers ~5 dB of coding
 * gain at moderate SNR (and ~2 dB more than hard-decision decoding).
 *
 * Generators (octal): G0 = 0171, G1 = 0133  -- the classic K=7 industry code
 * (Voyager, IS-95, 802.11 fallback). Memory m = 6, 64 trellis states.
 *
 * The encoder appends m=6 zero tail bits so the trellis terminates in state 0,
 * which lets the decoder exploit a known end state (better than truncation).
 *
 * Soft metric convention: one float per coded bit. Sign carries the hard
 * decision (>0 => bit 1, <0 => bit 0); magnitude carries confidence. Any
 * symmetric scale works (the decoder maximizes correlation). For erasures pass 0.
 */
#ifndef HYDRA_CONV_H
#define HYDRA_CONV_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

#define HYDRA_CONV_K        7
#define HYDRA_CONV_MEM      6          /* K-1 */
#define HYDRA_CONV_STATES   64         /* 2^MEM */
#define HYDRA_CONV_RATE_NUM 1
#define HYDRA_CONV_RATE_DEN 2

/* Number of coded bits produced for n message bits (incl. tail flush). */
static inline size_t hydra_conv_coded_len(size_t n_msg)
{ return 2u * (n_msg + HYDRA_CONV_MEM); }

/* Encode n_msg message bits -> hydra_conv_coded_len(n_msg) coded bits.
 * in_bits/out_bits are one uint8_t per bit (0/1). Returns coded bit count. */
size_t hydra_conv_encode(const uint8_t *in_bits, size_t n_msg, uint8_t *out_bits);

/* Soft-decision Viterbi decode. soft[] has n_coded entries (must equal
 * hydra_conv_coded_len(n_msg)); out_bits receives n_msg decoded message bits.
 * Returns n_msg on success, 0 on allocation failure or bad length.
 * Pass n_msg so the decoder knows where the tail begins. */
size_t hydra_conv_decode_soft(const float *soft, size_t n_coded,
                              size_t n_msg, uint8_t *out_bits);

#ifdef __cplusplus
}
#endif
#endif
