/* hydra_fec.h -- optional forward error correction.
 *
 * Baseline scheme: repetition-3 with hard-decision majority vote. It is small,
 * provably correct, and gives real coding gain against random (AWGN-like) bit
 * errors -- enough to demonstrate the FEC hook end to end. It is NOT bandwidth
 * efficient (3x expansion) and does nothing for burst errors.
 *
 * Upgrade path (documented, not implemented here): replace these two functions
 * with a convolutional+Viterbi or Reed-Solomon codec (e.g. via an external
 * liquid-dsp / libcorrect dependency) and add a block interleaver in front to
 * spread fades. The frame layout in hydra_frame.c already isolates the coded
 * region (DCF payload + CRC bits), so swapping the codec touches only this file.
 *
 * Bits are represented as one uint8_t per bit, value 0 or 1, MSB-first.
 */
#ifndef HYDRA_FEC_H
#define HYDRA_FEC_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

/* encode: out must hold 3*n bits. Returns number of output bits (3*n). */
size_t hydra_fec_rep3_encode(const uint8_t *in_bits, size_t n, uint8_t *out_bits);

/* decode: n_coded must be a multiple of 3. out must hold n_coded/3 bits.
 * Returns number of decoded bits (n_coded/3). */
size_t hydra_fec_rep3_decode(const uint8_t *coded_bits, size_t n_coded, uint8_t *out_bits);

#ifdef __cplusplus
}
#endif
#endif
