/* hydra_frame.h -- the half a modem that Faust cannot express.
 *
 * Bit/byte/symbol packing, frame assembly and the symbol-stream layout. All the
 * variable-length, data-dependent logic lives here in plain C; Faust only ever
 * sees continuous sample streams.
 *
 * Frame symbol stream produced by hydra_frame_build():
 *
 *   [ preamble        ][ sync          ][ coded( DCF payload + CRC16 )        ]
 *     preamble_syms      sync_syms        data_syms
 *     alt tone 0/N-1     sync_word bits    rep3 (or uncoded) -> symbols
 *
 * Bit order is MSB-first everywhere. Symbols are tone indices in [0, n_tones).
 */
#ifndef HYDRA_FRAME_H
#define HYDRA_FRAME_H

#include "hydra_profile.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ---- low-level bit/byte/symbol helpers (MSB-first) ---- */

/* nbytes bytes -> 8*nbytes bits (caller-owned, sized 8*nbytes). */
void hydra_bytes_to_bits(const uint8_t *bytes, size_t nbytes, uint8_t *bits);

/* nbits bits (multiple of 8) -> nbits/8 bytes. */
void hydra_bits_to_bytes(const uint8_t *bits, size_t nbits, uint8_t *bytes);

/* Pack bits into symbols, bits_per_symbol bits each, MSB-first. The final
 * symbol is zero-padded if nbits is not a multiple of bits_per_symbol.
 * Returns the number of symbols written. */
size_t hydra_bits_to_symbols(const uint8_t *bits, size_t nbits,
                             int bits_per_symbol, uint8_t *symbols);

/* Inverse: nsym symbols -> nsym*bits_per_symbol bits. */
void hydra_symbols_to_bits(const uint8_t *symbols, size_t nsym,
                           int bits_per_symbol, uint8_t *bits);

/* ---- frame assembly ---- */

/* Build the full TX symbol stream for one DCF frame: assembles payload+CRC,
 * applies the selected FEC (none/rep3/conv) and optional interleaving, maps to
 * symbols, and prepends preamble + sync.
 *   symbols_out : caller buffer, capacity `cap` symbols.
 *   *nsym_out   : set to p->total_syms on success.
 * Returns 0 on success, <0 if cap is too small or args are bad. */
int hydra_frame_build(const hydra_profile *p, const uint8_t payload[HYDRA_DCF_BYTES],
                      uint8_t *symbols_out, size_t cap, size_t *nsym_out);

/* Recover the DCF payload from soft per-coded-bit metrics in TRANSMITTED order
 * (p->coded_bits of them; sign = hard decision, magnitude = confidence). This
 * deinterleaves, runs the matching FEC decoder (soft Viterbi for CONV, majority
 * for REP3, direct for NONE), reassembles the bytes and checks the CRC.
 * Returns 0 if the CRC matches, <0 otherwise (-3 == CRC mismatch). */
int hydra_frame_decode_soft(const hydra_profile *p, const float *soft_coded,
                            uint8_t payload_out[HYDRA_DCF_BYTES]);

/* Expected sync-word bits (HYDRA_SYNC_BITS of them), MSB-first. Used by the RX
 * timing/sync search in hydra_modem.c. */
void hydra_frame_sync_bits(const hydra_profile *p, uint8_t sync_bits[HYDRA_SYNC_BITS]);

#ifdef __cplusplus
}
#endif
#endif
