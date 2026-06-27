/* hydra_dsp.h -- the per-sample DSP boundary.
 *
 * This is the ONE interface that the Faust-compiled DSP and the portable C
 * reference both implement. The modem layer (hydra_modem.c) talks only to this
 * interface, so you can validate framing/protocol with the C reference today
 * and drop in the Faust backend for deployment without touching anything above.
 *
 *   TX : freq_in[n] (instantaneous Hz, one per sample) -> audio_out[n]
 *   RX : audio_in[n] -> iq_out[n * 2 * n_tones]  (quadrature down-conversion,
 *        interleaved per sample then per tone:
 *        I0,Q0,I1,Q1,...,I(N-1),Q(N-1),  I0,Q0,...   = x*cos, x*sin per tone)
 *
 * The RX emits raw I/Q, NOT energy: the matched filter is a per-symbol
 * integrate-and-dump performed in C on the recovered timing grid (see
 * hydra_modem.c). This is the optimal non-coherent FSK receiver and gives the
 * C layer the soft information the Viterbi decoder needs.
 *
 * Two implementations are provided:
 *   hydra_dsp_ref.c    portable C, mirrors faust/demod_modem.lib 1:1 (default)
 *   hydra_dsp_faust.c  thin adapter over `faust -lang c -os` output (deploy)
 * Exactly one is linked (selected in the Makefile).
 */
#ifndef HYDRA_DSP_H
#define HYDRA_DSP_H

#include <stddef.h>
#include "hydra_profile.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ---- transmitter ---- */
typedef struct hydra_tx_dsp hydra_tx_dsp;
hydra_tx_dsp *hydra_tx_dsp_create(double sample_rate);
void          hydra_tx_dsp_destroy(hydra_tx_dsp *d);
/* Modulate n samples. freq_in and audio_out are length n. Output is a
 * unit-amplitude continuous-phase carrier (gain/DC-block applied above). */
void          hydra_tx_dsp_process(hydra_tx_dsp *d,
                                   const float *freq_in, float *audio_out, int n);

/* ---- receiver ---- */
typedef struct hydra_rx_dsp hydra_rx_dsp;
hydra_rx_dsp *hydra_rx_dsp_create(const hydra_profile *p);
void          hydra_rx_dsp_destroy(hydra_rx_dsp *d);
/* Quadrature down-convert n input samples. iq_out is length n*2*n_tones,
 * interleaved I0,Q0,...,I(N-1),Q(N-1) per sample (see header). */
void          hydra_rx_dsp_process(hydra_rx_dsp *d,
                                   const float *audio_in, float *iq_out, int n);

#ifdef __cplusplus
}
#endif
#endif
