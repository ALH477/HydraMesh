/* wav.h -- minimal mono WAV I/O (16-bit PCM) for file-based modem demos.
 * Lets you render a frame to a .wav you can inspect, play acoustically, or pipe
 * through FFmpeg (e.g. resample, transmit over a real speaker/mic link). */
#ifndef HYDRA_WAV_H
#define HYDRA_WAV_H
#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif

/* Write n mono float samples [-1,1] as 16-bit PCM at sample_rate. 0 on success. */
int hydra_wav_write(const char *path, const float *samples, size_t n, int sample_rate);

/* Read a mono 16-bit PCM WAV. Allocates *out (caller free()). Returns 0 on
 * success, <0 on error. If the file is multi-channel, channel 0 is taken. */
int hydra_wav_read(const char *path, float **out, size_t *n, int *sample_rate);

#ifdef __cplusplus
}
#endif
#endif
