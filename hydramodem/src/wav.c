/* wav.c -- minimal mono 16-bit PCM WAV reader/writer (little-endian hosts). */
#include "wav.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

static void w_u32(FILE *f, uint32_t v) { fputc(v&0xff,f);fputc((v>>8)&0xff,f);fputc((v>>16)&0xff,f);fputc((v>>24)&0xff,f); }
static void w_u16(FILE *f, uint16_t v) { fputc(v&0xff,f);fputc((v>>8)&0xff,f); }

int hydra_wav_write(const char *path, const float *samples, size_t n, int sample_rate)
{
    FILE *f = fopen(path, "wb");
    uint32_t data_bytes = (uint32_t)(n * 2u);
    uint32_t byte_rate  = (uint32_t)(sample_rate * 2);
    size_t i;
    if (!f) return -1;

    fwrite("RIFF", 1, 4, f); w_u32(f, 36u + data_bytes); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); w_u32(f, 16u);
    w_u16(f, 1);                 /* PCM            */
    w_u16(f, 1);                 /* mono           */
    w_u32(f, (uint32_t)sample_rate);
    w_u32(f, byte_rate);
    w_u16(f, 2);                 /* block align    */
    w_u16(f, 16);                /* bits/sample    */
    fwrite("data", 1, 4, f); w_u32(f, data_bytes);

    for (i = 0; i < n; ++i) {
        double s = samples[i];
        int v;
        if (s >  1.0) s =  1.0;
        if (s < -1.0) s = -1.0;
        v = (int)lround(s * 32767.0);
        w_u16(f, (uint16_t)(int16_t)v);
    }
    fclose(f);
    return 0;
}

static uint32_t r_u32(const uint8_t *p){return (uint32_t)p[0]|((uint32_t)p[1]<<8)|((uint32_t)p[2]<<16)|((uint32_t)p[3]<<24);}
static uint16_t r_u16(const uint8_t *p){return (uint16_t)(p[0]|(p[1]<<8));}

int hydra_wav_read(const char *path, float **out, size_t *n, int *sample_rate)
{
    FILE *f = fopen(path, "rb");
    uint8_t *buf; long sz;
    uint32_t pos;
    int channels = 1, bits = 16, sr = 48000;
    const uint8_t *data = NULL; uint32_t data_len = 0;

    if (!f) return -1;
    fseek(f, 0, SEEK_END); sz = ftell(f); fseek(f, 0, SEEK_SET);
    if (sz < 44) { fclose(f); return -2; }
    buf = (uint8_t *)malloc((size_t)sz);
    if (!buf) { fclose(f); return -3; }
    if (fread(buf, 1, (size_t)sz, f) != (size_t)sz) { free(buf); fclose(f); return -4; }
    fclose(f);

    if (memcmp(buf, "RIFF", 4) || memcmp(buf+8, "WAVE", 4)) { free(buf); return -5; }

    pos = 12;
    while (pos + 8 <= (uint32_t)sz) {
        const uint8_t *ck = buf + pos;
        uint32_t clen = r_u32(ck + 4);
        if (!memcmp(ck, "fmt ", 4) && clen >= 16) {
            channels = r_u16(ck + 8 + 2);
            sr       = (int)r_u32(ck + 8 + 4);
            bits     = r_u16(ck + 8 + 14);
        } else if (!memcmp(ck, "data", 4)) {
            data = ck + 8; data_len = clen;
        }
        pos += 8 + clen + (clen & 1u);     /* chunks are word-aligned */
    }
    if (!data || bits != 16 || channels < 1) { free(buf); return -6; }

    {
        size_t frames = data_len / (2u * (uint32_t)channels);
        float *o = (float *)malloc(frames * sizeof *o);
        size_t i;
        if (!o) { free(buf); return -7; }
        for (i = 0; i < frames; ++i) {
            const uint8_t *s = data + i * 2u * (uint32_t)channels; /* ch0 */
            int16_t v = (int16_t)r_u16(s);
            o[i] = (float)v / 32768.0f;
        }
        *out = o; *n = frames; if (sample_rate) *sample_rate = sr;
    }
    free(buf);
    return 0;
}
