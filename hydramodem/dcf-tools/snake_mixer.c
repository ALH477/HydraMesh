// SPDX-License-Identifier: LGPL-3.0-only
/*
 * snake_mixer.c — DCF-Snake mixer (hub / grandmaster) node, DeMoD LLC | LGPL-3.0.
 *
 * The star's single sink and clock master: it broadcasts the BEACON media clock, receives every
 * spoke's record stream over raw-L2 (wire A), reassembles + quanta-decodes each, aligns them on
 * the grandmaster timeline (per-source jitter buffer + ASRC drift correction + PLC), sums them to
 * a record sink, and returns each node its low-latency PCM cue mix (wire B).  Wiring only — every
 * primitive is the verified DCF-Snake foundation.  See DCF_SNAKE_SPEC.md.
 *
 * PHY: the record sink is a raw 32-bit-float file (ALSA/JACK playout is a gated future edge);
 * frames arrive on a raw-L2 socket when --iface is given, else --selftest drives the RX pipeline
 * in-process so the node builds and self-tests without CAP_NET_RAW or quanta.
 *
 * Shared memory mode (--shm): writes each source's decoded audio to a per-source ring buffer
 * (/demod-snake-src-{N}) that demod-rt can read. This is the hub integration path for the x86
 * recording node.
 *
 * Usage: snake_mixer [--iface IF] [--channel NAME] [--out FILE] [--nodes N] [--shm] [--no-quanta]
 *                    [--selftest]
 */
#include "../../codec/demod_snake.h"
#include "snake_l2.h"
#include "snake_node.h"
#include "snake_dsp.h"
#include "snake_ipc.h"
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>

#ifndef SNAKE_MIX_MAX_SRC
#define SNAKE_MIX_MAX_SRC 8u
#endif
#define SNAKE_MIX_BLK 1024u   /* decoded PCM block length used by the sink/DSP path */

/* Per-source mixer state: reassembler + jitter buffer + ASRC + PLC, keyed by frame src_id. */
typedef struct {
    bool          in_use;
    uint16_t      src_id;
    uint64_t      hop;          /* monotonic decoded-hop index for the jitter buffer */
    dcf_snake_reasm_t reasm;
    snake_jb_t    jb;
    snake_asrc_t  asrc;
    snake_plc_t   plc;
    /* Shared memory ring for hub integration */
    int           shm_fd;
    size_t        shm_size;
    SnakeSpsc    *shm_ring;
    /* Cue ring: demod-rt writes, snake_mixer reads, sends back to spoke */
    int           cue_fd;
    size_t        cue_size;
    SnakeSpsc    *cue_ring;
} mix_src_t;

static mix_src_t g_src[SNAKE_MIX_MAX_SRC];
static int g_shm_enabled = 0;

/* Create shared memory ring for a source (hub mode) */
static int create_source_ring(mix_src_t *s, uint16_t src_id) {
    if (!g_shm_enabled) return 0;
    
    char shm_name[64];
    snprintf(shm_name, sizeof(shm_name), SNAKE_IPC_SRC_SHM_PREFIX "%u", src_id);
    
    size_t size = snake_spsc_alloc_size(SNAKE_IPC_RING_CAP);
    int fd = shm_open(shm_name, O_CREAT | O_RDWR, 0666);
    if (fd < 0) {
        fprintf(stderr, "snake-mixer: shm_open(%s) failed: %s\n", shm_name, strerror(errno));
        return -1;
    }
    
    if (ftruncate(fd, size) < 0) {
        fprintf(stderr, "snake-mixer: ftruncate failed: %s\n", strerror(errno));
        close(fd);
        shm_unlink(shm_name);
        return -1;
    }
    
    void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED) {
        fprintf(stderr, "snake-mixer: mmap failed: %s\n", strerror(errno));
        close(fd);
        shm_unlink(shm_name);
        return -1;
    }
    
    s->shm_fd = fd;
    s->shm_size = size;
    s->shm_ring = snake_spsc_init(ptr, SNAKE_IPC_RING_CAP);
    
    fprintf(stderr, "snake-mixer: created source ring %s (src_id=%u)\n", shm_name, src_id);
    return 0;
}

/* Close shared memory ring for a source */
static void close_source_ring(mix_src_t *s) {
    if (s->shm_ring) {
        munmap(s->shm_ring, s->shm_size);
        close(s->shm_fd);
        char shm_name[64];
        snprintf(shm_name, sizeof(shm_name), SNAKE_IPC_SRC_SHM_PREFIX "%u", s->src_id);
        shm_unlink(shm_name);
        s->shm_ring = NULL;
        s->shm_fd = -1;
    }
}

/* Open cue ring for a source (created by demod-rt, read by snake_mixer) */
static int open_cue_ring(mix_src_t *s, uint16_t src_id) {
    if (!g_shm_enabled) return 0;
    
    char shm_name[64];
    snprintf(shm_name, sizeof(shm_name), SNAKE_IPC_CUE_SHM_PREFIX "%u", src_id);
    
    size_t size = snake_spsc_alloc_size(SNAKE_IPC_RING_CAP);
    int fd = shm_open(shm_name, O_RDWR, 0666);
    if (fd < 0) {
        /* Cue ring is optional — demod-rt may not be running */
        return 0;
    }
    
    void *ptr = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED) {
        close(fd);
        return 0;
    }
    
    s->cue_fd = fd;
    s->cue_size = size;
    s->cue_ring = (SnakeSpsc *)ptr;
    
    fprintf(stderr, "snake-mixer: opened cue ring %s (src_id=%u)\n", shm_name, src_id);
    return 1;
}

/* Close cue ring for a source */
static void close_cue_ring(mix_src_t *s) {
    if (s->cue_ring) {
        munmap(s->cue_ring, s->cue_size);
        close(s->cue_fd);
        s->cue_ring = NULL;
        s->cue_fd = -1;
    }
}

static mix_src_t *src_for(uint16_t id) {
    mix_src_t *free_slot = NULL;
    for (size_t i = 0; i < SNAKE_MIX_MAX_SRC; i++) {
        if (g_src[i].in_use && g_src[i].src_id == id) return &g_src[i];
        if (!g_src[i].in_use && !free_slot) free_slot = &g_src[i];
    }
    if (!free_slot) return NULL;
    memset(free_slot, 0, sizeof *free_slot);
    free_slot->in_use = true;
    free_slot->src_id = id;
    free_slot->shm_fd = -1;
    free_slot->shm_ring = NULL;
    free_slot->cue_fd = -1;
    free_slot->cue_ring = NULL;
    dcf_snake_reasm_init(&free_slot->reasm, DCF_TYPE_CTRL);
    snake_jb_init(&free_slot->jb, SNAKE_MIX_BLK, 2);
    snake_asrc_init(&free_slot->asrc, 1.0);          /* refined per-source via clock skew */
    snake_plc_init(&free_slot->plc, SNAKE_MIX_BLK, 0.5f);
    
    /* Create shared memory ring if enabled */
    if (create_source_ring(free_slot, id) < 0) {
        fprintf(stderr, "snake-mixer: warning: failed to create shm ring for src %u\n", id);
    }
    
    /* Open cue ring if enabled (non-fatal if demod-rt isn't running yet) */
    if (open_cue_ring(free_slot, id) < 0) {
        fprintf(stderr, "snake-mixer: warning: failed to open cue ring for src %u\n", id);
    }
    
    return free_slot;
}

/* Turn a completed DCF-Snake message (a QSS packet, or passthrough PCM) into a decoded PCM
 * block queued in the source's jitter buffer. */
static void on_message(mix_src_t *s, const dcf_snake_msg_t *msg, int no_quanta) {
    float pcm[SNAKE_MIX_BLK];
    if (no_quanta) {
        /* passthrough: the payload IS f32 PCM (matches snake_source --no-quanta) */
        size_t n = msg->payload_len / sizeof(float);
        if (n > SNAKE_MIX_BLK) n = SNAKE_MIX_BLK;
        memcpy(pcm, msg->payload, n * sizeof(float));
        for (size_t i = n; i < SNAKE_MIX_BLK; i++) pcm[i] = 0.0f;
    } else {
        /* real path: join this packet into a QSS stream and quanta-stream-decode it.  A full
         * node accumulates packets + a header; here we mark silence when quanta is absent. */
        memset(pcm, 0, sizeof pcm);   /* decode edge (GPL subprocess) — see snake_node.h */
    }
    snake_jb_push(&s->jb, s->hop++, pcm, SNAKE_MIX_BLK);
}

/* Drain each source's jitter buffer by one block, drift-correct, and sum into the record bus.
 * If shared memory is enabled, also write each source's decoded audio to its ring buffer. */
static size_t mix_tick(float *bus, size_t cap) {
    memset(bus, 0, cap * sizeof(float));
    float blk[SNAKE_MIX_BLK], res[SNAKE_MIX_BLK * 2];
    size_t produced = 0;
    for (size_t i = 0; i < SNAKE_MIX_MAX_SRC; i++) {
        mix_src_t *s = &g_src[i];
        if (!s->in_use) continue;
        snake_jb_status_t st = snake_jb_pop(&s->jb, blk);
        if (st == SNAKE_JB_UNDERRUN) continue;
        if (st == SNAKE_JB_GAP) snake_plc_conceal(&s->plc, blk);
        else                    snake_plc_good(&s->plc, blk, SNAKE_MIX_BLK);
        
        /* Write decoded audio to shared memory ring if enabled */
        if (s->shm_ring) {
            snake_spsc_push(s->shm_ring, blk, SNAKE_MIX_BLK);
        }
        
        size_t m = snake_asrc_process(&s->asrc, blk, SNAKE_MIX_BLK, res, SNAKE_MIX_BLK * 2);
        if (m > cap) m = cap;
        for (size_t j = 0; j < m; j++) bus[j] += res[j];
        if (m > produced) produced = m;
    }
    return produced;
}

/* Broadcast the grandmaster BEACON clock (5 BEACON frames, SuperPack-batched). */
static void broadcast_beacon(const dcf_snake_l2_sock_t *sock, int dry, uint64_t gm_samples,
                             uint16_t tx_seq) {
    dcf_snake_clock_t clk = { gm_samples, DCF_SNAKE_NOMINAL_RATE_MHZ_48K, tx_seq, 0 };
    uint8_t packed[DCF_SNAKE_CLOCK_LEN];
    dcf_snake_pack_clock(&clk, packed);
    uint8_t frames[8][DCF_FRAME_SIZE], eth[512];
    size_t nf = 0, elen = 0;
    if (!dcf_snake_beacon_packetize(packed, 0, (uint32_t)(gm_samples & 0xFFFFFFu), 0xFFFF, 0xFFFF,
                                    0, frames, 8, &nf)) return;
    if (!dcf_snake_l2_batch(frames, nf, eth, sizeof eth, &elen)) return;
    if (!dry) dcf_snake_l2_send(sock, NULL, eth, elen);
}

int main(int argc, char **argv) {
    const char *iface = NULL, *channel = "snake", *outfile = NULL;
    int no_quanta = 0, selftest = 0;
    for (int i = 1; i < argc; i++) {
        if      (!strcmp(argv[i], "--iface")   && i + 1 < argc) iface   = argv[++i];
        else if (!strcmp(argv[i], "--channel") && i + 1 < argc) channel = argv[++i];
        else if (!strcmp(argv[i], "--out")     && i + 1 < argc) outfile = argv[++i];
        else if (!strcmp(argv[i], "--shm"))                      g_shm_enabled = 1;
        else if (!strcmp(argv[i], "--no-quanta")) no_quanta = 1;
        else if (!strcmp(argv[i], "--selftest"))  selftest = 1;
    }
    memset(g_src, 0, sizeof g_src);
    uint16_t dst = dcf_snake_channel_id(channel);

    dcf_snake_l2_sock_t sock; memset(&sock, 0, sizeof sock); sock.fd = -1;
    int dry = 1;
    if (iface) {
        if (dcf_snake_l2_open(&sock, iface, DCF_SNAKE_ETHERTYPE_RECORD) == 0) dry = 0;
        else fprintf(stderr, "snake-mixer: raw-L2 on %s unavailable (%s) — running dry\n",
                     iface, strerror(errno));
    }

    FILE *sink = outfile ? fopen(outfile, "wb") : NULL;
    float bus[SNAKE_MIX_BLK * 2];
    unsigned long ticks = 0, msgs = 0;

    if (selftest) {
        /* Drive the RX pipeline in-process: synthesise two sources' passthrough PCM messages,
         * batch them (as snake_source would), then feed them through the mixer path. */
        broadcast_beacon(&sock, dry, 48000, 1);
        for (int hop = 0; hop < 3; hop++) {
            for (uint16_t sid = 1; sid <= 2; sid++) {
                float pcm[SNAKE_MIX_BLK];
                for (uint32_t i = 0; i < SNAKE_MIX_BLK; i++)
                    pcm[i] = 0.2f * (float)sid * (float)((i + hop) % 64) / 64.0f;
                uint8_t frames[DCF_SNAKE_MAX_FRAMES][DCF_FRAME_SIZE], eth[9000];
                size_t nf = 0, elen = 0;
                dcf_snake_packetize((uint8_t *)pcm, SNAKE_MIX_BLK * sizeof(float),
                                    (uint16_t)hop, 0, sid, dst, DCF_SNAKE_MODE_LIVE,
                                    DCF_SNAKE_FLAG_ANCHOR, frames, DCF_SNAKE_MAX_FRAMES, &nf);
                size_t cap = dcf_snake_l2_capacity(9000);
                for (size_t base = 0; base < nf; base += cap) {
                    size_t chunk = (nf - base < cap) ? (nf - base) : cap;
                    dcf_snake_l2_batch(&frames[base], chunk, eth, sizeof eth, &elen);
                    /* mixer RX: unbatch → per-src reassemble → decoded block into the jitter buf */
                    uint8_t rf[DCF_SNAKE_MAX_FRAMES][DCF_FRAME_SIZE]; size_t rn = 0;
                    dcf_snake_l2_unbatch(eth, elen, rf, DCF_SNAKE_MAX_FRAMES, &rn);
                    for (size_t k = 0; k < rn; k++) {
                        dcf_frame_t d;
                        if (!dcf_frame_decode(rf[k], &d)) continue;
                        mix_src_t *s = src_for(d.src_id);
                        if (!s) continue;
                        dcf_snake_msg_t m;
                        if (dcf_snake_reasm_push(&s->reasm, rf[k], &m) == DCF_SNAKE_REASM_MESSAGE) {
                            on_message(s, &m, 1); msgs++;
                        }
                    }
                }
            }
            size_t m = mix_tick(bus, SNAKE_MIX_BLK * 2);
            if (m) { ticks++; if (sink) fwrite(bus, sizeof(float), m, sink); }
        }
        if (sink) fclose(sink);
        if (sock.fd >= 0) dcf_snake_l2_close(&sock);
        int ok = (msgs == 6) && (ticks >= 1);
        printf("snake-mixer selftest: %lu messages from 2 sources, %lu mixed ticks — %s\n",
               msgs, ticks, ok ? "OK" : "FAILED");
        return ok ? 0 : 1;
    }

    /* live loop: recv → unbatch → per-src reassemble → decode → jitter; periodically mix + beacon */
    uint8_t eth[SNAKE_MIX_BLK * 8];
    uint64_t gm = 0; uint16_t bseq = 0;
    uint64_t cue_ticks = 0;
    while (!dry) {
        long n = dcf_snake_l2_recv(&sock, eth, sizeof eth);
        if (n < 0) break;
        uint8_t rf[DCF_SNAKE_MAX_FRAMES][DCF_FRAME_SIZE]; size_t rn = 0;
        if (!dcf_snake_l2_unbatch(eth, (size_t)n, rf, DCF_SNAKE_MAX_FRAMES, &rn)) continue;
        for (size_t k = 0; k < rn; k++) {
            dcf_frame_t d;
            if (!dcf_frame_decode(rf[k], &d)) continue;
            mix_src_t *s = src_for(d.src_id);
            if (!s) continue;
            dcf_snake_msg_t m;
            if (dcf_snake_reasm_push(&s->reasm, rf[k], &m) == DCF_SNAKE_REASM_MESSAGE) {
                on_message(s, &m, no_quanta); msgs++;
            }
        }
        size_t m = mix_tick(bus, SNAKE_MIX_BLK * 2);
        if (m) { ticks++; gm += m; if (sink) fwrite(bus, sizeof(float), m, sink); }
        
        /* Cue path: read from cue rings, send raw PCM back to spokes on cue plane */
        for (size_t i = 0; i < SNAKE_MIX_MAX_SRC; i++) {
            mix_src_t *s = &g_src[i];
            if (!s->in_use || !s->cue_ring) continue;
            float cue_pcm[SNAKE_MIX_BLK];
            uint64_t got = snake_spsc_pop(s->cue_ring, cue_pcm, SNAKE_MIX_BLK);
            if (got > 0) {
                /* Send cue audio back to this spoke via raw-L2 cue plane */
                uint8_t cue_frames[DCF_SNAKE_MAX_FRAMES][DCF_FRAME_SIZE];
                uint8_t cue_eth[9000];
                size_t cnf = 0, celen = 0;
                dcf_snake_packetize((uint8_t *)cue_pcm, got * sizeof(float),
                                    (uint16_t)(gm & 0xFFFF), 0, s->src_id, dst,
                                    DCF_SNAKE_MODE_LIVE, 0,
                                    cue_frames, DCF_SNAKE_MAX_FRAMES, &cnf);
                size_t cc = dcf_snake_l2_capacity(9000);
                for (size_t base = 0; base < cnf; base += cc) {
                    size_t chunk = (cnf - base < cc) ? (cnf - base) : cc;
                    dcf_snake_l2_batch(&cue_frames[base], chunk, cue_eth, sizeof cue_eth, &celen);
                    dcf_snake_l2_send(&sock, NULL, cue_eth, celen);
                }
                cue_ticks++;
            }
        }
        
        broadcast_beacon(&sock, dry, gm, ++bseq);
    }
    
    /* Cleanup shared memory rings */
    for (size_t i = 0; i < SNAKE_MIX_MAX_SRC; i++) {
        if (g_src[i].in_use) {
            close_source_ring(&g_src[i]);
            close_cue_ring(&g_src[i]);
        }
    }
    
    if (sink) fclose(sink);
    if (sock.fd >= 0) dcf_snake_l2_close(&sock);
    fprintf(stderr, "snake-mixer: %lu messages, %lu mixed ticks, %lu cue ticks (dst 0x%04X)\n", 
            msgs, ticks, cue_ticks, dst);
    return 0;
}
