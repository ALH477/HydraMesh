// SPDX-License-Identifier: LGPL-3.0-only
/*
 * libavformat/dcfdec.c — DeMoD DCF-Audio frame-dump demuxer ("dcf")
 * DeMoD LLC | LGPL-3.0
 *
 * Reads a ".dcf" frame dump — a flat concatenation of 17-byte DeModFrames, the
 * AUDIO adapter frames that ride on the DCF wire (one DeModFrame per UDP
 * MSG_AUDIO payload; see python/MCP/gen_audio_dump.py and `dcf_node wiretap
 * --dump`). DCF-Audio is an *adapter* over the 17-byte wire quantum: a 20 ms
 * codec block is serialised into 1 + frag_total CTRL frames. This demuxer reuses
 * the *certified* L2 reassembler and frame codec verbatim (demod_audio.h /
 * demod_frame.h, pinned by Documentation/audio_vectors.json + golden_vectors.json),
 * so what it hands FFmpeg is provably the same bytes the certificate covers.
 *
 * Each frame `src` id becomes one audio AVStream (audio is per-source, keyed by
 * src — mirrors the comms client's per-source tracks). The DCF codec id in the
 * packet descriptor maps onto existing FFmpeg decoders, so no custom AVCodec is
 * needed:
 *
 *   codec_id 0  Opus      -> AV_CODEC_ID_OPUS  (+ OpusHead extradata; -c:a copy is bit-exact)
 *   codec_id 1  PCM-diag  -> AV_CODEC_ID_PCM_U8 @ 6 kHz (the bytes *are* unsigned-8 PCM)
 *   codec_id 2  Faust-PM  -> synthesised here to AV_CODEC_ID_PCM_F32LE @ 48 kHz
 *
 * Packet PTS is the source's unwrapped 11-bit packet_id times the 20 ms block
 * length (in 1/sample_rate units), so lost blocks surface as honest PTS gaps.
 */
#include "libavutil/channel_layout.h"
#include "libavutil/dict.h"
#include "libavutil/intreadwrite.h"
#include "avformat.h"
#include "demux.h"
#include "internal.h"

/* Certified DCF codec: L2 reassembler + 17-byte frame codec + constants.
 * Included without DCF_AUDIO_{OPUS,PM,RT} so it pulls no libopus / synth deps. */
#include "demod_audio.h"

/* PM synthesis (codec_id 2): provided by dcf_pm_codec.gen.o, linked alongside. */
extern void dcf_pm_synth_block_ffi(const uint8_t *params8, float *out, int n);

#define DCF_MAX_SRCS         64
#define DCF_PID_MOD          2048ULL    /* 11-bit packet_id space                 */
#define DCF_PM_BLOCK_SAMPLES 960        /* 20 ms @ 48 kHz PM synthesis block      */

/* Keep the certified codec registry ODR-referenced so its (non-inline) static
 * vtable + helpers are not flagged unused when we don't call the registry. */
__attribute__((unused))
static const void *const dcf__keep = (const void *)dcf_codec_get;

typedef struct DCFStream {
    int      used;
    uint16_t src_id;
    int      stream_index;          /* -1 until the first complete packet         */
    uint8_t  codec_id;
    int      block_samples;         /* PTS units per 20 ms packet                 */
    int      have_pid;
    uint64_t pid_abs;               /* last unwrapped absolute packet_id          */
    dcf_audio_reasm_t reasm;        /* per-source: reassembler keys by packet_id  */
} DCFStream;

typedef struct DCFDemuxContext {
    DCFStream srcs[DCF_MAX_SRCS];
} DCFDemuxContext;

/* Unwrap an 11-bit rolling packet_id to a monotonic absolute index relative to a
 * previous absolute value (forward progress dominates; small backward = reorder).
 * Mirrors client/src-tauri/src/sync.rs::unwrap_pid. */
static uint64_t dcf_unwrap_pid(uint64_t prev_abs, uint16_t raw)
{
    uint64_t prev_lo = prev_abs % DCF_PID_MOD;
    uint64_t r       = (uint64_t)raw % DCF_PID_MOD;
    uint64_t fwd     = (r + DCF_PID_MOD - prev_lo) % DCF_PID_MOD;
    if (fwd <= DCF_PID_MOD / 2)
        return prev_abs + fwd;
    uint64_t back = DCF_PID_MOD - fwd;
    return prev_abs >= back ? prev_abs - back : 0;
}

static int dcf_probe(const AVProbeData *p)
{
    int n = 0;
    for (int i = 0; i + (int)DCF_FRAME_SIZE <= p->buf_size && n < 8;
         i += DCF_FRAME_SIZE) {
        if (!dcf_frame_valid(p->buf + i))   /* sync byte 0xD3 + CRC-16/CCITT gate */
            return 0;
        n++;
    }
    if (n == 0)
        return 0;
    /* A CRC-validated run of frames is essentially impossible by chance. */
    return n >= 2 ? AVPROBE_SCORE_MAX : AVPROBE_SCORE_EXTENSION + 1;
}

static int dcf_read_header(AVFormatContext *s)
{
    /* Streams appear lazily as new `src` ids are seen. */
    s->ctx_flags |= AVFMTCTX_NOHEADER;
    return 0;
}

static DCFStream *dcf_src(DCFDemuxContext *c, uint16_t src_id)
{
    DCFStream *slot = NULL;
    for (int i = 0; i < DCF_MAX_SRCS; i++) {
        if (c->srcs[i].used && c->srcs[i].src_id == src_id)
            return &c->srcs[i];
        if (!c->srcs[i].used && !slot)
            slot = &c->srcs[i];
    }
    if (slot) {
        slot->used         = 1;
        slot->src_id       = src_id;
        slot->stream_index = -1;
    }
    return slot;
}

static int dcf_open_stream(AVFormatContext *s, DCFStream *ds,
                           const dcf_audio_packet_t *ap)
{
    AVStream *st = avformat_new_stream(s, NULL);
    if (!st)
        return AVERROR(ENOMEM);
    AVCodecParameters *par = st->codecpar;

    par->codec_type = AVMEDIA_TYPE_AUDIO;
    av_channel_layout_default(&par->ch_layout, 1);   /* mono */
    ds->codec_id = ap->codec_id;

    switch (ap->codec_id) {
    case DCF_CODEC_OPUS:
        par->codec_id     = AV_CODEC_ID_OPUS;
        par->sample_rate  = 48000;
        ds->block_samples = 960;
        /* OpusHead (RFC 7845 §5.1): mono, 48 kHz, 3840-sample preskip — lets a
         * downstream muxer carry the raw Opus packets bit-exact with -c:a copy. */
        {
            int ret = ff_alloc_extradata(par, 19);
            if (ret < 0)
                return ret;
            uint8_t *e = par->extradata;
            memcpy(e, "OpusHead", 8);
            e[8] = 1;                 /* version       */
            e[9] = 1;                 /* channel count */
            AV_WL16(e + 10, 3840);    /* pre-skip      */
            AV_WL32(e + 12, 48000);   /* input rate    */
            AV_WL16(e + 16, 0);       /* output gain   */
            e[18] = 0;                /* mapping family 0 */
        }
        break;
    case DCF_CODEC_PCM_DIAG:
        par->codec_id              = AV_CODEC_ID_PCM_U8;
        par->sample_rate           = DCF_PCM_DIAG_RATE;    /* 6000 */
        par->bits_per_coded_sample = 8;
        ds->block_samples          = DCF_PCM_DIAG_BLOCK;   /* 120  */
        break;
    case DCF_CODEC_FAUST_PM:
        par->codec_id              = AV_CODEC_ID_PCM_F32LE;
        par->sample_rate           = 48000;
        par->bits_per_coded_sample = 32;
        ds->block_samples          = DCF_PM_BLOCK_SAMPLES; /* 960 */
        break;
    default:
        av_log(s, AV_LOG_WARNING,
               "dcf: unknown codec_id %u for src 0x%04x — emitting raw bytes\n",
               ap->codec_id, ds->src_id);
        par->codec_id     = AV_CODEC_ID_NONE;
        par->sample_rate  = 48000;
        ds->block_samples = 960;
        break;
    }

    st->id         = ds->src_id;   /* the peer (frame src) id — used for per-peer split */
    st->start_time = 0;
    /* Human-readable label so players / ffprobe / mka tracks name each peer. */
    {
        char title[32];
        snprintf(title, sizeof title, "peer 0x%04x", ds->src_id);
        av_dict_set(&st->metadata, "title", title, 0);
    }
    avpriv_set_pts_info(st, 64, 1, par->sample_rate);
    ds->stream_index = st->index;
    return 0;
}

static int dcf_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    DCFDemuxContext *c = s->priv_data;
    AVIOContext *pb = s->pb;
    uint8_t frame[DCF_FRAME_SIZE];

    for (;;) {
        if (avio_feof(pb))
            return AVERROR_EOF;

        int r = avio_read(pb, frame, DCF_FRAME_SIZE);
        if (r < (int)DCF_FRAME_SIZE)
            return AVERROR_EOF;           /* clean EOF or trailing partial frame  */

        dcf_frame_t fr;
        if (!dcf_frame_decode(frame, &fr))
            continue;                     /* junk between frames — resync          */
        if (fr.type != DCF_TYPE_CTRL)
            continue;                     /* not an audio adapter frame            */

        DCFStream *ds = dcf_src(c, fr.src_id);
        if (!ds)
            continue;                     /* > DCF_MAX_SRCS distinct sources        */

        dcf_audio_packet_t ap;
        if (dcf_audio_reasm_push(&ds->reasm, frame, &ap) != DCF_REASM_PACKET)
            continue;                     /* descriptor/fragments not all in yet    */

        if (ds->stream_index < 0) {
            int ret = dcf_open_stream(s, ds, &ap);
            if (ret < 0)
                return ret;
        }

        uint64_t abs;
        if (!ds->have_pid) {
            abs          = ap.packet_id % DCF_PID_MOD;
            ds->have_pid = 1;
            ds->pid_abs  = abs;
        } else {
            abs = dcf_unwrap_pid(ds->pid_abs, ap.packet_id);
            if (abs > ds->pid_abs)
                ds->pid_abs = abs;
        }
        int64_t pts = (int64_t)abs * ds->block_samples;

        int ret;
        if (ds->codec_id == DCF_CODEC_FAUST_PM) {
            /* Synthesise the 8-byte PM param block into one f32 PCM block. */
            ret = av_new_packet(pkt, DCF_PM_BLOCK_SAMPLES * (int)sizeof(float));
            if (ret < 0)
                return ret;
            uint8_t params[8] = { 0 };
            memcpy(params, ap.payload, ap.payload_len < 8 ? ap.payload_len : 8);
            dcf_pm_synth_block_ffi(params, (float *)pkt->data, DCF_PM_BLOCK_SAMPLES);
        } else {
            /* Opus packet / PCM-diag bytes pass through verbatim. */
            ret = av_new_packet(pkt, ap.payload_len);
            if (ret < 0)
                return ret;
            memcpy(pkt->data, ap.payload, ap.payload_len);
        }

        pkt->stream_index = ds->stream_index;
        pkt->pts          = pts;
        pkt->dts          = pts;
        pkt->duration     = ds->block_samples;
        pkt->flags       |= AV_PKT_FLAG_KEY;
        return 0;
    }
}

const FFInputFormat ff_dcf_demuxer = {
    .p.name         = "dcf",
    .p.long_name    = NULL_IF_CONFIG_SMALL("DeMoD DCF-Audio frame dump"),
    .p.extensions   = "dcf",
    .p.flags        = AVFMT_GENERIC_INDEX,
    .priv_data_size = sizeof(DCFDemuxContext),
    .read_probe     = dcf_probe,
    .read_header    = dcf_read_header,
    .read_packet    = dcf_read_packet,
};
