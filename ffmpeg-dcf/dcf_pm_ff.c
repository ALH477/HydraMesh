// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
// FFmpeg build shim for the DCF Faust phase-mod synth (codec_id 2). It compiles
// the committed, verbatim Faust output (dcf_pm_codec.gen.c, which #includes
// dcf_pm_faust.c) into libavformat and exposes dcf_pm_synth_block_ffi for the
// `dcf` demuxer. The Faust-generated C deliberately uses global functions with no
// separate prototypes; FFmpeg builds with -Werror=missing-prototypes, so we
// locally relax just that one diagnostic for this third-party-style codegen — the
// synthesis bytes themselves are unchanged (and intentionally NOT byte-certified).
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-prototypes"
#include "dcf_pm_codec.gen.c"
#pragma GCC diagnostic pop
