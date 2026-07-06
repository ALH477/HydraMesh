// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// DCF-SSTV L2 framing for Node.js — a faithful port of python/MCP/sstvlab_core.py:
// one still image is serialised into a descriptor + data DeModFrame DATA frames,
// byte-identical to the Python/Rust/C/Go references, so a JS node meshes with the
// Python DcfSstvNode.
//
//   seq (u16) = image_id[15:11] | frag_idx[10:0]
//   frag 0 (descriptor): payload = [len_hi, len_lo, format_id, flags]
//   frag k (data):       payload = bytes[(k-1)*4 .. +4]  (last chunk zero-padded)
//
// The image bytes are opaque; format_id is a hint (JPEG/PNG/raw) that never changes the
// vectors. Text and game also ride DATA(0) but use different seq splits — a node runs one
// reassembler per dst channel.

const { crc16, encode, decode, BROADCAST } = require('./frame.js');

const FDATA = 0;
const FRAG_BITS = 11;
const FRAG_MASK = (1 << FRAG_BITS) - 1; // 0x7FF
const MAX_FRAGS = FRAG_MASK; // 2047
const MAX_PAYLOAD = MAX_FRAGS * 4; // 8188
const MAX_IMAGE_ID = (1 << (16 - FRAG_BITS)) - 1; // 31

// Image format ids — opaque hints.
const FMT_RAW = 0;
const FMT_JPEG = 1;
const FMT_PNG = 2;
const FMT_WEBP = 3;
const FMT_RGB565 = 4;

// Descriptor flags — opaque to L2.
const FLAG_MORE = 0x01;
const FLAG_KEYFRAME = 0x02;
const FLAG_RELIABLE = 0x04;

/** Map a channel/passphrase to a 16-bit rendezvous dst (crc16 of the name). */
function channelId(name) {
  if (!name) return BROADCAST;
  return crc16(Buffer.from(name, 'utf8'));
}

/** Serialise one image into a list of 17-byte DeModFrame DATA frame Buffers. */
function packetize(image, imageId, tsUs, src, dst, formatId = 0, flags = 0) {
  const payload = Buffer.isBuffer(image) ? image : Buffer.from(image);
  if (formatId < 0 || formatId > 255) throw new Error('format_id must be u8');
  if (flags < 0 || flags > 255) throw new Error('flags must be u8');
  if (imageId < 0 || imageId > MAX_IMAGE_ID) throw new Error(`image_id must be 0..${MAX_IMAGE_ID}`);
  if (payload.length > MAX_PAYLOAD) throw new Error(`image ${payload.length}B exceeds ${MAX_PAYLOAD}B cap`);

  const fragTotal = Math.ceil(payload.length / 4);
  const frames = [];
  const descSeq = (imageId << FRAG_BITS) | 0;
  const descPayload = Buffer.from([(payload.length >> 8) & 0xFF, payload.length & 0xFF, formatId & 0xFF, flags & 0xFF]);
  frames.push(encode({ type: FDATA, seq: descSeq, src, dst, payload: descPayload, tsUs }));

  for (let k = 1; k <= fragTotal; k++) {
    const chunk = Buffer.alloc(4);
    payload.copy(chunk, 0, (k - 1) * 4, (k - 1) * 4 + 4);
    const seq = (imageId << FRAG_BITS) | k;
    frames.push(encode({ type: FDATA, seq, src, dst, payload: chunk, tsUs }));
  }
  return frames;
}

/** Stateful reassembler: push() emits completed images; ignores dups. */
class SstvReassembler {
  constructor(acceptDst = null) {
    this._accept = acceptDst; // null => accept every channel
    this._pkts = new Map();
  }

  push(frame) {
    let d;
    try {
      d = decode(frame);
    } catch (e) {
      return [];
    }
    if (d.type !== FDATA) return [];
    if (this._accept !== null && d.dst !== this._accept && d.dst !== BROADCAST) return [];
    const seq = d.seq;
    const imageId = seq >> FRAG_BITS;
    const fragIdx = seq & FRAG_MASK;
    const body = d.payload; // Buffer(4)

    let entry = this._pkts.get(imageId);
    if (!entry) {
      entry = { desc: null, ts: d.tsUs, src: d.src, dst: d.dst, frags: new Map() };
      this._pkts.set(imageId, entry);
    }
    entry.ts = d.tsUs;
    entry.src = d.src;
    entry.dst = d.dst;
    if (fragIdx === 0) {
      if (entry.desc === null) {
        const length = (body[0] << 8) | body[1];
        entry.desc = { len: length, formatId: body[2], flags: body[3], fragTotal: Math.ceil(length / 4) };
      }
    } else if (!entry.frags.has(fragIdx)) {
      entry.frags.set(fragIdx, Buffer.from(body));
    }
    return this._tryEmit(imageId);
  }

  _tryEmit(imageId) {
    const entry = this._pkts.get(imageId);
    if (!entry || entry.desc === null) return [];
    const { fragTotal, len, formatId, flags } = entry.desc;
    for (let k = 1; k <= fragTotal; k++) {
      if (!entry.frags.has(k)) return [];
    }
    const parts = [];
    for (let k = 1; k <= fragTotal; k++) parts.push(entry.frags.get(k));
    const raw = Buffer.concat(parts);
    const payload = raw.subarray(0, len);
    this._pkts.delete(imageId);
    return [{ imageId, tsUs: entry.ts, src: entry.src, dst: entry.dst, formatId, flags, data: Buffer.from(payload) }];
  }

  finalize() {
    const lost = [...this._pkts.keys()].sort((a, b) => a - b).map((iid) => ({ lost: iid }));
    this._pkts.clear();
    return lost;
  }
}

module.exports = {
  FDATA, FRAG_BITS, FRAG_MASK, MAX_FRAGS, MAX_PAYLOAD, MAX_IMAGE_ID,
  FMT_RAW, FMT_JPEG, FMT_PNG, FMT_WEBP, FMT_RGB565,
  FLAG_MORE, FLAG_KEYFRAME, FLAG_RELIABLE,
  channelId, packetize, SstvReassembler,
};
