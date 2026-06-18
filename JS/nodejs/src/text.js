// SPDX-License-Identifier: LGPL-3.0-only
'use strict';

// DCF-Text L2 framing for Node.js — a faithful port of python/MCP/textlab_core.py
// (and matrix-bridge/dcf_text.py): one UTF-8 message is serialised into a
// descriptor + data DeModFrame DATA frames, byte-identical to the Python/Rust/C
// references, so a JS node meshes with the Python DcfTextNode.
//
//   seq (u16) = packet_id[15:10] | frag_idx[9:0]
//   frag 0 (descriptor): payload = [len_hi, len_lo, flags, 0]
//   frag k (data):       payload = bytes[(k-1)*4 .. +4]  (last chunk zero-padded)

const { crc16, encode, decode, BROADCAST } = require('./frame.js');

const FDATA = 0;
const FRAG_BITS = 10;
const FRAG_MASK = (1 << FRAG_BITS) - 1; // 0x3FF
const MAX_FRAGS = FRAG_MASK; // 1023
const MAX_PAYLOAD = MAX_FRAGS * 4; // 4092
const MAX_PACKET_ID = (1 << (16 - FRAG_BITS)) - 1; // 63

/** Map a channel/passphrase to a 16-bit rendezvous dst (crc16 of the name). */
function channelId(name) {
  if (!name) return BROADCAST;
  return crc16(Buffer.from(name, 'utf8'));
}

/** Serialise one message into a list of 17-byte DeModFrame DATA frame Buffers. */
function packetize(text, packetId, tsUs, src, dst, flags = 0) {
  const payload = Buffer.isBuffer(text) ? text : Buffer.from(text, 'utf8');
  if (flags < 0 || flags > 255) throw new Error('flags must be u8');
  if (packetId < 0 || packetId > MAX_PACKET_ID) throw new Error(`packet_id must be 0..${MAX_PACKET_ID}`);
  if (payload.length > MAX_PAYLOAD) throw new Error(`message ${payload.length}B exceeds ${MAX_PAYLOAD}B cap`);

  const fragTotal = Math.ceil(payload.length / 4);
  const frames = [];
  const descSeq = (packetId << FRAG_BITS) | 0;
  const descPayload = Buffer.from([(payload.length >> 8) & 0xFF, payload.length & 0xFF, flags & 0xFF, 0]);
  frames.push(encode({ type: FDATA, seq: descSeq, src, dst, payload: descPayload, tsUs }));

  for (let k = 1; k <= fragTotal; k++) {
    const chunk = Buffer.alloc(4);
    payload.copy(chunk, 0, (k - 1) * 4, (k - 1) * 4 + 4);
    const seq = (packetId << FRAG_BITS) | k;
    frames.push(encode({ type: FDATA, seq, src, dst, payload: chunk, tsUs }));
  }
  return frames;
}

/** Stateful reassembler: push() emits completed messages; ignores dups. */
class TextReassembler {
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
    const packetId = seq >> FRAG_BITS;
    const fragIdx = seq & FRAG_MASK;
    const body = d.payload; // Buffer(4)

    let entry = this._pkts.get(packetId);
    if (!entry) {
      entry = { desc: null, ts: d.tsUs, src: d.src, dst: d.dst, frags: new Map() };
      this._pkts.set(packetId, entry);
    }
    entry.ts = d.tsUs;
    entry.src = d.src;
    entry.dst = d.dst;
    if (fragIdx === 0) {
      if (entry.desc === null) {
        const length = (body[0] << 8) | body[1];
        entry.desc = { len: length, flags: body[2], fragTotal: Math.ceil(length / 4) };
      }
    } else if (!entry.frags.has(fragIdx)) {
      entry.frags.set(fragIdx, Buffer.from(body));
    }
    return this._tryEmit(packetId);
  }

  _tryEmit(packetId) {
    const entry = this._pkts.get(packetId);
    if (!entry || entry.desc === null) return [];
    const { fragTotal, len, flags } = entry.desc;
    for (let k = 1; k <= fragTotal; k++) {
      if (!entry.frags.has(k)) return [];
    }
    const parts = [];
    for (let k = 1; k <= fragTotal; k++) parts.push(entry.frags.get(k));
    const raw = Buffer.concat(parts);
    const payload = raw.subarray(0, len);
    this._pkts.delete(packetId);
    return [{ packetId, tsUs: entry.ts, src: entry.src, dst: entry.dst, text: payload.toString('utf8'), flags }];
  }

  finalize() {
    const lost = [...this._pkts.keys()].sort((a, b) => a - b).map((pid) => ({ lost: pid }));
    this._pkts.clear();
    return lost;
  }
}

module.exports = {
  FDATA, FRAG_BITS, FRAG_MASK, MAX_FRAGS, MAX_PAYLOAD, MAX_PACKET_ID,
  channelId, packetize, TextReassembler,
};
