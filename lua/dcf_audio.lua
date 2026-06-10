-- SPDX-License-Identifier: LGPL-3.0-only
-- Copyright (c) 2026 DeMoD LLC. A commercial license is available on request — see LICENSING.md.
-- ============================================================================
--  dcf_audio.lua — DCF-Audio L2 framing + frequency rendezvous (pure Lua).
--
--  The canonical, intentionally-open Lua port of the CERTIFIED DCF-Audio codec
--  from this repo (python/MCP/audiolab_core.py + codec/{demod_audio.h,src/audio.rs});
--  the 17-byte DeModFrame codec is byte-identical to GUI/wirelab.lua / wirelab_core.py
--  / the C and Rust references. Audio is an *adapter* over the wire quantum: a 20 ms
--  codec block is serialised into 1 + frag_total ordinary CTRL frames.
--
--  L2 layout (all frames version=1, type=CTRL(3); big-endian):
--    seq (u16) = packet_id[15:5] (11 bits) | frag_idx[4:0] (5 bits)
--    frag_idx 0  descriptor : payload = [payload_len, frag_total, codec_id, flags]
--    frag_idx k  data       : payload = bytes[(k-1)*4 .. +4]  (last frame zero-padded)
--    frag_total = ceil(payload_len/4)   (<= 31  =>  payload_len <= 124 bytes / 20 ms)
--
--  Frequency rendezvous (handshakeless): peers pre-agree a CHANNEL (the frame dst
--  field). Tune to a numeric channel or derive one from a shared passphrase via the
--  certified crc16. A node accepts a frame iff dst == its channel, or dst == 0xFFFF
--  (broadcast). No wire change — ordinary, certified DeModFrames.
--
--  Self-certifies on load (M.CERTIFIED). Requires Lua 5.3+ (native bitwise ops),
--  which the demod-ui host provides.
-- ============================================================================

local M = {}

-- ── DeModFrame wire codec (byte-identical to GUI/wirelab.lua — certified) ───
local SYNC, VERSION, FRAME_LEN = 0xD3, 1, 17

local function crc16(bytes, n)
  local crc = 0xFFFF
  for i = 1, n do
    crc = (crc ~ ((bytes[i] & 0xFF) << 8)) & 0xFFFF
    for _ = 1, 8 do
      if (crc & 0x8000) ~= 0 then crc = ((crc << 1) ~ 0x1021) & 0xFFFF
      else crc = (crc << 1) & 0xFFFF end
    end
  end
  return crc
end

local function encode(ftype, seq, src, dst, payload, ts)
  local w = {}
  w[1] = SYNC
  w[2] = ((VERSION & 0xF) << 4) | (ftype & 0xF)
  w[3] = (seq >> 8) & 0xFF; w[4] = seq & 0xFF
  w[5] = (src >> 8) & 0xFF; w[6] = src & 0xFF
  w[7] = (dst >> 8) & 0xFF; w[8] = dst & 0xFF
  for i = 1, 4 do w[8 + i] = (payload[i] or 0) & 0xFF end
  w[13] = (ts >> 16) & 0xFF; w[14] = (ts >> 8) & 0xFF; w[15] = ts & 0xFF
  local c = crc16(w, 15)
  w[16] = (c >> 8) & 0xFF; w[17] = c & 0xFF
  return w
end

local function decode(w)
  if #w ~= FRAME_LEN then return nil, "length" end
  if w[1] ~= SYNC then return nil, "bad sync" end
  if (w[2] >> 4) ~= VERSION then return nil, "bad version" end
  if crc16(w, 15) ~= ((w[16] << 8) | w[17]) then return nil, "crc" end
  return {
    ftype = w[2] & 0xF,
    seq = (w[3] << 8) | w[4], src = (w[5] << 8) | w[6], dst = (w[7] << 8) | w[8],
    payload = { w[9], w[10], w[11], w[12] },
    ts = (w[13] << 16) | (w[14] << 8) | w[15],
  }
end

local function frame_to_hex(w)
  local t = {}
  for i = 1, #w do t[i] = ("%02X"):format(w[i]) end
  return table.concat(t)
end

M.crc16, M.encode, M.decode, M.frame_to_hex = crc16, encode, decode, frame_to_hex

-- ── L2 constants ────────────────────────────────────────────────────────────
M.FCTRL        = 3
M.FRAG_BITS    = 5
M.FRAG_MASK    = 0x1F
M.MAX_FRAGS    = 31
M.MAX_PAYLOAD  = 124            -- bytes / 20 ms block
M.MAX_PACKETID = 2047
M.BROADCAST    = 0xFFFF
M.CODEC_OPUS, M.CODEC_PCM_DIAG, M.CODEC_FAUST_PM = 0, 1, 2  -- id 3 reserved
M.FLAG_END_TALKSPURT, M.FLAG_PM_VOICE = 0x01, 0x02

-- ── Frequency rendezvous (channel = frame dst) ──────────────────────────────
-- Derive a u16 channel from a shared passphrase using the certified crc16, so two
-- peers who agree on a word land on the same handshakeless channel. crc16 of the
-- empty string is 0xFFFF (== broadcast) — callers should treat "" as "no passphrase".
function M.channel_from_passphrase(s)
  local b = {}
  for i = 1, #s do b[i] = string.byte(s, i) end
  return crc16(b, #s)
end

-- A node tuned to my_channel accepts a frame addressed to its channel or broadcast.
function M.accepts(frame_dst, my_channel)
  return frame_dst == my_channel or frame_dst == M.BROADCAST
end

-- ── L2: packetize ───────────────────────────────────────────────────────────
-- payload: 1-indexed byte array (0..124). dst is the rendezvous channel (or
-- BROADCAST). Returns { frame, ... }, descriptor first then data fragments.
function M.packetize(codec_id, payload, packet_id, ts_us, src, dst, flags)
  local n = #payload
  assert(n <= M.MAX_PAYLOAD, "payload exceeds 124 B/block")
  assert(packet_id <= M.MAX_PACKETID, "packet_id exceeds 11 bits")
  flags = flags or 0
  local frag_total = (n + 3) // 4
  local frames = {}

  local desc_seq = (packet_id << M.FRAG_BITS)
  frames[1] = encode(M.FCTRL, desc_seq, src, dst, { n, frag_total, codec_id, flags }, ts_us)

  for k = 1, frag_total do
    local off = (k - 1) * 4
    local chunk = { payload[off + 1] or 0, payload[off + 2] or 0,
                    payload[off + 3] or 0, payload[off + 4] or 0 }
    local seq = (packet_id << M.FRAG_BITS) | k
    frames[k + 1] = encode(M.FCTRL, seq, src, dst, chunk, ts_us)
  end
  return frames
end

-- ── L2: reassembler (channel-aware) ─────────────────────────────────────────
local Reassembler = {}
Reassembler.__index = Reassembler

-- channel: if given, frames not addressed to this channel (and not broadcast) are
-- dropped (the handshakeless rendezvous filter). Pass nil to accept everything.
function Reassembler.new(channel)
  return setmetatable({ slots = {}, channel = channel, rejected = 0 }, Reassembler)
end

function Reassembler:push(frame)
  local d = decode(frame)
  if not d or d.ftype ~= M.FCTRL then return nil end
  if self.channel ~= nil and not M.accepts(d.dst, self.channel) then
    self.rejected = self.rejected + 1
    return nil
  end
  local pid = d.seq >> M.FRAG_BITS
  local fidx = d.seq & M.FRAG_MASK
  local s = self.slots[pid]
  if not s then s = { desc = nil, ts = d.ts, frags = {} }; self.slots[pid] = s end
  s.ts = d.ts
  if fidx == 0 then
    if not s.desc then
      s.desc = { len = d.payload[1], total = d.payload[2], codec = d.payload[3], flags = d.payload[4] }
    end
  elseif not s.frags[fidx] then
    s.frags[fidx] = d.payload
  end
  if not s.desc then return nil end
  for k = 1, s.desc.total do if not s.frags[k] then return nil end end
  local bytes = {}
  for k = 1, s.desc.total do
    local p = s.frags[k]
    bytes[#bytes + 1] = p[1]; bytes[#bytes + 1] = p[2]
    bytes[#bytes + 1] = p[3]; bytes[#bytes + 1] = p[4]
  end
  for i = #bytes, s.desc.len + 1, -1 do bytes[i] = nil end
  self.slots[pid] = nil
  return { packet_id = pid, ts_us = s.ts, codec_id = s.desc.codec, flags = s.desc.flags, payload = bytes }
end

function Reassembler:finalize()
  local lost = {}
  for pid in pairs(self.slots) do lost[#lost + 1] = pid end
  table.sort(lost)
  self.slots = {}
  return lost
end

M.Reassembler = Reassembler

-- ── L1: PCM-diagnostic codec (id 1) — 6 kHz 8-bit mono, 120 B/block ─────────
M.PCM_DIAG_RATE, M.PCM_DIAG_BLOCK = 6000, 120

function M.pcm_diag_encode(samples)
  local out = {}
  for i = 1, #samples do
    local v = math.floor(samples[i] * 128.0 + 0.5) + 128
    out[i] = (v < 0 and 0) or (v > 255 and 255) or v
  end
  return out
end

function M.pcm_diag_decode(bytes)
  local out = {}
  for i = 1, #bytes do out[i] = (bytes[i] - 128) / 128.0 end
  return out
end

-- ── L1: Faust phase-mod (id 2) — 8-byte parameter block (certified layout) ──
function M.pm_pack(p)
  return {
    (p.f0 >> 8) & 0xFF, p.f0 & 0xFF,
    p.amp & 0xFF, p.mod_index & 0xFF, p.mod_ratio & 0xFF,
    p.bright & 0xFF, p.env & 0xFF, p.flags & 0xFF,
  }
end

function M.pm_unpack(b)
  return {
    f0 = (b[1] << 8) | b[2],
    amp = b[3], mod_index = b[4], mod_ratio = b[5],
    bright = b[6], env = b[7], flags = b[8],
  }
end

-- ── Self-certification (matches Documentation/audio_vectors.json anchors) ────
local function selfcert()
  local a = {}
  for i = 1, 9 do a[i] = string.byte("123456789", i) end
  if crc16(a, 9) ~= 0x29B1 then return false end
  -- the channel hash reuses the certified crc16, so the same anchor pins it:
  if M.channel_from_passphrase("123456789") ~= 0x29B1 then return false end

  local frames = M.packetize(1, { 0xDE, 0xAD, 0xBE, 0xEF, 0x12, 0x34 },
                             0x10, 0x010203, 0x0001, 0xFFFF, 0)
  local want = {
    "D31302000001FFFF06020100010203CAD7",
    "D31302010001FFFFDEADBEEF0102032489",
    "D31302020001FFFF1234000001020332BC",
  }
  if #frames ~= 3 then return false end
  for i = 1, 3 do if frame_to_hex(frames[i]) ~= want[i] then return false end end

  local r = Reassembler.new()
  local got
  for i = 1, #frames do local p = r:push(frames[i]); if p then got = p end end
  if not got or got.codec_id ~= 1 or #got.payload ~= 6 then return false end
  if got.payload[1] ~= 0xDE or got.payload[6] ~= 0x34 then return false end

  -- channel filter: accept on match/broadcast, reject otherwise
  if not M.accepts(0x1234, 0x1234) or not M.accepts(M.BROADCAST, 0x1234) then return false end
  if M.accepts(0x1234, 0x5678) then return false end

  local pp = { f0 = 0x1234, amp = 17, mod_index = 99, mod_ratio = 5, bright = 33, env = 7, flags = 1 }
  local up = M.pm_unpack(M.pm_pack(pp))
  if up.f0 ~= pp.f0 or up.flags ~= pp.flags or up.mod_index ~= pp.mod_index then return false end

  return true
end

M.CERTIFIED = selfcert()

return M
