-- SPDX-License-Identifier: LGPL-3.0-only
-- Copyright (c) 2026 DeMoD LLC. A commercial license is available on request — see LICENSING.md.
-- ============================================================================
--  dcf_superpack.lua — DCF SuperPack container (pure Lua, self-certifying).
--
--  SuperPack carries TWO 17-byte DeModFrame quanta in ONE 32-byte message under a
--  single joint CRC-16. Two raw frames cost 2*17 = 34 bytes; when frames are sent
--  in pairs the second header is largely recoverable from context (both inner sync
--  bytes are 0xD3; each inner CRC is a pure function of its own 15 leading bytes).
--  SuperPack drops those 6 redundant bytes and spends 4 back on one outer sync, a
--  type/version tag, and one joint CRC — net 34 -> 32 bytes plus stronger integrity.
--
--  Why it is the lower-latency option: a SuperPack puts a frame pair on the wire as
--  a SINGLE datagram instead of two — one packet, one IP/UDP header, one syscall —
--  so paired traffic crosses the network with strictly lower per-pair overhead and
--  latency than emitting the two frames separately.
--
--  Unpack reconstructs each inner frame bit-exact, so the outputs are ordinary
--  valid DeModFrames and the 246-vector wire certificate is untouched. The codec
--  here is byte-identical to GUI/wirelab.lua / wirelab_core.py / the C/Rust/Go refs.
--
--  Layout (1-indexed bytes): [1] sync 0xD3 | [2] sflags 0x15 | [3..16] frame A core
--  | [17..30] frame B core | [31..32] CRC-16/CCITT-FALSE over [1..30].
--
--  Self-certifies on load (M.CERTIFIED) against embedded golden vectors. Lua 5.3+.
-- ============================================================================

local M = {}

local SYNC, VERSION, FRAME_LEN = 0xD3, 1, 17
local SUPER_TYPE, SUPER_LEN, SUPER_CORE_LEN = 0x05, 32, 14
local SUPER_SFLAGS = ((VERSION & 0xF) << 4) | SUPER_TYPE

M.SUPER_TYPE, M.SUPER_LEN, M.SUPER_CORE_LEN = SUPER_TYPE, SUPER_LEN, SUPER_CORE_LEN

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

-- The 14 reconstructable bytes of a 17-byte frame, or nil + reason.
local function frame_core(f)
  if #f ~= FRAME_LEN then return nil, "need a 17-byte frame" end
  if f[1] ~= SYNC then return nil, "bad sync byte" end
  if (f[2] >> 4) ~= VERSION then return nil, "bad version nibble" end
  if crc16(f, 15) ~= ((f[16] << 8) | f[17]) then return nil, "inner frame CRC mismatch" end
  local core = {}
  for i = 1, SUPER_CORE_LEN do core[i] = f[1 + i] end
  return core
end

-- Rebuild a full 17-byte frame from its 14-byte core (sync + recomputed CRC).
local function rebuild_frame(core)
  local f = { SYNC }
  for i = 1, SUPER_CORE_LEN do f[1 + i] = core[i] end
  local c = crc16(f, 15)
  f[16] = (c >> 8) & 0xFF
  f[17] = c & 0xFF
  return f
end

-- pack(a, b): combine two valid 17-byte frame tables into one 32-byte SuperPack.
function M.pack(a, b)
  local ca, ea = frame_core(a); if not ca then return nil, ea end
  local cb, eb = frame_core(b); if not cb then return nil, eb end
  local out = { SYNC, SUPER_SFLAGS }
  for i = 1, SUPER_CORE_LEN do
    out[2 + i] = ca[i]
    out[2 + SUPER_CORE_LEN + i] = cb[i]
  end
  local c = crc16(out, 30)
  out[31] = (c >> 8) & 0xFF
  out[32] = c & 0xFF
  return out
end

-- True iff buf looks like a SuperPack (length + sync + version/type tag).
function M.is_superpack(buf)
  return #buf == SUPER_LEN and buf[1] == SYNC and buf[2] == SUPER_SFLAGS
end

-- unpack(buf): split a 32-byte SuperPack into frame_a, frame_b, or nil + reason.
function M.unpack(buf)
  if #buf ~= SUPER_LEN then return nil, "length ~= 32" end
  if buf[1] ~= SYNC then return nil, "bad sync byte" end
  if (buf[2] >> 4) ~= VERSION then return nil, "bad version nibble" end
  if (buf[2] & 0x0F) ~= SUPER_TYPE then return nil, "not a SuperPack type" end
  if crc16(buf, 30) ~= ((buf[31] << 8) | buf[32]) then return nil, "SuperPack CRC mismatch" end
  local core_a, core_b = {}, {}
  for i = 1, SUPER_CORE_LEN do
    core_a[i] = buf[2 + i]
    core_b[i] = buf[2 + SUPER_CORE_LEN + i]
  end
  return rebuild_frame(core_a), rebuild_frame(core_b)
end

-- ── hex helpers ─────────────────────────────────────────────────────────────
local function bytes_to_hex(w)
  local t = {}
  for i = 1, #w do t[i] = ("%02x"):format(w[i]) end
  return table.concat(t)
end

local function hex_to_bytes(s)
  local w = {}
  for i = 1, #s // 2 do w[i] = tonumber(s:sub(i * 2 - 1, i * 2), 16) end
  return w
end

M.bytes_to_hex, M.hex_to_bytes = bytes_to_hex, hex_to_bytes

-- ── self-cert against embedded golden vectors (superpack_vectors.json subset) ─
local function selfcert()
  local cases = {
    { "d310000000000000000000000000005b80", "d31312340001ffffdeadbeefab12cd24c0",
      "d31510000000000000000000000000001312340001ffffdeadbeefab12cd2435" },
    { "d310010203040506cafebabe010203f4af", "d3117fffa1a100b270696e670000ff93b3",
      "d31510010203040506cafebabe010203117fffa1a100b270696e670000ff2ea0" },
    { "d31200a010002000ff00ff00ffffffb630", "d313ffffffffffffffffffffffffff00fc",
      "d3151200a010002000ff00ff00ffffff13ffffffffffffffffffffffffff02d4" },
  }
  for _, c in ipairs(cases) do
    local a, b, sp = hex_to_bytes(c[1]), hex_to_bytes(c[2]), c[3]
    local packed = M.pack(a, b)
    if not packed or bytes_to_hex(packed) ~= sp then return false end
    if not M.is_superpack(hex_to_bytes(sp)) then return false end
    local ra, rb = M.unpack(hex_to_bytes(sp))
    if not ra or bytes_to_hex(ra) ~= c[1] or bytes_to_hex(rb) ~= c[2] then return false end
  end
  -- zero-core joint CRC anchor = 0x5B75
  local zero = hex_to_bytes("d310000000000000000000000000005b80")
  local spz = M.pack(zero, zero)
  if not spz or ((spz[31] << 8) | spz[32]) ~= 0x5B75 then return false end
  return true
end

M.CERTIFIED = selfcert()

-- Runnable standalone: `lua dcf_superpack.lua` prints the cert status.
if arg and arg[0] and arg[0]:match("dcf_superpack%.lua$") then
  if M.CERTIFIED then
    print("dcf_superpack (lua): CERTIFIED — 34 -> 32 bytes, lossless, joint-CRC, anchor 0x5B75")
  else
    io.stderr:write("dcf_superpack (lua): FAILED self-cert\n")
    os.exit(1)
  end
end

return M
