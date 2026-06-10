-- SPDX-License-Identifier: LGPL-3.0-only
-- Copyright (c) 2026 DeMoD LLC. A commercial license is available on request — see LICENSING.md.
-- ============================================================================
--  dcf_jam.lua — headless DCF-Audio jam / frequency-rendezvous demo (pure Lua).
--
--  Demonstrates the handshakeless rendezvous: pick a CHANNEL (a numeric frequency
--  or a shared passphrase), stream certified DCF-Audio to it, and watch a peer
--  TUNED to the same channel reassemble it (with packet-loss concealment) while a
--  MISTUNED peer rejects every frame. No sockets — two virtual peers in-process —
--  so it runs anywhere with stock Lua 5.3+.
--
--    lua dcf_jam.lua --passphrase basement-jam --codec pcm --loss 0.05
--    lua dcf_jam.lua --freq 1420 --blocks 50
-- ============================================================================
local HERE = (debug.getinfo(1, "S").source:gsub("^@", "")):match("(.*/)") or "./"
local A = dofile(HERE .. "dcf_audio.lua")

-- ── args ────────────────────────────────────────────────────────────────────
local opt = { codec = "pcm", blocks = 100, loss = 0.0, seed = 0xD3, freq = nil, passphrase = nil }
do
  local i = 1
  while arg and arg[i] do
    local a = arg[i]
    if a == "--codec" then i = i + 1; opt.codec = arg[i]
    elseif a == "--blocks" then i = i + 1; opt.blocks = tonumber(arg[i]) or 100
    elseif a == "--loss" then i = i + 1; opt.loss = tonumber(arg[i]) or 0.0
    elseif a == "--seed" then i = i + 1; opt.seed = tonumber(arg[i]) or 0xD3
    elseif a == "--freq" then i = i + 1; opt.freq = tonumber(arg[i])
    elseif a == "--passphrase" then i = i + 1; opt.passphrase = arg[i]
    elseif a == "--help" or a == "-h" then
      print("usage: lua dcf_jam.lua [--freq N | --passphrase W] [--codec pcm|pm] [--blocks N] [--loss P]")
      os.exit(0)
    end
    i = i + 1
  end
end

-- ── channel (the handshakeless rendezvous) ──────────────────────────────────
local channel, how
if opt.passphrase and #opt.passphrase > 0 then
  channel = A.channel_from_passphrase(opt.passphrase)
  how = ("passphrase %q -> CH %d"):format(opt.passphrase, channel)
elseif opt.freq then
  channel = opt.freq & 0xFFFF
  how = ("freq -> CH %d"):format(channel)
else
  channel = A.BROADCAST
  how = "broadcast (CH 0xFFFF — all peers hear)"
end
local mistuned_ch = (channel == A.BROADCAST) and 0x1000 or ((channel ~ 0x5A5A) & 0xFFFF)

local codec_id = (opt.codec == "pm") and A.CODEC_FAUST_PM or A.CODEC_PCM_DIAG

-- deterministic xorshift for reproducible loss
local rng = opt.seed
local function nextf()
  rng = rng ~ ((rng << 13) & 0xFFFFFFFFFFFF)
  rng = rng ~ (rng >> 7)
  rng = rng ~ ((rng << 17) & 0xFFFFFFFFFFFF)
  return (rng & 0xFFFFFF) / 0x1000000
end

local function make_payload(n)
  if codec_id == A.CODEC_FAUST_PM then
    return A.pm_pack({ f0 = 5353, amp = 200, mod_index = 64, mod_ratio = 16, bright = 40, env = 255, flags = 0 })
  end
  local s = {}
  for i = 1, A.PCM_DIAG_BLOCK do
    s[i] = 0.7 * math.sin((n * A.PCM_DIAG_BLOCK + i) * 440.0 * 2 * math.pi / A.PCM_DIAG_RATE)
  end
  return A.pcm_diag_encode(s)
end

-- ── stream to the channel; tuned vs mistuned peers receive ──────────────────
local tuned   = A.Reassembler.new(channel)
local mistuned = A.Reassembler.new(mistuned_ch)
local frames_sent, frames_dropped = 0, 0
local tuned_pkts = {}
for blk = 0, opt.blocks - 1 do
  local pid = blk & A.MAX_PACKETID
  local frames = A.packetize(codec_id, make_payload(blk), pid, (blk * 20000) & 0xFFFFFF, 1, channel, 0)
  for _, f in ipairs(frames) do
    if nextf() < opt.loss then
      frames_dropped = frames_dropped + 1
    else
      frames_sent = frames_sent + 1
      local p = tuned:push(f); if p then tuned_pkts[p.packet_id] = true end
      mistuned:push(f)
    end
  end
end
local tuned_lost = tuned:finalize()
mistuned:finalize()

-- ── playout accounting (tuned peer): recovered vs concealed ─────────────────
local recovered, concealed = 0, 0
for blk = 0, opt.blocks - 1 do
  if tuned_pkts[blk & A.MAX_PACKETID] then recovered = recovered + 1 else concealed = concealed + 1 end
end

print(("DCF jam  codec=%s  rendezvous: %s"):format(opt.codec, how))
print(("  wire      : %s"):format(A.CERTIFIED and "CERTIFIED" or "FAULT"))
print(("  frames    : %d sent, %d dropped (%.1f%% loss)")
  :format(frames_sent, frames_dropped, 100.0 * frames_dropped / math.max(1, frames_sent + frames_dropped)))
print(("  TUNED   CH %5d: %d/%d packets recovered, %d concealed (PLC)")
  :format(channel, recovered, opt.blocks, concealed))
print(("  MISTUNED CH %5d: %d frames rejected (wrong channel — no rendezvous)")
  :format(mistuned_ch, mistuned.rejected))
print((recovered + concealed >= opt.blocks and frames_sent > 0) and "JAM OK" or "JAM INCOMPLETE")
