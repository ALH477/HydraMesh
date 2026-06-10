-- SPDX-License-Identifier: LGPL-3.0-only
-- Copyright (c) 2026 DeMoD LLC. A commercial license is available on request — see lua/LICENSING.md.
-- ============================================================================
-- DCF AUDIOLAB — DeMoD UI front panel for DCF-Audio + frequency rendezvous.
-- Run:  ./demod-ui GUI/audiolab.lua
--
-- The audio sibling of GUI/wirelab.lua: pick a codec, tune a CHANNEL (numeric or a
-- shared passphrase), START a stream, and watch the certified DCF-Audio frames fly.
-- Reuses the LGPL framework in lua/dcf_audio.lua (self-certifies on load).
-- ============================================================================
local HERE = (debug.getinfo(1, "S").source:gsub("^@", "")):match("(.*/)") or "./"
local A = dofile(HERE .. "../lua/dcf_audio.lua")

local floor, sin, min, max = math.floor, math.sin, math.min, math.max

local COL = {
  bg = { 10, 10, 15 }, white = { 232, 232, 240 }, dim = { 106, 106, 134 },
  green = { 76, 255, 130 }, cyan = { 80, 230, 220 }, red = { 240, 90, 90 },
  amber = { 240, 190, 90 }, sel = { 30, 34, 52 }, tune = { 250, 210, 90 },
}
local FIELD = {
  { 240, 90, 90 }, { 240, 160, 80 }, { 240, 220, 90 }, { 240, 220, 90 },
  { 120, 230, 120 }, { 120, 230, 120 }, { 80, 220, 220 }, { 80, 220, 220 },
  { 120, 160, 250 }, { 120, 160, 250 }, { 120, 160, 250 }, { 120, 160, 250 },
  { 210, 120, 240 }, { 210, 120, 240 }, { 210, 120, 240 }, { 150, 150, 170 }, { 150, 150, 170 },
}

local CODECS = {
  { name = "PCM-diag", id = A.CODEC_PCM_DIAG, rate = 6000, block = 120 },
  { name = "Opus", id = A.CODEC_OPUS, rate = 48000, block = 960 },
  { name = "Faust-PM", id = A.CODEC_FAUST_PM, rate = 48000, block = 960 },
}
-- passphrase presets; index 1 = use the numeric channel, others hash a shared word
local PASS = { "(numeric)", "basement-jam", "146.520", "DeMoD" }

local ROW_FREQ, ROW_PASS = #CODECS + 1, #CODECS + 2
local ROW_XPORT = #CODECS + 3
local NROWS = ROW_XPORT

local S = {
  focus = 1, codec = 1, running = false, tuning = false,
  channel = 1420, pass = 1,
  t = 0, acc = 0, packet_id = 0, packets = 0, frames = 0, last_bytes = 0, last_frames = nil,
}

local function active_channel()
  if S.pass == 1 then return S.channel & 0xFFFF, ("CH %d"):format(S.channel & 0xFFFF) end
  local ch = A.channel_from_passphrase(PASS[S.pass])
  return ch, ("%q -> CH %d"):format(PASS[S.pass], ch)
end

local function make_payload(cdef, n)
  if cdef.id == A.CODEC_FAUST_PM then
    return A.pm_pack({ f0 = 5353, amp = 200, mod_index = 64, mod_ratio = 16, bright = 40, env = 255, flags = 0 })
  elseif cdef.id == A.CODEC_PCM_DIAG then
    local s = {}
    for i = 1, A.PCM_DIAG_BLOCK do s[i] = 0.7 * sin((n * A.PCM_DIAG_BLOCK + i) * 440.0 * 2 * math.pi / cdef.rate) end
    return A.pcm_diag_encode(s)
  else
    local b, seed = {}, (n * 2654435761) & 0xFFFFFFFF
    for i = 1, 60 do seed = (seed * 1103515245 + 12345) & 0xFFFFFFFF; b[i] = (seed >> 16) & 0xFF end
    return b
  end
end

local function jam_tick()
  local cdef = CODECS[S.codec]
  local ch = active_channel()
  local payload = make_payload(cdef, S.packets)
  S.last_frames = A.packetize(cdef.id, payload, S.packet_id, (S.packets * 20000) & 0xFFFFFF, 1, ch, 0)
  S.last_bytes = #payload
  S.packets = S.packets + 1
  S.frames = S.frames + #S.last_frames
  S.packet_id = (S.packet_id + 1) & A.MAX_PACKETID
end

local function reset_stream() S.packets, S.frames, S.packet_id, S.acc = 0, 0, 0, 0 end
local function move(d) S.focus = ((S.focus - 1 + d) % NROWS) + 1 end

local function activate()
  if S.focus <= #CODECS then S.codec = S.focus; if S.running then reset_stream() end
  elseif S.focus == ROW_FREQ then S.tuning = not S.tuning
  elseif S.focus == ROW_PASS then S.pass = (S.pass % #PASS) + 1; if S.running then reset_stream() end
  elseif S.focus == ROW_XPORT then S.running = not S.running; if S.running then reset_stream() end end
end

function on_nav(action)
  if action == "back" or action == "cancel" then
    if dm.quit then dm.quit() end
  elseif action == "up" or action == "prev" or action == "left" then
    if S.tuning and S.focus == ROW_FREQ then S.channel = (S.channel - 1) & 0xFFFF else move(-1) end
  elseif action == "down" or action == "next" or action == "right" then
    if S.tuning and S.focus == ROW_FREQ then S.channel = (S.channel + 1) & 0xFFFF else move(1) end
  elseif action == "select" or action == "ok" or action == "enter" then
    activate()
  end
  if dm.redraw then dm.redraw() end
end

function on_input(evt, btn, val)
  if evt == "DOWN" then
    if btn == "NAV_BACK" then if dm.quit then dm.quit() end
    elseif btn == "NAV_UP" then on_nav("up")
    elseif btn == "NAV_DOWN" then on_nav("down")
    elseif btn == "NAV_SELECT" or btn == "ENCODER_PUSH" then activate() end
  elseif evt == "ENCODER" and val then
    on_nav(val > 0 and "down" or "up")
  end
  if dm.redraw then dm.redraw() end
end

function on_update(dt)
  S.t = S.t + dt
  if S.running then
    S.acc = S.acc + dt
    local g = 0
    while S.acc >= 0.020 and g < 8 do S.acc = S.acc - 0.020; jam_tick(); g = g + 1 end
  end
  if dm.redraw then dm.redraw() end
end

local function txt(x, y, s, c, a) dm.draw.text(floor(x), floor(y), s, c[1], c[2], c[3], a or 230) end
local function rect(x, y, w, h, c, a) dm.draw.rect(floor(x), floor(y), floor(w), floor(h), c[1], c[2], c[3], a or 255) end

function on_draw()
  local W, H = dm.width(), dm.height()
  rect(0, 0, W, H, COL.bg)
  local off = (S.t * 14) % 4
  for y = 0, H, 4 do dm.draw.line(0, floor(y + off), W, floor(y + off), 18, 18, 30, 30) end

  txt(16, 12, "DCF AUDIOLAB", COL.white)
  local lamp = A.CERTIFIED and COL.cyan or COL.red
  rect(W - 150, 12, 10, 10, lamp, A.CERTIFIED and (140 + floor(80 * (0.5 + 0.5 * sin(S.t * 4)))) or 200)
  txt(W - 134, 12, A.CERTIFIED and "WIRE CERTIFIED" or "WIRE FAULT", lamp)

  local _, chlabel = active_channel()
  txt(16, 30, "RENDEZVOUS  " .. chlabel, COL.green, 220)

  local y0 = 52
  for i, c in ipairs(CODECS) do
    local y = y0 + (i - 1) * 20
    if S.focus == i then rect(12, y - 2, W - 24, 18, COL.sel) end
    txt(20, y, ((S.codec == i) and "● " or "○ ") .. ("id %d  %s"):format(c.id, c.name),
      (S.codec == i) and COL.cyan or COL.dim)
  end
  local fy = y0 + #CODECS * 20 + 4
  if S.focus == ROW_FREQ then rect(12, fy - 2, W - 24, 18, S.tuning and COL.tune or COL.sel) end
  txt(20, fy, ("FREQUENCY  CH %d  %s"):format(S.channel & 0xFFFF, S.tuning and "[tuning: up/down]" or ""),
    S.tuning and COL.tune or COL.white)
  local py = fy + 20
  if S.focus == ROW_PASS then rect(12, py - 2, W - 24, 18, COL.sel) end
  txt(20, py, "PASSPHRASE  " .. PASS[S.pass], COL.white)
  local ty = py + 20
  if S.focus == ROW_XPORT then rect(12, ty - 2, W - 24, 18, COL.sel) end
  txt(20, ty, S.running and "■ STOP JAM" or "▶ START JAM", S.running and COL.green or COL.amber)

  local sy = ty + 24
  txt(20, sy, ("packets %d  frames %d  block %d B  pkt_id %d")
    :format(S.packets, S.frames, S.last_bytes, S.packet_id), COL.white, 200)

  local gy = sy + 20
  txt(20, gy, "DeModFrame (descriptor) — dst = rendezvous channel", COL.dim, 170)
  local frame = S.last_frames and S.last_frames[1]
  if frame then
    local m0, by = 16, gy + 16
    local cw = max(14, min(22, floor((W - 2 * m0) / 17)))
    for i = 1, 17 do
      local fc = FIELD[i]
      rect(m0 + (i - 1) * cw, by, cw - 2, 18, fc, 60)
      dm.draw.text(floor(m0 + (i - 1) * cw + 2), floor(by + 5), ("%02X"):format(frame[i]), fc[1], fc[2], fc[3], 240)
    end
  else
    txt(16, gy + 16, "press START to stream", COL.dim, 160)
  end

  local hint = "[ up/down move/tune   select choose/toggle   back exit ]"
  txt(W / 2 - #hint * 4, H - 16, hint, COL.dim, 170)
end

io.stderr:write("[audiolab] up; wire " .. (A.CERTIFIED and "CERTIFIED" or "FAULT") .. "\n")
