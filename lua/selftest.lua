-- SPDX-License-Identifier: LGPL-3.0-only
-- Copyright (c) 2026 DeMoD LLC. A commercial license is available on request — see LICENSING.md.
-- ============================================================================
--  selftest.lua — certify the Lua DCF-Audio framework against the golden anchors.
--  Run:  lua lua/selftest.lua   (Lua 5.3+).  Exit 0 iff every check passes.
--
--  The embedded exampleAudioPacket frames are the certified vectors from
--  Documentation/audio_vectors.json — the same bytes the C, Rust, and Python
--  implementations are certified against.
-- ============================================================================
local HERE = (debug.getinfo(1, "S").source:gsub("^@", "")):match("(.*/)") or "./"
local A = dofile(HERE .. "dcf_audio.lua")

local ok, fail = 0, 0
local function check(name, cond)
  if cond then ok = ok + 1; print("  PASS  " .. name)
  else fail = fail + 1; print("  FAIL  " .. name) end
end

check("self-certifies on load (M.CERTIFIED)", A.CERTIFIED == true)

local frames = A.packetize(1, { 0xDE, 0xAD, 0xBE, 0xEF, 0x12, 0x34 }, 0x10, 0x010203, 0x0001, 0xFFFF, 0)
local golden = {
  "D31302000001FFFF06020100010203CAD7",
  "D31302010001FFFFDEADBEEF0102032489",
  "D31302020001FFFF1234000001020332BC",
}
local match = (#frames == 3)
for i = 1, #golden do match = match and (A.frame_to_hex(frames[i]) == golden[i]) end
check("exampleAudioPacket = golden vectors", match)

-- reassembly: reorder, drop->lost, duplicate
local big = A.packetize(1, { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }, 5, 0x010203, 1, 0xFFFF, 0)
local r = A.Reassembler.new(); local got
for _, i in ipairs({ 3, 1, 4, 2 }) do local p = r:push(big[i]); if p then got = p end end
check("reorder reassembles 10-byte payload", got and #got.payload == 10 and got.payload[10] == 10)

local r2 = A.Reassembler.new(); r2:push(big[1]); r2:push(big[2])
check("dropped fragment -> packet lost", #r2:finalize() == 1)

local r3 = A.Reassembler.new(); local dgot
for _, i in ipairs({ 1, 1, 2, 2, 3, 4 }) do local p = r3:push(big[i]); if p then dgot = p end end
check("duplicate frames ignored", dgot and #dgot.payload == 10)

-- frequency rendezvous
check("passphrase -> channel is deterministic + certified",
  A.channel_from_passphrase("123456789") == 0x29B1 and
  A.channel_from_passphrase("basement-jam") == A.channel_from_passphrase("basement-jam"))
check("accepts() matches channel and broadcast, rejects others",
  A.accepts(0x1234, 0x1234) and A.accepts(A.BROADCAST, 0x1234) and not A.accepts(0x1234, 0x5678))

-- channel-filtered reassembler: tuned accepts, mistuned rejects
local ch = A.channel_from_passphrase("basement-jam")
local onchan = A.packetize(1, { 9, 9 }, 7, 0, 1, ch, 0)
local tuned, mis = A.Reassembler.new(ch), A.Reassembler.new(ch ~ 0xFFFF)
local tg
for _, f in ipairs(onchan) do local p = tuned:push(f); if p then tg = p end; mis:push(f) end
check("tuned peer receives, mistuned peer rejects all", tg ~= nil and mis.rejected == #onchan)

-- PCM-diag + PM round-trips
local bytes = {}; for i = 1, 120 do bytes[i] = (i * 7) % 256 end
local rt = A.pcm_diag_encode(A.pcm_diag_decode(bytes))
local pcm_ok = true; for i = 1, 120 do pcm_ok = pcm_ok and (rt[i] == bytes[i]) end
check("PCM-diag decode∘encode = id (120 B)", pcm_ok)

local p = { f0 = 0x1234, amp = 17, mod_index = 99, mod_ratio = 5, bright = 33, env = 7, flags = 1 }
local up = A.pm_unpack(A.pm_pack(p))
check("PM param pack/unpack = id", up.f0 == p.f0 and up.amp == p.amp and up.flags == p.flags)

check("payload > 124 B rejected", pcall(function()
  local t = {}; for i = 1, 125 do t[i] = 0 end; A.packetize(1, t, 0, 0, 1, 1, 0)
end) == false)

print(("\n%d passed, %d failed"):format(ok, fail))
os.exit(fail == 0 and 0 or 1)
