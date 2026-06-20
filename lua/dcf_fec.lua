-- SPDX-License-Identifier: LGPL-3.0-only
-- Copyright (c) 2026 DeMoD LLC. A commercial license is available on request — see LICENSING.md.
-- ============================================================================
--  dcf_fec.lua — DCF forward error correction (pure Lua, self-certifying).
--
--  Systematic Reed-Solomon over GF(2^8) (prim 0x11D, generator a=2, fcr=0) plus a
--  block interleaver — byte-identical to python/MCP/feclab_core.py, codec/demod_fec.h,
--  codec/src/fec.rs, go/dcf/fec.go, JS/nodejs/src/fec.js, cpp/include/dcf/fec.hpp, and
--  perl/lib/DCF/FEC.pm (pinned by Documentation/fec_vectors.json). FEC wraps a 17-byte
--  DeModFrame so a lossy medium (RF/SDR, acoustic) can CORRECT it, not just detect.
--  Default 2t=16 parity -> corrects 8 byte-errors; the message layer chunks +
--  interleaves any-length payloads behind a self-protecting header.
--
--  Self-certifies on load (M.CERTIFIED) against embedded golden vectors. Lua 5.3+.
-- ============================================================================

local M = {}

local GF_PRIM, GF_GEN, FCR = 0x11D, 2, 0
local RS_DEFAULT_NPARITY, HDR_PARITY, HDR_LEN = 16, 16, 21
M.RS_DEFAULT_NPARITY, M.HDR_PARITY = RS_DEFAULT_NPARITY, HDR_PARITY

local EXP, LOG = {}, {}
do
  local x = 1
  for i = 0, 254 do EXP[i] = x; LOG[x] = i; x = x << 1; if x & 0x100 ~= 0 then x = x ~ GF_PRIM end end
  for i = 255, 511 do EXP[i] = EXP[i - 255] end
end
local function gmul(a, b) if a == 0 or b == 0 then return 0 end return EXP[LOG[a] + LOG[b]] end
local function gdiv(a, b) if a == 0 then return 0 end return EXP[(LOG[a] + 255 - LOG[b]) % 255] end
local function gpow(a, p) return EXP[((LOG[a] * p) % 255 + 255) % 255] end
local function ginv(a) return EXP[255 - LOG[a]] end

-- 0-indexed arrays {n = length, [0..n-1] = bytes} to mirror the Python reference.
local function pmul(p, q)
  local r = { n = p.n + q.n - 1 }
  for i = 0, r.n - 1 do r[i] = 0 end
  for j = 0, q.n - 1 do for i = 0, p.n - 1 do r[i + j] = r[i + j] ~ gmul(p[i], q[j]) end end
  return r
end
local function padd(p, q)
  local n = math.max(p.n, q.n)
  local r = { n = n }
  for i = 0, n - 1 do r[i] = 0 end
  for i = 0, p.n - 1 do r[i + n - p.n] = p[i] end
  for i = 0, q.n - 1 do r[i + n - q.n] = r[i + n - q.n] ~ q[i] end
  return r
end
local function pscale(p, s)
  local r = { n = p.n }
  for i = 0, p.n - 1 do r[i] = gmul(p[i], s) end
  return r
end
local function peval(p, x)
  local y = p[0]
  for i = 1, p.n - 1 do y = gmul(y, x) ~ p[i] end
  return y
end

local function genpoly(np)
  local g = { n = 1, [0] = 1 }
  for i = 0, np - 1 do g = pmul(g, { n = 2, [0] = 1, [1] = gpow(GF_GEN, FCR + i) }) end
  return g
end

-- byte string <-> 0-indexed array
local function s2a(s) local a = { n = #s } for i = 1, #s do a[i - 1] = s:byte(i) end return a end
local function a2s(a) local t = {} for i = 0, a.n - 1 do t[i + 1] = string.char(a[i]) end return table.concat(t) end
local function hex2s(h) return (h:gsub("..", function(c) return string.char(tonumber(c, 16)) end)) end
local function s2hex(s) return (s:gsub(".", function(c) return string.format("%02x", c:byte()) end)) end
M.hex_to_bytes, M.bytes_to_hex = hex2s, s2hex

-- rs_encode(byte string, nparity) -> codeword byte string (systematic).
function M.rs_encode(msg, np)
  local m = s2a(msg)
  local gen = genpoly(np)
  local out = { n = m.n + np }
  for i = 0, m.n - 1 do out[i] = m[i] end
  for i = m.n, m.n + np - 1 do out[i] = 0 end
  for i = 0, m.n - 1 do
    local coef = out[i]
    if coef ~= 0 then for j = 1, gen.n - 1 do out[i + j] = out[i + j] ~ gmul(gen[j], coef) end end
  end
  local code = { n = m.n + np }
  for i = 0, m.n - 1 do code[i] = m[i] end
  for i = 0, np - 1 do code[m.n + i] = out[m.n + i] end
  return a2s(code)
end

local function syndromes(cw, np)
  local s = { n = np + 1, [0] = 0 }
  for i = 0, np - 1 do s[i + 1] = peval(cw, gpow(GF_GEN, FCR + i)) end
  return s
end
local function errloc(synd, np)
  local el = { n = 1, [0] = 1 }
  local ol = { n = 1, [0] = 1 }
  for i = 0, np - 1 do
    local delta = synd[i + 1]
    for j = 1, el.n - 1 do delta = delta ~ gmul(el[el.n - 1 - j], synd[i + 1 - j]) end
    ol[ol.n] = 0; ol.n = ol.n + 1
    if delta ~= 0 then
      if ol.n > el.n then
        local nl = pscale(ol, delta)
        ol = pscale(el, ginv(delta))
        el = nl
      end
      el = padd(el, pscale(ol, delta))
    end
  end
  local s = 0
  while s < el.n and el[s] == 0 do s = s + 1 end
  local trimmed = { n = el.n - s }
  for i = 0, trimmed.n - 1 do trimmed[i] = el[s + i] end
  return trimmed
end
local function rev(a)
  local r = { n = a.n }
  for i = 0, a.n - 1 do r[i] = a[a.n - 1 - i] end
  return r
end

-- rs_decode(byte string, nparity[, msglen]) -> (message string, corrected count). Errors on failure.
function M.rs_decode(codeword, np, msglen)
  local cw = s2a(codeword)
  if msglen == nil then msglen = cw.n - np end
  local synd = syndromes(cw, np)
  local allzero = true
  for i = 0, synd.n - 1 do if synd[i] ~= 0 then allzero = false; break end end
  if allzero then
    local m = { n = msglen } for i = 0, msglen - 1 do m[i] = cw[i] end
    return a2s(m), 0
  end
  local el = errloc(synd, np)
  if el.n - 1 > np // 2 then error("fec: too many errors") end
  local elrev = rev(el)
  local errs = elrev.n - 1
  local pos, np2 = {}, 0
  for i = 0, cw.n - 1 do
    if peval(elrev, gpow(GF_GEN, i)) == 0 then pos[np2] = cw.n - 1 - i; np2 = np2 + 1 end
  end
  if np2 ~= errs then error("fec: error location failed") end
  -- Forney
  local eloc = { n = 1, [0] = 1 }
  local X = {}
  for i = 0, np2 - 1 do
    local cp = cw.n - 1 - pos[i]
    eloc = pmul(eloc, { n = 2, [0] = gpow(GF_GEN, cp), [1] = 1 })
    X[i] = gpow(GF_GEN, cp)
  end
  local sr = rev(synd)
  local prod = pmul(sr, eloc)
  local rem = { n = eloc.n }
  for i = 0, eloc.n - 1 do rem[i] = prod[prod.n - eloc.n + i] end
  for i = 0, np2 - 1 do
    local Xi = X[i]; local XiInv = ginv(Xi)
    local denom = 1
    for j = 0, np2 - 1 do if j ~= i then denom = gmul(denom, 1 ~ gmul(XiInv, X[j])) end end
    local numer = peval(rem, XiInv)
    numer = gmul(numer, gpow(Xi, 1 - FCR))
    if denom == 0 then error("fec: forney denominator zero") end
    cw[pos[i]] = cw[pos[i]] ~ gdiv(numer, denom)
  end
  local s2 = syndromes(cw, np)
  for i = 0, s2.n - 1 do if s2[i] ~= 0 then error("fec: residual syndrome") end end
  local m = { n = msglen } for i = 0, msglen - 1 do m[i] = cw[i] end
  return a2s(m), np2
end

-- interleave(list of equal-length codeword strings) -> stream string.
function M.interleave(cws)
  if #cws == 0 then return "" end
  local d, n = #cws, #cws[1]
  local arrs = {}
  for r = 1, d do arrs[r] = s2a(cws[r]) end
  local out = { n = d * n }
  for r = 0, d - 1 do for c = 0, n - 1 do out[c * d + r] = arrs[r + 1][c] end end
  return a2s(out)
end
function M.deinterleave(stream, depth, n)
  local s = s2a(stream)
  local cws = {}
  for r = 0, depth - 1 do
    local a = { n = n }
    for c = 0, n - 1 do a[c] = s[c * depth + r] end
    cws[r + 1] = a2s(a)
  end
  return cws
end

local function chunking(l, np)
  local maxk = 255 - np
  local nchunks = (l == 0) and 1 or ((l + maxk - 1) // maxk)
  local k = (l == 0) and 1 or ((l + nchunks - 1) // nchunks)
  return nchunks, k
end

function M.encode_message(msg, np)
  local l = #msg
  local nchunks, k = chunking(l, np)
  local hdr = string.char((l >> 24) & 255, (l >> 16) & 255, (l >> 8) & 255, l & 255, np)
  local out = M.rs_encode(hdr, HDR_PARITY)
  local cws = {}
  for c = 0, nchunks - 1 do
    local block = msg:sub(c * k + 1, c * k + k)
    if #block < k then block = block .. string.rep("\0", k - #block) end
    cws[c + 1] = M.rs_encode(block, np)
  end
  return out .. M.interleave(cws)
end

function M.decode_message(blob)
  if #blob < HDR_LEN then error("fec: short blob") end
  local hdr = M.rs_decode(blob:sub(1, HDR_LEN), HDR_PARITY, 5)
  local h = s2a(hdr)
  local l = (h[0] << 24) | (h[1] << 16) | (h[2] << 8) | h[3]
  local np = h[4]
  local nchunks, k = chunking(l, np)
  local cwlen = k + np
  local body = blob:sub(HDR_LEN + 1)
  if #body ~= nchunks * cwlen then error("fec: blob length mismatch") end
  local cws = M.deinterleave(body, nchunks, cwlen)
  local out, total = {}, 0
  for c = 1, nchunks do
    local block, nc = M.rs_decode(cws[c], np, k)
    total = total + nc
    out[c] = block
  end
  local joined = table.concat(out)
  if l < #joined then joined = joined:sub(1, l) end
  return joined, total
end

-- ── self-cert against embedded golden vectors (a fec_vectors.json subset) ─────
local function selfcert()
  local enc = {
    { "d310000000000000000000000000005b80", "d310000000000000000000000000005b802c70076e2823cf5ee0bc717689f56efe" },
    { "d31312340001ffffdeadbeefab12cd24c0", "d31312340001ffffdeadbeefab12cd24c0f318f5e24eefa07297dc976efa4a9e46" },
    { "d310010203040506cafebabe010203f4af", "d310010203040506cafebabe010203f4afcc84e6dd5eae8b567aeddf51f7038c90" },
  }
  for _, c in ipairs(enc) do
    if s2hex(M.rs_encode(hex2s(c[1]), 16)) ~= c[2] then return false end
  end
  -- pinned corrupted codeword -> recovers the frame
  local corrupt = "891000005a0000005a0000005a00005bda2c7007342823cf04e0bc712c89f56efe"
  local msg = "d310000000000000000000000000005b80"
  local ok, rec = pcall(function() return (M.rs_decode(hex2s(corrupt), 16, 17)) end)
  if not ok or s2hex(rec) ~= msg then return false end
  -- multi-codeword message: golden blob byte-identical + round-trip + burst
  local m17 = "030a11181f262d343b424950575e656c73"
  local blob17 = "00000011109c5909e6b614a1c89a033820722a7841030a11181f262d343b424950575e656c73c35135b3a0449ef708d87ccb305dd575"
  local b = M.encode_message(hex2s(m17), 16)
  if s2hex(b) ~= blob17 then return false end
  local rt = M.decode_message(b)
  if s2hex(rt) ~= m17 then return false end
  -- corrupt up to t bytes of the body -> still recovers
  local bb = { b:byte(1, #b) }
  for i = HDR_LEN + 2, HDR_LEN + 9 do bb[i] = bb[i] ~ 0x5A end
  local corrupted = string.char(table.unpack(bb))
  local ok2, rt2 = pcall(function() return (M.decode_message(corrupted)) end)
  if not ok2 or s2hex(rt2) ~= m17 then return false end
  return true
end

M.CERTIFIED = selfcert()

if arg and arg[0] and arg[0]:match("dcf_fec%.lua$") then
  if M.CERTIFIED then
    print("dcf_fec (lua): CERTIFIED — RS(GF256) encode + correct + multi-codeword messages")
  else
    io.stderr:write("dcf_fec (lua): FAILED self-cert\n")
    os.exit(1)
  end
end

return M
