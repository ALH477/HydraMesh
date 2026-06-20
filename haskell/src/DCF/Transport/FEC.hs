-- SPDX-License-Identifier: LGPL-3.0-only

-- | DCF forward-error-correction adapter — systematic Reed-Solomon over GF(2^8),
-- byte-identical to feclab_core.py, codec/demod_fec.h, codec/src/fec.rs, go/dcf/fec.go,
-- JS/nodejs/src/fec.js, cpp/include/dcf/fec.hpp, perl/lib/DCF/FEC.pm, lua/dcf_fec.lua,
-- and lisp/src/fec.lisp (pinned by Documentation/fec_vectors.json). FEC wraps a 17-byte
-- DeModFrame so a lossy medium (RF/SDR, acoustic) can CORRECT it, not just detect.
--
-- Field GF(2^8), prim 0x11D, generator 2, fcr 0. Systematic: codeword =
-- message ++ parity. Default 2t=16 parity -> corrects 8 byte-errors. The message
-- layer chunks + interleaves any-length payloads behind a self-protecting header.
module DCF.Transport.FEC
  ( rsEncode
  , rsDecode
  , interleave
  , deinterleave
  , encodeMessage
  , decodeMessage
  , rsDefaultParity
  ) where

import           Control.Monad       (forM, forM_, when)
import           Control.Monad.ST    (ST, runST)
import           Data.Array          (Array, listArray, (!))
import           Data.Array.ST       (STUArray, newArray, readArray, writeArray)
import           Data.Bits           (shiftL, shiftR, xor, (.&.), (.|.))
import           Data.List           (foldl')
import           Data.Word           (Word8)

rsDefaultParity :: Int
rsDefaultParity = 16

gfPrim :: Int
gfPrim = 0x11D

expList :: [Int]
expList = take 255 (iterate step 1)
  where step x = let y = shiftL x 1 in if y .&. 0x100 /= 0 then y `xor` gfPrim else y

gfExp :: Array Int Int
gfExp = listArray (0, 511) (expList ++ [ expList !! (i `mod` 255) | i <- [255 .. 511] ])

gfLog :: Array Int Int
gfLog = listArray (0, 255) [ logOf b | b <- [0 .. 255] ]
  where pairs = zip expList [0 ..]
        logOf 0 = 0
        logOf b = maybe 0 id (lookup b pairs)

gmul :: Int -> Int -> Int
gmul a b | a == 0 || b == 0 = 0
         | otherwise = gfExp ! (gfLog ! a + gfLog ! b)
gdiv :: Int -> Int -> Int
gdiv a b | a == 0 = 0
         | otherwise = gfExp ! ((gfLog ! a - gfLog ! b) `mod` 255)
gpow :: Int -> Int -> Int
gpow a p = gfExp ! ((gfLog ! a * p) `mod` 255)
ginv :: Int -> Int
ginv a = gfExp ! (255 - gfLog ! a)

-- Polynomials as [Int], high-order coefficient first.
pmul :: [Int] -> [Int] -> [Int]
pmul p q = [ foldl' xor 0 [ gmul (p !! i) (q !! (k - i))
                          | i <- [max 0 (k - (length q - 1)) .. min k (length p - 1)] ]
           | k <- [0 .. length p + length q - 2] ]
padd :: [Int] -> [Int] -> [Int]
padd p q = zipWith xor (pad p) (pad q)
  where n = max (length p) (length q)
        pad xs = replicate (n - length xs) 0 ++ xs
pscale :: [Int] -> Int -> [Int]
pscale p s = map (`gmul` s) p
peval :: [Int] -> Int -> Int
peval p x = foldl' (\y c -> gmul y x `xor` c) (head p) (tail p)

genpoly :: Int -> [Int]
genpoly np = foldl' (\g i -> pmul g [1, gpow 2 i]) [1] [0 .. np - 1]

-- ── encode (systematic, via the reference's polynomial division) ──────────────
rsEncodeI :: [Int] -> Int -> [Int]
rsEncodeI msg np = msg ++ runST go
  where
    m   = length msg
    gen = genpoly np
    go :: ST s [Int]
    go = do
      arr <- newArray (0, m + np - 1) 0 :: ST s (STUArray s Int Int)
      forM_ (zip [0 ..] msg) $ \(i, b) -> writeArray arr i b
      forM_ [0 .. m - 1] $ \i -> do
        coef <- readArray arr i
        when (coef /= 0) $
          forM_ [1 .. np] $ \j -> do
            v <- readArray arr (i + j)
            writeArray arr (i + j) (v `xor` gmul (gen !! j) coef)
      forM [m .. m + np - 1] (readArray arr)

rsEncode :: [Word8] -> Int -> [Word8]
rsEncode msg np = map fromIntegral (rsEncodeI (map fromIntegral msg) np)

-- ── decode (Berlekamp–Massey -> Chien -> Forney) ──────────────────────────────
syndromes :: [Int] -> Int -> [Int]
syndromes cw np = 0 : [ peval cw (gpow 2 i) | i <- [0 .. np - 1] ]

errLocator :: [Int] -> Int -> [Int]
errLocator synd np = dropWhile (== 0) (go 0 [1] [1])
  where
    go i el ol
      | i >= np = el
      | otherwise =
          let delta = foldl' xor (synd !! (i + 1))
                        [ gmul (el !! (length el - 1 - j)) (synd !! (i + 1 - j)) | j <- [1 .. length el - 1] ]
              ol' = ol ++ [0]
          in if delta == 0
               then go (i + 1) el ol'
               else let (el2, ol2) = if length ol' > length el
                                       then (pscale ol' delta, pscale el (ginv delta))
                                       else (el, ol')
                    in go (i + 1) (padd el2 (pscale ol2 delta)) ol2

rsDecodeI :: [Int] -> Int -> Int -> Either String ([Int], Int)
rsDecodeI codeword np msglen =
  let synd = syndromes codeword np
  in if all (== 0) synd
       then Right (take msglen codeword, 0)
       else
         let el  = errLocator synd np
             rev = reverse el
             errs = length rev - 1
             n   = length codeword
             pos = [ n - 1 - i | i <- [0 .. n - 1], peval rev (gpow 2 i) == 0 ]
         in if length el - 1 > np `div` 2 then Left "too many errors"
            else if length pos /= errs then Left "error location failed"
            else
              let coefPos = map (\p -> n - 1 - p) pos
                  eloc = foldl' (\e cp -> pmul e [gpow 2 cp, 1]) [1] coefPos
                  xs   = map (\cp -> gpow 2 cp) coefPos
                  prod = pmul (reverse synd) eloc
                  rem' = drop (length prod - length eloc) prod
                  fixed = runST $ do
                    arr <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Int)
                    forM_ (zip [0 ..] codeword) $ \(i, b) -> writeArray arr i b
                    forM_ (zip [0 ..] xs) $ \(i, xi) -> do
                      let xinv  = ginv xi
                          denom = foldl' (\d (j, xj) -> if j /= i then gmul d (1 `xor` gmul xinv xj) else d)
                                         1 (zip [0 ..] xs)
                          numer = gmul (peval rem' xinv) (gpow xi 1)
                      v <- readArray arr (pos !! i)
                      writeArray arr (pos !! i) (v `xor` gdiv numer denom)
                    forM [0 .. n - 1] (readArray arr)
              in if all (== 0) (syndromes fixed np)
                   then Right (take msglen fixed, length pos)
                   else Left "residual syndrome"

rsDecode :: [Word8] -> Int -> Maybe Int -> Either String ([Word8], Int)
rsDecode codeword np mlen =
  let ci = map fromIntegral codeword
      ml = maybe (length ci - np) id mlen
  in case rsDecodeI ci np ml of
       Left e        -> Left e
       Right (m, nc) -> Right (map fromIntegral m, nc)

-- ── interleaver + multi-codeword messages ─────────────────────────────────────
interleave :: [[Word8]] -> [Word8]
interleave [] = []
interleave cws = [ (cws !! r) !! c | c <- [0 .. n - 1], r <- [0 .. d - 1] ]
  where d = length cws; n = length (head cws)

deinterleave :: [Word8] -> Int -> Int -> [[Word8]]
deinterleave stream depth n =
  [ [ stream !! (c * depth + r) | c <- [0 .. n - 1] ] | r <- [0 .. depth - 1] ]

hdrParity :: Int
hdrParity = 16
hdrLen :: Int
hdrLen = 21

chunking :: Int -> Int -> (Int, Int)
chunking l np
  | l == 0    = (1, 1)
  | otherwise = let nchunks = (l + maxk - 1) `div` maxk
                    k       = (l + nchunks - 1) `div` nchunks
                in (nchunks, k)
  where maxk = 255 - np

encodeMessage :: [Word8] -> Int -> [Word8]
encodeMessage msg np =
  let l = length msg
      (nchunks, k) = chunking l np
      hdr = [ fromIntegral (shiftR l 24 .&. 255), fromIntegral (shiftR l 16 .&. 255)
            , fromIntegral (shiftR l 8 .&. 255),  fromIntegral (l .&. 255)
            , fromIntegral np ]
      cws = [ rsEncode (block c) np | c <- [0 .. nchunks - 1] ]
      block c = take k (drop (c * k) msg ++ replicate k 0)
  in rsEncode hdr hdrParity ++ interleave cws

decodeMessage :: [Word8] -> Either String ([Word8], Int)
decodeMessage blob
  | length blob < hdrLen = Left "short blob"
  | otherwise =
      case rsDecode (take hdrLen blob) hdrParity (Just 5) of
        Left e        -> Left e
        Right (h, _)  ->
          let l  = (fromIntegral (h !! 0) `shiftL` 24) .|. (fromIntegral (h !! 1) `shiftL` 16)
                   .|. (fromIntegral (h !! 2) `shiftL` 8) .|. fromIntegral (h !! 3) :: Int
              np = fromIntegral (h !! 4)
              (nchunks, k) = chunking l np
              cwlen = k + np
              body  = drop hdrLen blob
          in if length body /= nchunks * cwlen then Left "blob length mismatch"
             else
               let results = map (\cw -> rsDecode cw np (Just k)) (deinterleave body nchunks cwlen)
               in case sequence results of
                    Left e   -> Left e
                    Right rs -> Right (take l (concatMap fst rs), sum (map snd rs))
