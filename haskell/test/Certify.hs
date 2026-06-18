-- SPDX-License-Identifier: LGPL-3.0-only
{-# LANGUAGE OverloadedStrings #-}

-- | Certifies the Haskell DeModFrame codec ("DCF.Transport.FrameSpec") byte-for-byte
-- against the cross-language golden vectors in Documentation/golden_vectors.json.
-- Exits non-zero if any anchor, encode-basis, or syndrome-basis check fails.
module Main (main) where

import           Control.Monad        (forM_, unless, when)
import           Data.Aeson           (Value (..), decode)
import           Data.Aeson.Key       (Key)
import qualified Data.Aeson.KeyMap    as KM
import           Data.Bits            (shiftL, xor, (.&.), (.|.))
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (digitToInt, intToDigit)
import           Data.Foldable        (toList)
import           Data.IORef           (modifyIORef', newIORef, readIORef)
import           Data.Maybe           (fromMaybe)
import           Data.Scientific      (Scientific, toBoundedInteger)
import qualified Data.Text            as T
import           Data.Word            (Word16, Word8)
import           Numeric              (showHex)
import           System.Directory     (doesFileExist)
import           System.Exit          (exitFailure, exitSuccess)

import           DCF.Transport.FrameSpec
import           DCF.Transport.SuperPack (isSuperPack, packSuper, unpackSuper)

main :: IO ()
main = do
  path <- findGolden
  raw  <- BL.readFile path
  let root = fromMaybe (error ("cannot parse JSON: " ++ path)) (decode raw :: Maybe Value)
  fails <- newIORef (0 :: Int)
  let bad msg = modifyIORef' fails (+ 1) >> putStrLn ("  FAIL  " ++ msg)
      ok  msg = putStrLn ("  PASS  " ++ msg)

  -- 1. CRC anchors
  let crc1 = crc16ccitt (strBytes "123456789")
  if crc1 == 0x29B1 then ok "CRC(\"123456789\") = 0x29B1"
                    else bad ("CRC(\"123456789\") = 0x" ++ hex16 crc1)
  let crc0 = crc16ccitt (replicate 15 0)
  if crc0 == 0x4EC3 then ok "CRC(0^15) = 0x4EC3"
                    else bad ("CRC(0^15) = 0x" ++ hex16 crc0)

  -- 2. example frame anchor
  let exHex   = T.unpack (str (look "exampleFrame_full" (look "anchors" root)))
      exFrame = Frame 1 Ctrl 0x1234 1 broadcast (0xDE, 0xAD, 0xBE, 0xEF) 0xAB12CD
      exGot   = bytesHex (encodeFrame exFrame)
  if exGot == exHex then ok "exampleFrame_full matches"
                    else bad ("exampleFrame: got " ++ exGot ++ " want " ++ exHex)

  -- 3. encode_basis: raw-CRC-valid + (known types) decode/roundtrip
  let encs = arr (look "encode_basis" root)
  forM_ (zip [0 :: Int ..] encs) $ \(i, o) -> do
    let bytes = hexBytes (T.unpack (str (look "frame" o)))
    if not (rawValid bytes)
      then bad ("encode_basis[" ++ show i ++ "]: raw CRC invalid")
      else when ((bytes !! 1) .&. 0x0F <= 3) $
             case decodeFrame bytes of
               Right f  -> unless (encodeFrame f == bytes) $
                             bad ("encode_basis[" ++ show i ++ "]: roundtrip mismatch")
               Left err -> bad ("encode_basis[" ++ show i ++ "]: " ++ show err)

  -- 4. syndrome_basis
  let syns = arr (look "syndrome_basis" root)
  forM_ (zip [0 :: Int ..] syns) $ \(i, o) -> do
    let word = case lookMaybe "bit" o of
                 Just (Number n) -> oneBit (sciInt n)
                 _               -> replicate frameSize 0
        expected = fromIntegral (numInt (look "syndrome" o)) :: Word16
        got = crc16ccitt (take 15 word) `xor` makeW16 (word !! 15) (word !! 16)
    when (got /= expected) $
      bad ("syndrome_basis[" ++ show i ++ "]: got 0x" ++ hex16 got ++ " want 0x" ++ hex16 expected)

  -- 5. SuperPack container vectors (Documentation/superpack_vectors.json)
  spPath <- findSuper
  spRaw  <- BL.readFile spPath
  let spRoot  = fromMaybe (error ("cannot parse JSON: " ++ spPath)) (decode spRaw :: Maybe Value)
      spCases = arr (look "cases" spRoot)
  forM_ (zip [0 :: Int ..] spCases) $ \(i, o) -> do
    let aHex = T.unpack (str (look "a" o))
        bHex = T.unpack (str (look "b" o))
        spHx = T.unpack (str (look "super" o))
    case packSuper (hexBytes aHex) (hexBytes bHex) of
      Left e   -> bad ("superpack[" ++ show i ++ "]: pack " ++ e)
      Right pk -> do
        unless (isSuperPack pk) $ bad ("superpack[" ++ show i ++ "]: not recognised")
        unless (bytesHex pk == spHx) $ bad ("superpack[" ++ show i ++ "]: pack bytes")
    case unpackSuper (hexBytes spHx) of
      Left e         -> bad ("superpack[" ++ show i ++ "]: unpack " ++ e)
      Right (ra, rb) ->
        unless (bytesHex ra == aHex && bytesHex rb == bHex) $
          bad ("superpack[" ++ show i ++ "]: unpack mismatch")
  let zeroFrame = encodeFrame (Frame 1 Data 0 0 0 (0, 0, 0, 0) 0)
  case packSuper zeroFrame zeroFrame of
    Right spz -> unless (makeW16 (spz !! 30) (spz !! 31) == 0x5B75) $
                   bad "superpack zero-core anchor /= 0x5B75"
    Left e    -> bad ("superpack zero-core: " ++ e)

  n <- readIORef fails
  putStrLn ""
  if n == 0
    then do putStrLn ("ALL CHECKS PASSED (" ++ show (length encs) ++ " encode + "
                      ++ show (length syns) ++ " syndrome + " ++ show (length spCases)
                      ++ " superpack) — Haskell codec cemented.")
            exitSuccess
    else do putStrLn (show n ++ " certification check(s) FAILED")
            exitFailure

-- ── JSON helpers ────────────────────────────────────────────────────────────

findGolden :: IO FilePath
findGolden = go [ "../Documentation/golden_vectors.json"
                , "Documentation/golden_vectors.json"
                , "../python/MCP/golden_vectors.json" ]
  where
    go []       = error "golden_vectors.json not found in expected locations"
    go (p : ps) = do e <- doesFileExist p; if e then return p else go ps

findSuper :: IO FilePath
findSuper = go [ "../Documentation/superpack_vectors.json"
               , "Documentation/superpack_vectors.json"
               , "../python/MCP/superpack_vectors.json" ]
  where
    go []       = error "superpack_vectors.json not found (run gen_superpack_vectors.py)"
    go (p : ps) = do e <- doesFileExist p; if e then return p else go ps

look :: Key -> Value -> Value
look k (Object o) = fromMaybe (error ("missing key " ++ show k)) (KM.lookup k o)
look k _          = error ("expected object for key " ++ show k)

lookMaybe :: Key -> Value -> Maybe Value
lookMaybe k (Object o) = KM.lookup k o
lookMaybe _ _          = Nothing

arr :: Value -> [Value]
arr (Array a) = toList a
arr _         = error "expected array"

str :: Value -> T.Text
str (String t) = t
str _          = error "expected string"

numInt :: Value -> Int
numInt (Number n) = sciInt n
numInt _          = error "expected number"

sciInt :: Scientific -> Int
sciInt s = fromMaybe (error "non-integer number") (toBoundedInteger s)

-- ── byte / hex helpers ──────────────────────────────────────────────────────

strBytes :: String -> [Word8]
strBytes = map (fromIntegral . fromEnum)

hexBytes :: String -> [Word8]
hexBytes []           = []
hexBytes (a : b : cs) = fromIntegral (digitToInt a * 16 + digitToInt b) : hexBytes cs
hexBytes _            = error "odd-length hex string"

bytesHex :: [Word8] -> String
bytesHex = concatMap byteHex
  where
    byteHex w = [ intToDigit (fromIntegral w `div` 16)
                , intToDigit (fromIntegral w `mod` 16) ]

hex16 :: Word16 -> String
hex16 w = showHex w ""

makeW16 :: Word8 -> Word8 -> Word16
makeW16 h l = fromIntegral h `shiftL` 8 .|. fromIntegral l

oneBit :: Int -> [Word8]
oneBit bit = [ if i == bit `div` 8 then 1 `shiftL` (7 - bit `mod` 8) else 0
             | i <- [0 .. frameSize - 1] ]

rawValid :: [Word8] -> Bool
rawValid bytes =
     length bytes == frameSize
  && head bytes == syncByte
  && crc16ccitt (take 15 bytes) == makeW16 (bytes !! 15) (bytes !! 16)
