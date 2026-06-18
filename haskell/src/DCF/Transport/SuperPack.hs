-- SPDX-License-Identifier: LGPL-3.0-only

-- | DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
-- DeModFrame quanta under a single joint CRC-16.
--
-- Two raw frames cost 2*17 = 34 bytes. When frames are emitted in pairs the
-- second header is largely recoverable from context (both inner sync bytes are
-- 0xD3; each inner CRC is a pure function of its own 15 leading bytes). SuperPack
-- drops those 6 redundant bytes and spends 4 back on one outer sync, a
-- type\/version tag, and ONE joint CRC over the whole container — net 34 -> 32
-- bytes plus a strictly stronger integrity check.
--
-- Why it is the lower-latency option: a SuperPack puts a frame pair on the wire
-- as a single datagram instead of two — one packet, one IP\/UDP header, one
-- syscall — so paired traffic crosses the network with strictly lower per-pair
-- overhead and latency than emitting the two frames separately.
--
-- Unpack reconstructs each inner frame bit-exact, so the outputs are ordinary
-- valid DeModFrames and the 246-vector wire certificate is untouched.
module DCF.Transport.SuperPack
  ( superType
  , superLen
  , superCoreLen
  , superSflags
  , packSuper
  , unpackSuper
  , isSuperPack
  ) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import           Data.Word             (Word16, Word8)

import           DCF.Transport.FrameSpec (crc16ccitt, decodeFrame, syncByte)

superVersion :: Word8
superVersion = 1

superType :: Word8
superType = 0x05

superLen :: Int
superLen = 32

superCoreLen :: Int
superCoreLen = 14

-- | sflags byte = version nibble (1) in the high nibble, SUPER type in the low.
superSflags :: Word8
superSflags = (superVersion `shiftL` 4) .|. superType

hi, lo :: Word16 -> Word8
hi w = fromIntegral (w `shiftR` 8)
lo w = fromIntegral (w .&. 0xFF)

w16 :: Word8 -> Word8 -> Word16
w16 h l = fromIntegral h `shiftL` 8 .|. fromIntegral l

-- | The 14 reconstructable bytes of a frame, validating sync, version, inner CRC.
frameCore :: [Word8] -> Either String [Word8]
frameCore f
  | length f /= 17               = Left "need a 17-byte frame"
  | head f /= syncByte           = Left "bad sync byte"
  | (f !! 1) `shiftR` 4 /= superVersion = Left "bad version nibble"
  | crc16ccitt (take 15 f) /= w16 (f !! 15) (f !! 16) = Left "inner frame CRC mismatch"
  | otherwise                    = Right (take superCoreLen (drop 1 f))

-- | Rebuild a full 17-byte frame from its 14-byte core (sync + recomputed CRC).
rebuildFrame :: [Word8] -> [Word8]
rebuildFrame core = body ++ [hi crc, lo crc]
  where
    body = syncByte : core
    crc  = crc16ccitt body

-- | Combine two valid 17-byte frames into one 32-byte SuperPack.
packSuper :: [Word8] -> [Word8] -> Either String [Word8]
packSuper a b = do
  ca <- frameCore a
  cb <- frameCore b
  let header = [syncByte, superSflags] ++ ca ++ cb
      crc    = crc16ccitt header
  Right (header ++ [hi crc, lo crc])

-- | True iff buf looks like a SuperPack (length + sync + version/type tag).
isSuperPack :: [Word8] -> Bool
isSuperPack buf = length buf == superLen
               && head buf == syncByte
               && (buf !! 1) == superSflags

-- | Split a 32-byte SuperPack into (frameA, frameB), each a bit-exact frame.
unpackSuper :: [Word8] -> Either String ([Word8], [Word8])
unpackSuper buf
  | length buf /= superLen                = Left "length /= 32"
  | head buf /= syncByte                  = Left "bad sync byte"
  | (buf !! 1) `shiftR` 4 /= superVersion = Left "bad version nibble"
  | (buf !! 1) .&. 0x0F /= superType      = Left "not a SuperPack type"
  | crc16ccitt (take 30 buf) /= w16 (buf !! 30) (buf !! 31) = Left "SuperPack CRC mismatch"
  | otherwise =
      let a = rebuildFrame (take superCoreLen (drop 2 buf))
          b = rebuildFrame (take superCoreLen (drop (2 + superCoreLen) buf))
      in case (decodeFrame a, decodeFrame b) of
           (Right _, Right _) -> Right (a, b)
           _                  -> Left "inner frame failed to decode"
