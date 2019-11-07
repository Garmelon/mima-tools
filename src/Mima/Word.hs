{-# LANGUAGE OverloadedStrings #-}

module Mima.Word
  ( topBit
  -- * 24-bit value
  , MimaWord
  -- ** Formatting
  , wordToHex
  , wordToDec
  , wordToHexDec
  -- ** Converting
  , bytesToWord
  , wordToBytes
  , boolToWord
  -- ** Properties
  , getSmallOpcode
  , getLargeOpcode
  , getAddress
  , getLongValue
  , getShortValue
  -- ** Operations
  , addWords
  -- * 20-bit value
  , LongValue
  , MimaAddress
  -- ** Formatting
  , longValueToHex
  , longValueToDec
  , longValueToHexDec
  -- ** Converting
  , bytesToLongValue
  , longValueToBytes
  , longValueToWord
  -- * 16-bit value
  , ShortValue
  -- ** Formatting
  , shortValueToHex
  , shortValueToDec
  , shortValueToHexDec
  -- ** Converting
  , signedShortValueToWord
  ) where

import           Data.Bits
import qualified Data.Text as T
import           Data.Word

import           Mima.Util

{- Type classes and instances for free! -}
-- Get them now while they're hot!

-- This typeclass is for automatic bit twiddling and enumification for
-- 'Word32' based types.
class Word32Based t where
  usedBits :: t -> Int
  fromWord32 :: Word32 -> t
  toWord32 :: t -> Word32

topBit :: (Word32Based t) => t -> Bool
topBit t = testBit (toWord32 t) (usedBits t - 1)

-- Required to make the compiler shut up (see
-- https://stackoverflow.com/a/17866970)
newtype WB t = WB { unWB :: t}

instance (Show t) => Show (WB t) where
  show = show . unWB

-- Kinda obvious, isn't it? :P
instance (Word32Based t) => Word32Based (WB t) where
  usedBits = usedBits . unWB
  fromWord32 = WB . fromWord32
  toWord32 = toWord32 . unWB

instance (Word32Based t) => Eq (WB t) where
  w1 == w2 = toWord32 (unWB w1) == toWord32 (unWB w2)

instance (Word32Based t) => Bits (WB t) where
  t1 .&.   t2 = fromWord32 $ toWord32 t1 .&.   toWord32 t2
  t1 .|.   t2 = fromWord32 $ toWord32 t1 .|.   toWord32 t2
  t1 `xor` t2 = fromWord32 $ toWord32 t1 `xor` toWord32 t2
  complement = fromWord32 . complement . toWord32

  shiftL t i = fromWord32 $ shiftL (toWord32 t) i
  shiftR t i = fromWord32 $
    let rightShifted = shiftR (toWord32 t) i
        leftOver = max 0 (usedBits t - i)
    in  if topBit t
        then shiftL (complement zeroBits) leftOver .|. rightShifted
        else rightShifted

  rotateL t i = rotateR t (usedBits t - i)
  rotateR t i =
    let i' = i `mod` usedBits t
        w  = toWord32 t
    in  fromWord32 $ shiftR w i' .|. shiftL w (usedBits t - i')

  zeroBits = fromWord32 zeroBits
  bit = fromWord32 . bit
  testBit t = testBit (toWord32 t)
  bitSize = usedBits
  bitSizeMaybe = Just . usedBits
  isSigned = const True
  popCount = popCount . toWord32

instance (Word32Based t) => Bounded (WB t) where
  minBound = fromWord32 zeroBits
  maxBound = fromWord32 (complement zeroBits)

instance (Word32Based t) => Enum (WB t) where
  toEnum i =
    let lower = fromEnum $ toWord32 (minBound :: MimaAddress)
        upper = fromEnum $ toWord32 (maxBound :: MimaAddress)
    in  if lower <= i && i <= upper
        then fromWord32 $ toEnum i
        else error $ "Enum.toEnum: tag (" ++ show i
             ++ ") is out of bounds " ++ show (lower, upper)

  fromEnum = fromEnum . toWord32

  -- See 'Enum' laws for types with a 'Bounded' instance
  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise                = minBound

{- The types -}

type MimaWord = WB MimaWord_
newtype MimaWord_ = MimaWord_ Word32

instance Word32Based MimaWord_ where
  usedBits _ = 24
  fromWord32 w = MimaWord_ $ w .&. 0xFFFFFF
  toWord32 (MimaWord_ w) = w

instance Show MimaWord_ where
  show mw = T.unpack $ "MimaWord_ 0x" <> toHex 6 (toWord32 mw)

wordToHex :: MimaWord -> T.Text
wordToHex = toHex 6 . toWord32

wordToDec :: MimaWord -> T.Text
wordToDec = toDec 8 . toWord32

wordToHexDec :: MimaWord -> T.Text
wordToHexDec mw = wordToHex mw <> " (" <> wordToDec mw <> ")"

bytesToWord :: Word8 -> Word8 -> Word8 -> MimaWord
bytesToWord w1 w2 w3 =
  let (w1', w2', w3') = (fromIntegral w1, fromIntegral w2, fromIntegral w3)
  in  fromWord32 $ shiftL w1' 16 .|. shiftL w2' 8 .|. w3'

wordToBytes :: MimaWord -> (Word8, Word8, Word8)
wordToBytes mw =
  let w = toWord32 mw
      -- Mask for w1 not strictly necessary, since upper bytes are
      -- already zero due to implementation of 'fromWord32'.
      w1 = fromIntegral $ shiftR w 16 .&. 0xFF 
      w2 = fromIntegral $ shiftR w 8  .&. 0xFF
      w3 = fromIntegral $          w  .&. 0xFF
  in  (w1, w2, w3)

boolToWord :: Bool -> MimaWord
boolToWord False = zeroBits
boolToWord True = complement zeroBits

getSmallOpcode :: MimaWord -> Word32
getSmallOpcode mw = shiftR (toWord32 mw) 20 .&. 0xF

getLargeOpcode :: MimaWord -> Word32
getLargeOpcode mw = shiftR (toWord32 mw) 16 .&. 0xF

getAddress :: MimaWord -> MimaAddress
getAddress = getLongValue

getLongValue :: MimaWord -> LongValue
getLongValue = fromWord32 . toWord32

getShortValue :: MimaWord -> ShortValue
getShortValue = fromWord32 . toWord32

addWords :: MimaWord -> MimaWord -> MimaWord
addWords w1 w2 = fromWord32 $ toWord32 w1 + toWord32 w2

type MimaAddress = LongValue
type LongValue = WB LongValue_
newtype LongValue_ = LongValue_ Word32

instance Word32Based LongValue_ where
  usedBits _ = 20
  fromWord32 w = LongValue_ $ w .&. 0xFFFFF
  toWord32 (LongValue_ w) = w

instance Show LongValue_ where
  show lv = T.unpack $ "LongValue_ 0x" <> toHex 5 (toWord32 lv)

longValueToHex :: MimaWord -> T.Text
longValueToHex = toHex 5 . toWord32

longValueToDec :: MimaWord -> T.Text
longValueToDec = toDec 7 . toWord32

longValueToHexDec :: MimaWord -> T.Text
longValueToHexDec mw = longValueToHex mw <> " (" <> longValueToDec mw <> ")"

bytesToLongValue :: Word8 -> Word8 -> Word8 -> LongValue
bytesToLongValue w1 w2 w3 = getAddress $ bytesToWord w1 w2 w3

longValueToBytes :: LongValue -> (Word8, Word8, Word8)
longValueToBytes = wordToBytes . longValueToWord

longValueToWord :: LongValue -> MimaWord
longValueToWord = fromWord32 . toWord32

type ShortValue = WB ShortValue_
newtype ShortValue_ = ShortValue_ Word32

instance Word32Based ShortValue_ where
  usedBits _ = 16
  fromWord32 w = ShortValue_ $ w .&. 0xFFFF
  toWord32 (ShortValue_ w) = w

instance Show ShortValue_ where
  show lv = T.unpack $ "ShortValue_ 0x" <> toHex 4 (toWord32 lv)

shortValueToHex :: MimaWord -> T.Text
shortValueToHex = toHex 4 . toWord32

shortValueToDec :: MimaWord -> T.Text
shortValueToDec = toDec 5 . toWord32

shortValueToHexDec :: MimaWord -> T.Text
shortValueToHexDec mw = shortValueToHex mw <> " (" <> shortValueToDec mw <> ")"

signedShortValueToWord :: ShortValue -> MimaWord
signedShortValueToWord sv
  | topBit sv = fromWord32 $ 0xFFFF0000 .|. toWord32 sv
  | otherwise = fromWord32 $ toWord32 sv
