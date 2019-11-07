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
  , getLargeValue
  , getSmallValue
  -- ** Operations
  , addWords
  -- * 20-bit value
  , LargeValue
  , MimaAddress
  -- ** Formatting
  , largeValueToHex
  , largeValueToDec
  , largeValueToHexDec
  -- ** Converting
  , bytesToLargeValue
  , largeValueToBytes
  , largeValueToWord
  -- * 16-bit value
  , SmallValue
  -- ** Formatting
  , smallValueToHex
  , smallValueToDec
  , smallValueToHexDec
  -- ** Converting
  , signedSmallValueToWord
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
getAddress = getLargeValue

getLargeValue :: MimaWord -> LargeValue
getLargeValue = fromWord32 . toWord32

getSmallValue :: MimaWord -> SmallValue
getSmallValue = fromWord32 . toWord32

addWords :: MimaWord -> MimaWord -> MimaWord
addWords w1 w2 = fromWord32 $ toWord32 w1 + toWord32 w2

type MimaAddress = LargeValue
type LargeValue = WB LargeValue_
newtype LargeValue_ = LargeValue_ Word32

instance Word32Based LargeValue_ where
  usedBits _ = 20
  fromWord32 w = LargeValue_ $ w .&. 0xFFFFF
  toWord32 (LargeValue_ w) = w

instance Show LargeValue_ where
  show lv = T.unpack $ "LargeValue_ 0x" <> toHex 5 (toWord32 lv)

largeValueToHex :: MimaWord -> T.Text
largeValueToHex = toHex 5 . toWord32

largeValueToDec :: MimaWord -> T.Text
largeValueToDec = toDec 7 . toWord32

largeValueToHexDec :: MimaWord -> T.Text
largeValueToHexDec mw = largeValueToHex mw <> " (" <> largeValueToDec mw <> ")"

bytesToLargeValue :: Word8 -> Word8 -> Word8 -> LargeValue
bytesToLargeValue w1 w2 w3 = getAddress $ bytesToWord w1 w2 w3

largeValueToBytes :: LargeValue -> (Word8, Word8, Word8)
largeValueToBytes = wordToBytes . largeValueToWord

largeValueToWord :: LargeValue -> MimaWord
largeValueToWord = fromWord32 . toWord32

type SmallValue = WB SmallValue_
newtype SmallValue_ = SmallValue_ Word32

instance Word32Based SmallValue_ where
  usedBits _ = 16
  fromWord32 w = SmallValue_ $ w .&. 0xFFFF
  toWord32 (SmallValue_ w) = w

instance Show SmallValue_ where
  show lv = T.unpack $ "SmallValue_ 0x" <> toHex 4 (toWord32 lv)

smallValueToHex :: MimaWord -> T.Text
smallValueToHex = toHex 4 . toWord32

smallValueToDec :: MimaWord -> T.Text
smallValueToDec = toDec 5 . toWord32

smallValueToHexDec :: MimaWord -> T.Text
smallValueToHexDec mw = smallValueToHex mw <> " (" <> smallValueToDec mw <> ")"

signedSmallValueToWord :: SmallValue -> MimaWord
signedSmallValueToWord sv
  | topBit sv = fromWord32 $ 0xFFFF0000 .|. toWord32 sv
  | otherwise = fromWord32 $ toWord32 sv
