{-# LANGUAGE OverloadedStrings #-}

module Mima.Word
  ( MimaWord
  , bytesToWord
  , wordToBytes
  , boolToWord
  , topBit
  , upperOpcode
  , lowerOpcode
  , address
  , addWords
  , MimaAddress
  , addressToWord
  ) where

import           Data.Bits
import qualified Data.Text as T
import           Data.Word

import           Mima.Util

class Word32Based t where
  fromWord32 :: Word32 -> t
  toWord32 :: t -> Word32

-- The MiMa's words are 24 bits long. The smallest word they fit in is
-- 'Word32'.

-- TODO Maybe store in the upper 24 bits of a signed type?

newtype MimaWord = MimaWord Word32
  deriving (Eq)

wordSize :: Int
wordSize = 24

instance ToText MimaWord where
  toText (MimaWord w) = toHex 6 w

instance Show MimaWord where
  show mw = T.unpack $ "MimaWord 0x" <> toText mw

instance Word32Based MimaWord where
  fromWord32 w = MimaWord $ w .&. 0x00FFFFFF
  toWord32 (MimaWord w) = w

instance Bits MimaWord where
  mw1 .&.   mw2 = fromWord32 $ toWord32 mw1 .&.   toWord32 mw2
  mw1 .|.   mw2 = fromWord32 $ toWord32 mw1 .|.   toWord32 mw2
  mw1 `xor` mw2 = fromWord32 $ toWord32 mw1 `xor` toWord32 mw2
  complement = fromWord32 . complement . toWord32

  shiftR mw i = fromWord32 $
    let rightShifted = shiftR (toWord32 mw) i
        leftOver = max 0 (wordSize - i)
    in  if topBit mw
        then shiftL 0xFFFFFFFF leftOver .|. rightShifted
        else rightShifted

  shiftL mw i = fromWord32 $ shiftL (toWord32 mw) i

  rotateR mw i =
    let i' = i `mod` wordSize
        w        = toWord32 mw
    in  fromWord32 $ shiftR w i' .|. shiftL w (wordSize - i')

  rotateL mw i = rotateR mw (wordSize - i)

  zeroBits = fromWord32 zeroBits
  bit = fromWord32 . bit
  testBit mw i = testBit (toWord32 mw) i
  bitSize = const wordSize
  bitSizeMaybe = const (Just wordSize)
  isSigned = const True
  popCount = popCount . toWord32

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
boolToWord True  = complement zeroBits

topBit :: MimaWord -> Bool
topBit mw = testBit (toWord32 mw) (wordSize - 1)

upperOpcode :: MimaWord -> Word32
upperOpcode mw = shiftR (toWord32 mw) 20 .&. 0xF

lowerOpcode :: MimaWord -> Word32
lowerOpcode mw = shiftR (toWord32 mw) 16 .&. 0xF

address :: MimaWord -> MimaAddress
address = fromWord32 . toWord32

addWords :: MimaWord -> MimaWord -> MimaWord
addWords mw1 mw2 = fromWord32 $ toWord32 mw1 + toWord32 mw2

-- The MiMa's addresses are 20 bits long. The smallest word they fit
-- in is 'Word32'.

newtype MimaAddress = MimaAddress Word32
  deriving (Eq, Ord)

instance ToText MimaAddress where
  toText (MimaAddress w) = toHex 5 w

instance Show MimaAddress where
  show ma = T.unpack $ "MimaAddress 0x" <> toText ma

instance Word32Based MimaAddress where
  fromWord32 w = MimaAddress $ w .&. 0x000FFFFF
  toWord32 (MimaAddress w) = w

instance Bounded MimaAddress where
  minBound = fromWord32 0x00000
  maxBound = fromWord32 0xFFFFF

-- TODO satisfy enum laws with regards to bounded instance
instance Enum MimaAddress where
  toEnum i =
    let lower = fromEnum $ toWord32 (minBound :: MimaAddress)
        upper = fromEnum $ toWord32 (maxBound :: MimaAddress)
    in  if lower <= i && i <= upper
        then fromWord32 $ toEnum i
        else error $ "Enum.toEnum{MimaAddress}: tag (" ++ show i
                     ++ ") is out of bounds " ++ show (lower, upper)
  fromEnum = fromEnum . toWord32

addressToWord :: MimaAddress -> MimaWord
addressToWord = fromWord32 . toWord32
