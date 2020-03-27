{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mima.Vm.Word
  (
  -- * Types
    MimaWord
  , MimaAddress
  , LargeValue
  , SmallValue
  , Opcode
  , topBit
  -- * Converting between types
  , bytesToWord
  , wordToBytes
  , boolToWord
  , largeValueToWord
  , signedLargeValueToWord
  , signedSmallValueToLargeValue
  , wordFromSmallOpcode
  , wordFromLargeOpcode
  -- ** 'MimaWord' properties
  , getSmallOpcode
  , getLargeOpcode
  , getLargeValue
  , getSmallValue
  ) where

import           Data.Bits
import           Data.Word
import           Data.Word.Odd

-- | The MiMa operates on words with a width of 24 bits.
newtype MimaWord = MimaWord Word24
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, Bits, FiniteBits)

-- | MiMa adresses have a width of 20 bits.
type MimaAddress = LargeValue

-- | A large value is the argument to a small opcode (4 bits). It has a width of
-- 20 bits.
newtype LargeValue = LargeValue Word20
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, Bits, FiniteBits)

-- | A small value is the argument to a large opcode (8 bits). It has a width of
-- 16 bits.
newtype SmallValue = SmallValue Word16
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, Bits, FiniteBits)

-- | A small opcode has a width of 4 bits. In the case of a large opcode with a
-- width of 8 bits, the most significant 4 bits are always 0xF, so large opcodes
-- can be represented by this data type too.
newtype Opcode = Opcode Word4
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, Bits, FiniteBits)

-- | Return the most significant bit of the input value.
topBit :: (FiniteBits b) => b -> Bool
topBit b = testBit b $ finiteBitSize b - 1

-- | Combine three bytes into a 'MimaWord'. The bytes are in big-endian order
-- (starting with the most significant byte).
bytesToWord :: (Word8, Word8, Word8) -> MimaWord
bytesToWord (w1, w2, w3) =
  let (w1', w2', w3') = (fromIntegral w1, fromIntegral w2, fromIntegral w3)
  in  shiftL w1' 16 .|. shiftL w2' 8 .|. w3'

-- | Split up a 'MimaWord' into its component bytes. The bytes are in big-endian
-- order (starting with the most significant byte).
wordToBytes :: MimaWord -> (Word8, Word8, Word8)
wordToBytes mw =
  -- No masks necessary since converting to 'Word8' already cuts off
  -- all higher bits.
  let w1 = fromIntegral $ shiftR mw 16
      w2 = fromIntegral $ shiftR mw 8
      w3 = fromIntegral mw
  in  (w1, w2, w3)

-- | This function behaves as if the boolean was the result of an equality check
-- for the @EQL@ command: 'False' is represented by only zeroes while 'True' is
-- represented by only ones.
boolToWord :: Bool -> MimaWord
boolToWord False = zeroBits
boolToWord True  = complement zeroBits

-- | Fills in the four most significant bits with zeroes.
largeValueToWord :: LargeValue -> MimaWord
largeValueToWord = fromIntegral

-- | Performs sign expansion.
signedLargeValueToWord :: LargeValue -> MimaWord
signedLargeValueToWord lv
  | topBit lv = 0xF00000 .|. fromIntegral lv
  | otherwise = fromIntegral lv

-- | Performs sign expansion.
signedSmallValueToLargeValue :: SmallValue -> LargeValue
signedSmallValueToLargeValue sv
  | topBit sv = 0xF0000 .|. fromIntegral sv
  | otherwise = fromIntegral sv

-- | Combine a small opcode and a large value into a 'MimaWord'.
wordFromSmallOpcode :: Opcode -> LargeValue -> MimaWord
wordFromSmallOpcode so lv = shiftL (fromIntegral so) 20 .|. fromIntegral lv

-- | Combine a large opcode and a small value into a 'MimaWord'. The four most
-- significant bits are set to ones.
wordFromLargeOpcode :: Opcode -> SmallValue -> MimaWord
wordFromLargeOpcode lo sv = 0xF00000 .|. shiftL (fromIntegral lo) 16 .|. fromIntegral sv

-- | Returns the four most significant bits of the word.
getSmallOpcode :: MimaWord -> Opcode
getSmallOpcode mw = fromIntegral $ shiftR mw 20

-- | Returns the fifth to eigth most significant bits of the word. The four most
-- significant bits of the word should all be ones.
getLargeOpcode :: MimaWord -> Opcode
getLargeOpcode mw = fromIntegral $ shiftR mw 16

-- | A word's 20 least significant bits.
getLargeValue :: MimaWord -> LargeValue
getLargeValue = fromIntegral

-- | A word's 16 least significant bits.
getSmallValue :: MimaWord -> SmallValue
getSmallValue = fromIntegral
