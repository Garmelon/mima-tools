module Mima.Word
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
  , signedSmallValueToWord
  -- ** 'MimaWord' properties
  , getSmallOpcode
  , getLargeOpcode
  , getLargeValue
  , getSmallValue
  ) where

import           Data.Bits
import           Data.Word
import           Data.Word.Odd

type MimaWord    = Word24
type MimaAddress = LargeValue
type LargeValue  = Word20
type SmallValue  = Word16
type Opcode      = Word4

topBit :: (FiniteBits b) => b -> Bool
topBit b = testBit b $ finiteBitSize b - 1

bytesToWord :: (Word8, Word8, Word8) -> MimaWord
bytesToWord (w1, w2, w3) =
  let (w1', w2', w3') = (fromIntegral w1, fromIntegral w2, fromIntegral w3)
  in  shiftL w1' 16 .|. shiftL w2' 8 .|. w3'

wordToBytes :: MimaWord -> (Word8, Word8, Word8)
wordToBytes mw =
  -- No masks necessary since converting to 'Word8' already cuts off
  -- all higher bits.
  let w1 = fromIntegral $ shiftR mw 16
      w2 = fromIntegral $ shiftR mw 8
      w3 = fromIntegral mw
  in  (w1, w2, w3)

boolToWord :: Bool -> MimaWord
boolToWord False = zeroBits
boolToWord True = complement zeroBits

largeValueToWord :: LargeValue -> MimaWord
largeValueToWord = fromIntegral

signedSmallValueToWord :: SmallValue -> MimaWord
signedSmallValueToWord sv
  | topBit sv = 0xFF0000 .|. fromIntegral sv
  | otherwise = fromIntegral sv

getSmallOpcode :: MimaWord -> Opcode
getSmallOpcode mw = fromIntegral $ shiftR mw 20

getLargeOpcode :: MimaWord -> Opcode
getLargeOpcode mw = fromIntegral $ shiftR mw 16

getLargeValue :: MimaWord -> LargeValue
getLargeValue = fromIntegral

getSmallValue :: MimaWord -> SmallValue
getSmallValue = fromIntegral
