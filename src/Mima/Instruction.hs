{-# LANGUAGE OverloadedStrings #-}

module Mima.Instruction
  ( SmallOpcode(..)
  , LargeOpcode(..)
  , Instruction(..)
  , wordToInstruction
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Word

import           Mima.Util
import           Mima.Word

data SmallOpcode = LDC | LDV | STV | ADD | AND | OR | XOR | EQL
                 | JMP | JMN | LDIV | STIV | CALL | LDVR | STVR
  deriving (Show, Eq, Ord)

instance ToText SmallOpcode where
  toText = T.pack . show

allSmallOpcodes :: [SmallOpcode]
allSmallOpcodes = [LDC, LDV, STV, ADD, AND, OR, XOR, EQL,
                   JMP, JMN, LDIV, STIV, CALL, LDVR, STVR]

smallOpcodeToWord32 :: SmallOpcode -> Word32
smallOpcodeToWord32 LDC  =  0
smallOpcodeToWord32 LDV  =  1
smallOpcodeToWord32 STV  =  2
smallOpcodeToWord32 ADD  =  3
smallOpcodeToWord32 AND  =  4
smallOpcodeToWord32 OR   =  5
smallOpcodeToWord32 XOR  =  6
smallOpcodeToWord32 EQL  =  7
smallOpcodeToWord32 JMP  =  8
smallOpcodeToWord32 JMN  =  9
smallOpcodeToWord32 LDIV = 10
smallOpcodeToWord32 STIV = 11
smallOpcodeToWord32 CALL = 12
smallOpcodeToWord32 LDVR = 13
smallOpcodeToWord32 STVR = 14

smallOpcodeMap :: Map.Map Word32 SmallOpcode
smallOpcodeMap = Map.fromList [(smallOpcodeToWord32 so, so) | so <- allSmallOpcodes]

data LargeOpcode = HALT | NOT | RAR | RET | LDRA | STRA
                 | LDSP | STSP | LDFP | STFP | ADC
  deriving (Show, Eq, Ord)

instance ToText LargeOpcode where
  toText = T.pack . show

allLargeOpcodes :: [LargeOpcode]
allLargeOpcodes = [HALT, NOT, RAR, RET, LDRA, STRA, LDSP, STSP, LDFP, STFP, ADC]

largeOpcodeToWord32 :: LargeOpcode -> Word32
largeOpcodeToWord32 HALT =  0
largeOpcodeToWord32 NOT  =  1
largeOpcodeToWord32 RAR  =  2
largeOpcodeToWord32 RET  =  3

largeOpcodeToWord32 LDRA =  4
largeOpcodeToWord32 STRA =  5
largeOpcodeToWord32 LDSP =  6
largeOpcodeToWord32 STSP =  7
largeOpcodeToWord32 LDFP =  8
largeOpcodeToWord32 STFP =  9
largeOpcodeToWord32 ADC  = 10

largeOpcodeMap :: Map.Map Word32 LargeOpcode
largeOpcodeMap = Map.fromList [(largeOpcodeToWord32 lo, lo) | lo <- allLargeOpcodes]

data Instruction
  = SmallInstruction !SmallOpcode !LargeValue
  | LargeInstruction !LargeOpcode !SmallValue
  deriving (Show, Eq)

instance ToText Instruction where
  toText (SmallInstruction oc lv) = T.justifyLeft 4 ' ' (toText oc) <> " " <> largeValueToDec lv
  toText (LargeInstruction oc sv)
    | sv == minBound = T.justifyLeft 4 ' ' (toText oc)
    | otherwise      = T.justifyLeft 4 ' ' (toText oc) <> " " <> smallValueToDec sv

wordToInstruction :: MimaWord -> Either T.Text Instruction
wordToInstruction mw = if getLargeOpcode mw == 0xF
                       then parseLargeInstruction mw
                       else parseSmallInstruction mw

parseSmallInstruction :: MimaWord -> Either T.Text Instruction
parseSmallInstruction mw = do
  so <- parseSmallOpcode (getSmallOpcode mw)
  pure $ SmallInstruction so (getLargeValue mw)

-- Assumes that all bits not part of the opcode are zeroed. The opcode
-- uses the lowest four bits.
parseSmallOpcode :: Word32 -> Either T.Text SmallOpcode
parseSmallOpcode w = case smallOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown small opcode " <> T.pack (show w)
                     <> " (" <> toHex 2 w <> ")"

parseLargeInstruction :: MimaWord -> Either T.Text Instruction
parseLargeInstruction mw = do
  lo <- parseLargeOpcode (getLargeOpcode mw)
  pure $ LargeInstruction lo (getSmallValue mw)

-- Assumes that all bits not part of the opcode are zeroed. The opcode
-- uses the lowest four bits.
parseLargeOpcode :: Word32 -> Either T.Text LargeOpcode
parseLargeOpcode w = case largeOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown large opcode " <> T.pack (show w)
                     <> " (" <> toHex 2 w <> ")"
