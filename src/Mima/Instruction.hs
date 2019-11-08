{-# LANGUAGE OverloadedStrings #-}

module Mima.Instruction
  ( SmallOpcode(..)
  , LargeOpcode(..)
  , Instruction(..)
  , wordToInstruction
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

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

smallOpcodeNr :: SmallOpcode -> Opcode
smallOpcodeNr LDC  =  0
smallOpcodeNr LDV  =  1
smallOpcodeNr STV  =  2
smallOpcodeNr ADD  =  3
smallOpcodeNr AND  =  4
smallOpcodeNr OR   =  5
smallOpcodeNr XOR  =  6
smallOpcodeNr EQL  =  7
smallOpcodeNr JMP  =  8
smallOpcodeNr JMN  =  9
smallOpcodeNr LDIV = 10
smallOpcodeNr STIV = 11
smallOpcodeNr CALL = 12
smallOpcodeNr LDVR = 13
smallOpcodeNr STVR = 14

smallOpcodeMap :: Map.Map Opcode SmallOpcode
smallOpcodeMap = Map.fromList [(smallOpcodeNr so, so) | so <- allSmallOpcodes]

data LargeOpcode = HALT | NOT | RAR | RET | LDRA | STRA
                 | LDSP | STSP | LDFP | STFP | ADC
  deriving (Show, Eq, Ord)

instance ToText LargeOpcode where
  toText = T.pack . show

allLargeOpcodes :: [LargeOpcode]
allLargeOpcodes = [HALT, NOT, RAR, RET, LDRA, STRA, LDSP, STSP, LDFP, STFP, ADC]

largeOpcodeNr :: LargeOpcode -> Opcode
largeOpcodeNr HALT =  0
largeOpcodeNr NOT  =  1
largeOpcodeNr RAR  =  2
largeOpcodeNr RET  =  3
largeOpcodeNr LDRA =  4
largeOpcodeNr STRA =  5
largeOpcodeNr LDSP =  6
largeOpcodeNr STSP =  7
largeOpcodeNr LDFP =  8
largeOpcodeNr STFP =  9
largeOpcodeNr ADC  = 10

largeOpcodeMap :: Map.Map Opcode LargeOpcode
largeOpcodeMap = Map.fromList [(largeOpcodeNr lo, lo) | lo <- allLargeOpcodes]

data Instruction
  = SmallInstruction !SmallOpcode !LargeValue
  | LargeInstruction !LargeOpcode !SmallValue
  deriving (Show, Eq)

wordToInstruction :: MimaWord -> Either T.Text Instruction
wordToInstruction mw = if getSmallOpcode mw == 0xF
                       then parseLargeInstruction mw
                       else parseSmallInstruction mw

parseSmallInstruction :: MimaWord -> Either T.Text Instruction
parseSmallInstruction mw = do
  so <- parseSmallOpcode (getSmallOpcode mw)
  pure $ SmallInstruction so (getLargeValue mw)

-- Assumes that all bits not part of the opcode are zeroed. The opcode
-- uses the lowest four bits.
parseSmallOpcode :: Opcode -> Either T.Text SmallOpcode
parseSmallOpcode w = case smallOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown small opcode " <> T.pack (show w)
                     <> " (" <> integralToHex 1 w <> ")"

parseLargeInstruction :: MimaWord -> Either T.Text Instruction
parseLargeInstruction mw = do
  lo <- parseLargeOpcode (getLargeOpcode mw)
  pure $ LargeInstruction lo (getSmallValue mw)

-- Assumes that all bits not part of the opcode are zeroed. The opcode
-- uses the lowest four bits.
parseLargeOpcode :: Opcode -> Either T.Text LargeOpcode
parseLargeOpcode w = case largeOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown large opcode " <> T.pack (show w)
                     <> " (" <> integralToHex 1 w <> ")"
