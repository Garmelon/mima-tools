{-# LANGUAGE OverloadedStrings #-}

module Mima.Vm.Instruction
  ( SmallOpcode(..)
  , LargeOpcode(..)
  , argumentIsOptional
  , Instruction(..)
  , wordToInstruction
  , instructionToWord
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import           Mima.Format
import           Mima.Vm.Word

data SmallOpcode = LDC | LDV | STV | ADD | AND | OR | XOR | EQL
                 | JMP | JMN | LDIV | STIV | CALL | ADC
  deriving (Show, Eq, Ord, Bounded, Enum)

instance ToText SmallOpcode where
  toText = T.pack . show

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
smallOpcodeNr ADC  = 13

smallOpcodeMap :: Map.Map Opcode SmallOpcode
smallOpcodeMap = Map.fromList [(smallOpcodeNr so, so) | so <- [minBound..maxBound]]

data LargeOpcode = HALT | NOT | RAR | RET | LDRA | STRA | LDSP | STSP
                 | LDFP | STFP | LDRS | STRS | LDRF | STRF
  deriving (Show, Eq, Ord, Bounded, Enum)

instance ToText LargeOpcode where
  toText = T.pack . show

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
largeOpcodeNr LDRS = 10
largeOpcodeNr STRS = 11
largeOpcodeNr LDRF = 12
largeOpcodeNr STRF = 13

largeOpcodeMap :: Map.Map Opcode LargeOpcode
largeOpcodeMap = Map.fromList [(largeOpcodeNr lo, lo) | lo <- [minBound..maxBound]]

argumentIsOptional :: LargeOpcode -> Bool
argumentIsOptional HALT = True
argumentIsOptional NOT  = True
argumentIsOptional RAR  = True
argumentIsOptional RET  = True
argumentIsOptional LDRA = True
argumentIsOptional STRA = True
argumentIsOptional LDSP = True
argumentIsOptional STSP = True
argumentIsOptional LDFP = True
argumentIsOptional STFP = True
argumentIsOptional LDRS = False
argumentIsOptional STRS = False
argumentIsOptional LDRF = False
argumentIsOptional STRF = False

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

parseSmallOpcode :: Opcode -> Either T.Text SmallOpcode
parseSmallOpcode w = case smallOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown small opcode " <> toDec w <> " (" <> fixWidthHex 1 (toHex w)
                     <> ", " <> fixWidthBin 4 (toBin w) <> ")"

parseLargeInstruction :: MimaWord -> Either T.Text Instruction
parseLargeInstruction mw = do
  lo <- parseLargeOpcode (getLargeOpcode mw)
  pure $ LargeInstruction lo (getSmallValue mw)

parseLargeOpcode :: Opcode -> Either T.Text LargeOpcode
parseLargeOpcode w = case largeOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown large opcode " <> toDec w <> " (" <> fixWidthHex 1 (toHex w)
                     <> ", " <> fixWidthBin 4 (toBin w) <> ")"

instructionToWord :: Instruction -> MimaWord
instructionToWord (SmallInstruction so lv) = wordFromSmallOpcode (smallOpcodeNr so) lv
instructionToWord (LargeInstruction lo sv) = wordFromLargeOpcode (largeOpcodeNr lo) sv
