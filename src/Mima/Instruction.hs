{-# LANGUAGE OverloadedStrings #-}

module Mima.Instruction
  ( SmallOpcode(..)
  , LargeOpcode(..)
  , Instruction(..)
  , wordToInstruction
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Word

import           Mima.Util
import           Mima.Word

data SmallOpcode = LDC | LDV | STV | ADD | AND | OR | XOR | EQL | JMP | JMN | STIV | LDIV
  deriving (Show, Eq, Ord)

instance ToText SmallOpcode where
  toText = T.pack . show

allSmallOpcodes :: [SmallOpcode]
allSmallOpcodes = [LDC, LDV, STV, ADD, AND, OR, XOR, EQL, JMP, JMN, STIV, LDIV]

getSmallOpcode :: SmallOpcode -> Word32
getSmallOpcode LDC = 0
getSmallOpcode LDV = 1
getSmallOpcode STV = 2
getSmallOpcode ADD = 3
getSmallOpcode AND = 4
getSmallOpcode OR  = 5
getSmallOpcode XOR = 6
getSmallOpcode EQL = 7
getSmallOpcode JMP = 8
getSmallOpcode JMN = 9
getSmallOpcode STIV = 10
getSmallOpcode LDIV = 11

smallOpcodeMap :: Map.Map Word32 SmallOpcode
smallOpcodeMap = Map.fromList [(getSmallOpcode oc, oc) | oc <- allSmallOpcodes]

data LargeOpcode = HALT | NOT | RAR
  deriving (Show, Eq, Ord)

instance ToText LargeOpcode where
  toText = T.pack . show

allLargeOpcodes :: [LargeOpcode]
allLargeOpcodes = [HALT, NOT, RAR]

getLargeOpcode :: LargeOpcode -> Word32
getLargeOpcode HALT = 0
getLargeOpcode NOT  = 1
getLargeOpcode RAR  = 2

largeOpcodeMap :: Map.Map Word32 LargeOpcode
largeOpcodeMap = Map.fromList [(getLargeOpcode oc, oc) | oc <- allLargeOpcodes]

data Instruction
  = SmallInstruction !SmallOpcode !MimaAddress
  | LargeInstruction !LargeOpcode
  deriving (Show, Eq)

instance ToText Instruction where
  toText (SmallInstruction oc addr) = T.justifyLeft 4 ' ' (toText oc) <> " " <> addrToDec addr
  toText (LargeInstruction oc)      = toText oc


wordToInstruction :: MimaWord -> Either T.Text Instruction
wordToInstruction mw = if upperOpcode mw == 0xF
                       then parseLargeInstruction mw
                       else parseSmallInstruction mw

parseSmallInstruction :: MimaWord -> Either T.Text Instruction
parseSmallInstruction mw = do
  oc <- parseSmallOpcode (upperOpcode mw)
  pure $ SmallInstruction oc (address mw)

-- Assumes that all bits not part of the opcode are zeroed. The opcode
-- uses the lowest four bits.
parseSmallOpcode :: Word32 -> Either T.Text SmallOpcode
parseSmallOpcode w = case smallOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown small opcode " <> T.pack (show w)
                     <> " (" <> toHex 2 w <> ")"

parseLargeInstruction :: MimaWord -> Either T.Text Instruction
parseLargeInstruction mw = LargeInstruction <$> parseLargeOpcode (lowerOpcode mw)

-- Assumes that all bits not part of the opcode are zeroed. The opcode
-- uses the lowest four bits.
parseLargeOpcode :: Word32 -> Either T.Text LargeOpcode
parseLargeOpcode w = case largeOpcodeMap Map.!? w of
  Just oc -> pure oc
  Nothing -> Left $ "Unknown large opcode " <> T.pack (show w)
                     <> " (" <> toHex 2 w <> ")"
