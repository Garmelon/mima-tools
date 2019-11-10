{-# LANGUAGE OverloadedStrings #-}

module Mima.Assembler.Parser.RawInstruction
  ( RawInstruction(..)
  , rawInstruction
  , rawInstructionToWord
  ) where

import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C

import           Mima.Assembler.Parser.Basic
import           Mima.Assembler.Parser.Label
import           Mima.Instruction
import           Mima.Word

data RawInstruction addr
  = RawLIT MimaWord
  | RawSmallInstruction SmallOpcode addr
  | RawLargeInstruction LargeOpcode SmallValue
  deriving (Show)

parseByLiteral :: [(T.Text, b)] -> Parser b
parseByLiteral = foldl (<|>) empty . map (\(a, b) -> b <$ C.string' a)

smallOpcode' :: Parser SmallOpcode
smallOpcode' = parseByLiteral
  [ ( "LDC",  LDC)
  , ( "LDV",  LDV)
  , ( "STV",  STV)
  , ( "ADD",  ADD)
  , ( "AND",  AND)
  , (  "OR",   OR)
  , ( "XOR",  XOR)
  , ( "EQL",  EQL)
  , ( "JMP",  JMP)
  , ( "JMN",  JMN)
  , ("LDIV", LDIV)
  , ("STIV", STIV)
  , ("CALL", CALL)
  , ("LDVR", LDVR)
  , ("STVR", STVR)
  ]

largeOpcode' :: Parser LargeOpcode
largeOpcode' = parseByLiteral [( "ADC",  ADC)]

largeOptionalOpcode' :: Parser LargeOpcode
largeOptionalOpcode' = parseByLiteral
  [ ("HALT", HALT)
  , ( "NOT",  NOT)
  , ( "RAR",  RAR)
  , ( "RET",  RET)
  , ("LDRA", LDRA)
  , ("STRA", STRA)
  , ("LDSP", LDSP)
  , ("STSP", STSP)
  , ("LDFP", LDFP)
  , ("STFP", STFP)
  ]

rawInstruction :: Parser (RawInstruction Address)
rawInstruction = label "instruction" $
      (RawLIT <$> (C.string' "LIT" *> instr mimaWord))
  <|> (RawSmallInstruction <$> smallOpcode' <*> instr address)
  <|> (RawLargeInstruction <$> largeOpcode' <*> instr smallValue)
  <|> (RawLargeInstruction <$> largeOptionalOpcode' <*> instr' smallValue)
  where
    -- These assume that the parser is a lexeme
    instr  parser = lexeme whitespace *> parser
    instr' parser = try (instr parser) <|> lexeme (pure 0)

rawInstructionToWord :: RawInstruction MimaAddress -> MimaWord
rawInstructionToWord (RawLIT word) = word
rawInstructionToWord (RawSmallInstruction so lv) = instructionToWord (SmallInstruction so lv)
rawInstructionToWord (RawLargeInstruction lo sv) = instructionToWord (LargeInstruction lo sv)
