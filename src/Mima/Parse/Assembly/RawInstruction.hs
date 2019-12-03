{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.Assembly.RawInstruction
  ( RawInstruction(..)
  , lRawInstruction
  , cookInstruction
  ) where

import           Control.Monad
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C

import           Mima.Instruction
import           Mima.Parse.Assembly.Common
import           Mima.Parse.Assembly.Lexeme
import           Mima.Parse.Common
import           Mima.Word
  
data RawInstruction a
  = RawSmallInstruction SmallOpcode a
  | RawLargeInstruction LargeOpcode SmallValue
  deriving (Show)

parseByName :: [(T.Text, a)] -> Parser a
parseByName = foldl (<|>) empty . map (\(name, a) -> a <$ C.string' name)

smallOpcode :: Parser SmallOpcode
smallOpcode = parseByName
  [ ("ldc", LDC)
  , ("ldv", LDV)
  , ("stv", STV)
  , ("add", ADD)
  , ("and", AND)
  , ("or", OR)
  , ("xor", XOR)
  , ("eql", EQL)
  , ("jmp", JMP)
  , ("jmn", JMN)
  , ("ldiv", LDIV)
  , ("stiv", STIV)
  , ("call", CALL)
  , ("adc", ADC)
  ]

largeOpcode :: Parser LargeOpcode
largeOpcode = parseByName
  [ ("halt", HALT)
  , ("not", NOT)
  , ("rar", RAR)
  , ("ret", RET)
  , ("ldra", LDRA)
  , ("stra", STRA)
  , ("ldsp", LDSP)
  , ("stsp", STSP)
  , ("ldfp", LDFP)
  , ("stfp", STFP)
  , ("ldrs", LDRS)
  , ("strs", STRS)
  , ("ldrf", LDRF)
  , ("strf", STRF)
  ]

lRawInstruction :: Parser (RawInstruction Address)
lRawInstruction = label "instruction" $ smallInstruction <|> largeInstruction
  where
    smallInstruction = do
      so <- smallOpcode
      void lSpace
      lv <- lexeme address
      pure $ RawSmallInstruction so lv
    largeInstruction = do
      lo <- largeOpcode
      if argumentIsOptional lo
        then do
          sv <- lexeme (try (lSpace *> smallValue) <|> pure 0)
          pure $ RawLargeInstruction lo sv
        else do
          sv <- lSpace *> lexeme smallValue
          pure $ RawLargeInstruction lo sv

cookInstruction :: RawInstruction MimaAddress -> Instruction
cookInstruction (RawSmallInstruction so lv) = SmallInstruction so lv
cookInstruction (RawLargeInstruction lo sv) = LargeInstruction lo sv
