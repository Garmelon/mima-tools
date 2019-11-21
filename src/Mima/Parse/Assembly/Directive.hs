{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.Assembly.Directive
  ( SetRegister(..)
  , Directive(..)
  , lDirective
  ) where

import qualified Data.Set as Set
import           Text.Megaparsec

import           Mima.Parse.Assembly.Common
import           Mima.Parse.Assembly.Lexeme
import           Mima.Parse.Common
import           Mima.Word

data SetRegister a
  = SetIAR a
  | SetACC MimaWord
  | SetRA a
  | SetSP a
  | SetFP a
  deriving (Show)

data Directive a
  = DReg (SetRegister a)    -- .reg (iar|acc|ra|sp|fp) <initial value>
  | DOrg MimaAddress        -- .org <address>
  | DLit MimaWord           -- .lit <word>
  | DArr [MimaWord]         -- .arr [<word>, ...]
  | DFlag (Set.Set Char)    -- .flag <chars>
  | DFlagOn (Set.Set Char)  -- .flagon <chars>
  | DFlagOff (Set.Set Char) -- .flagoff <chars>
  deriving (Show)

lSetRegister :: Parser (SetRegister Address)
lSetRegister =
      SetIAR <$> sepBySpace "iar" address
  <|> SetACC <$> sepBySpace "acc" word
  <|> SetRA  <$> sepBySpace "ra" address
  <|> SetSP  <$> sepBySpace "sp" address
  <|> SetFP  <$> sepBySpace "fp" address
  where
    sepBySpace name parser = symbol' name *> lSpace *> lexeme parser

lWordArray :: Parser [MimaWord]
lWordArray = open *> (word `sepBy` comma) <* close
  where
    open  = lexeme $ symbol "["
    comma = lexeme $ symbol ","
    close = lexeme $ symbol "]"

lFlags :: Parser (Set.Set Char)
lFlags = Set.unions <$> some (lexeme flag)

lDirective :: Parser (Directive Address)
lDirective = label "assembler directive" $
      DReg <$> directive ".reg" lSetRegister
  <|> DOrg <$> directive ".org" (lexeme largeValue)
  <|> DLit <$> directive ".lit" (lexeme word)
  <|> DArr <$> directive ".arr" lWordArray
  <|> DFlag    <$> directive ".flag"    lFlags
  <|> DFlagOn  <$> directive ".flagon"  lFlags
  <|> DFlagOff <$> directive ".flagoff" lFlags
  where
    directive name parser = symbol name *> lSpace *> parser
