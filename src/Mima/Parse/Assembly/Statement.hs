{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.Assembly.Statement
  ( Statement(..)
  , lStatement
  , lStatements
  ) where

import Text.Megaparsec

import Mima.Label
import Mima.Parse.Assembly.Common
import Mima.Parse.Assembly.Directive
import Mima.Parse.Assembly.Lexeme
import Mima.Parse.Assembly.RawInstruction
import Mima.Parse.Common
import Mima.Parse.Weed

data Statement a
  = SDirective (Directive a)
  | SRawInstruction (RawInstruction a)
  | SLabel LabelName
  deriving (Show)

lLabel :: Parser LabelName
lLabel = lexeme $ try $ labelName <* symbol ":"

lStatement :: Parser (Statement Address)
lStatement =
      try (SDirective <$> lDirective <* lNewlines)
  <|> try (SRawInstruction <$> lRawInstruction <* lNewlines)
  <|>     (SLabel <$> lLabel <* many lNewline)

lStatements :: Parser [WithOffset (Statement Address)]
lStatements = many (withOffset lStatement)
