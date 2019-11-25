{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.Assembly.Lexeme
  ( space
  , lexeme
  , symbol
  , symbol'
  , lSpace
  , lNewline
  , lNewlines
  ) where

import           Control.Monad
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import           Mima.Parse.Common

space :: Parser ()
space = L.space (void whitespace) (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

symbol' :: T.Text -> Parser T.Text
symbol' = L.symbol' space

lSpace :: Parser ()
lSpace = () <$ lexeme whitespace

lNewline :: Parser ()
lNewline = void $ lexeme C.newline

lNewlines :: Parser ()
lNewlines = void (some lNewline) <|> eof
