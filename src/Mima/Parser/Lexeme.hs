module Mima.Parser.Lexeme
  ( space
  , lexeme
  , symbol
  , lNewline
  , lNewlines
  ) where

import           Control.Monad
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import           Mima.Parser.Common

space :: Parser ()
space = L.space (void whitespace) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

lNewline :: Parser ()
lNewline = void $ lexeme C.newline

lNewlines :: Parser ()
lNewlines = void (some lNewline) <|> eof
