{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.Assembly.Common
  ( number
  , word
  , largeValue
  , smallValue
  , Address(..)
  , address
  ) where

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import           Mima.Label
import           Mima.Parse.Assembly.Lexeme
import           Mima.Parse.Common
import           Mima.Parse.Weed
import           Mima.Word

number :: Parser Integer
number = L.signed (pure ()) $
      (symbol' "0b" *> binNumber)
  <|> (symbol' "0o" *> octNumber)
  <|> (symbol' "0x" *> hexNumber)
  <|> decNumber

word :: Parser MimaWord
word = label "word (24 bit)" $ asWord number

largeValue :: Parser LargeValue
largeValue = label "large value (20 bit)" $ asLargeValue number

smallValue :: Parser SmallValue
smallValue = label "large value (16 bit)" $ asSmallValue number

data Address = Direct LargeValue | Indirect (WithOffset LabelName)
  deriving (Show)

address :: Parser Address
address = (Direct <$> largeValue) <|> (Indirect <$> withOffset labelName)
