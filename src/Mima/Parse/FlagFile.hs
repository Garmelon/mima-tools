{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.FlagFile
  ( parseFlagFile
  , readFlagFile
  ) where

import           Control.Monad
import           Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Text.Megaparsec

import           Mima.Flag
import           Mima.Parse.Common
import           Mima.Parse.Lexeme
import           Mima.Parse.Weed
import           Mima.Word

lAddress :: Parser MimaAddress
lAddress = lexeme fixedWidthHexAddress

lFlag :: Parser (Set.Set Char)
lFlag = lexeme $ label "alphanumeric character" $ Set.singleton <$> satisfy isAlphaNum

lFlags :: Parser (Set.Set Char)
lFlags = Set.unions <$> some lFlag

lAddressRange :: Parser AddressRange
lAddressRange = do
  firstAddress <- lAddress
  secondAddress <- (symbol "-" *> lAddress) <|> pure firstAddress
  pure $ range firstAddress secondAddress

lLine :: Parser (AddressRange, Set.Set Char)
lLine = do
  a <- lAddressRange
  void $ symbol ":"
  f <- lFlags
  hidden lNewlines
  pure (a, f)

parseFlagFile :: Parser RawFlags
parseFlagFile = space *> many lNewline *> (Map.fromList <$> many lLine) <* hidden eof

readFlagFile :: FilePath -> T.Text -> Either WeedErrorBundle RawFlags
readFlagFile filename input = parse parseFlagFile filename input
