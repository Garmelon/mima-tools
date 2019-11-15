{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.FlagFile
  ( parseFlagFile
  ) where

import           Control.Monad
import           Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Text.Megaparsec

import           Mima.Flag
import           Mima.Parse.Common
import           Mima.Parse.Lexeme
import           Mima.Word

lAddress :: Parser MimaAddress
lAddress = lexeme fixedWidthHexAddress

lFlag :: Parser (Set.Set Flag)
lFlag =
  -- Not sure if there's a better way than writing the fold
  -- explicitly. Mconcat doesn't seem to do the trick.
  let knownFlags = foldl (<|>) empty
                 $ map (\f -> Set.singleton f <$ single (flagChar f)) allFlags
      otherFlags = label "alphanumeric character" $ Set.empty <$ satisfy isAlphaNum
  in lexeme $ knownFlags <|> otherFlags

lFlags :: Parser (Set.Set Flag)
lFlags = Set.unions <$> some lFlag

lAddressRange :: Parser AddressRange
lAddressRange = do
  firstAddress <- lAddress
  secondAddress <- (symbol "-" *> lAddress) <|> pure firstAddress
  void $ symbol ":"
  pure $ range firstAddress secondAddress

lLine :: Parser (AddressRange, Set.Set Flag)
lLine = do
  a <- lAddressRange
  void $ symbol ":"
  f <- lFlags
  hidden lNewlines
  pure (a, f)

parseFlagFile :: Parser (Map.Map AddressRange (Set.Set Flag))
parseFlagFile = space *> many lNewline *> (Map.fromList <$> many lLine) <* hidden eof
