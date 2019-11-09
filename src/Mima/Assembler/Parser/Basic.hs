{-# LANGUAGE OverloadedStrings #-}

module Mima.Assembler.Parser.Basic
  ( Parser
  , withOffset
  , failAt
  -- * Character specifications
  , isAlphabet
  , isConnecting
  , isWhitespace
  -- * Lexme
  , whitespace
  , space
  , lexeme
  , newline
  , newlines
  , colon
  -- * Basic data types
  , mimaWord
  , largeValue
  , largeValue'
  , smallValue
  -- * Stateful parsing
  , StatefulParser
  , runStatefulParser
  ) where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import           Mima.Word

type Parser = Parsec Void T.Text

withOffset :: Parser a -> Parser (a, Int)
withOffset parser = (,) <$> parser <*> getOffset

failAt :: Int -> String -> Parser a
failAt offset message = do
  setOffset offset
  fail message

{- Character specifications -}

isOneOf :: String -> Char -> Bool
isOneOf s t =
  let charSet = Set.fromList s
  in  t `Set.member` charSet

isAlphabet :: Char -> Bool
isAlphabet = isOneOf (['a'..'z'] ++ ['A'..'Z'])

isConnecting :: Char -> Bool
isConnecting = isOneOf "_-"

isWhitespace :: Char -> Bool
isWhitespace = isOneOf " \t"

{- Lexeme stuff -}

whitespace :: Parser Char
whitespace = label "space" $ satisfy isWhitespace

space :: Parser ()
space = L.space (void whitespace) (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

newline :: Parser ()
newline = void $ lexeme C.newline

newlines :: Parser ()
newlines = void (some newline) <|> eof

colon :: Parser ()
colon = void $ lexeme $ C.string ":"

{- Basic data types -}

fromHex :: (Num a) => Int -> Parser a
fromHex bitWidth = do
  void $ C.string' "0x"
  n <- L.hexadecimal :: Parser Integer
  let upperBound = 2^bitWidth - 1
  if 0 <= n && n <= upperBound
    then pure $ fromIntegral n
    else fail $ "value " ++ show n ++ " out of bounds " ++ show (0 :: Integer, upperBound)

fromDec :: (Num a) => Int -> Parser a
fromDec bitWidth = do
  n <- L.signed mempty L.decimal :: Parser Integer
  let upperBound = 2^bitWidth - 1
  if (-upperBound) <= n && n <= upperBound
    then pure $ fromIntegral n
    else fail $ "value " ++ show n ++ " out of bounds " ++ show (-upperBound, upperBound)

mimaWord :: Parser MimaWord
mimaWord = lexeme $ label "24-bit number" $ fromHex 24 <|> fromDec 24

largeValue :: Parser LargeValue
largeValue = lexeme $ largeValue'

-- | Non-lexeme version of 'largeValue'
largeValue' :: Parser LargeValue
largeValue' = label "20-bit number" $ fromHex 20 <|> fromDec 20

smallValue :: Parser SmallValue
smallValue = lexeme $ label "16-bit number" $ fromHex 16 <|> fromDec 16

{- Stateful parsing -}

type StatefulParser s a = StateT s Parser a

runStatefulParser :: StatefulParser s a -> s -> Parser (a, s)
runStatefulParser = runStateT
