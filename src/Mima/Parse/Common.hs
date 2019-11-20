module Mima.Parse.Common
  ( Parser
  -- * Character specifications
  , isConnecting
  , isWhitespace
  -- * Basic parsers
  , whitespace
  , labelName
  , flag
  -- ** Number literals
  , binDigit
  , decDigit
  , octDigit
  , hexDigit
  , binNumber
  , decNumber
  , octNumber
  , hexNumber
  , fixedWidthBin
  , fixedWidthDec
  , fixedWidthOct
  , fixedWidthHex
  -- ** MiMa types
  , asWord
  , asLargeValue
  , asSmallValue
  , fixedWidthHexAddress
  ) where

import           Data.Char
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec

import           Mima.Word

type Parser = Parsec Void T.Text

{- Character specifications -}

isConnecting :: Char -> Bool
isConnecting '_' = True
isConnecting '-' = True
isConnecting _   = False

isWhitespace :: Char -> Bool
isWhitespace '\n' = False
isWhitespace c    = isSpace c

{- Basic parsers -}

whitespace :: Parser Char
whitespace = label "whitespace" $ satisfy isWhitespace

labelName :: Parser T.Text
labelName = label "label" $ do
  t <- satisfy isAlpha
  ts <- takeWhileP Nothing (\c -> isAlphaNum c || isConnecting c)
  pure $ T.singleton t <> ts

flag :: Parser (Set.Set Char)
flag = label "alphanumeric character" $ Set.singleton <$> satisfy isAlphaNum

binDigit :: (Num a) => Parser a
binDigit = label "binary digit" $ token helper Set.empty
  where
    helper '0' = Just 0
    helper '1' = Just 1
    helper _   = Nothing

octDigit :: (Num a) => Parser a
octDigit = label "octal digit" $ token helper Set.empty
  where
    helper '0' = Just 0
    helper '1' = Just 1
    helper '2' = Just 2
    helper '3' = Just 3
    helper '4' = Just 4
    helper '5' = Just 5
    helper '6' = Just 6
    helper '7' = Just 7
    helper _   = Nothing

decDigit :: (Num a) => Parser a
decDigit = label "decimal digit" $ token helper Set.empty
  where
    helper '0' = Just 0
    helper '1' = Just 1
    helper '2' = Just 2
    helper '3' = Just 3
    helper '4' = Just 4
    helper '5' = Just 5
    helper '6' = Just 6
    helper '7' = Just 7
    helper '8' = Just 8
    helper '9' = Just 9
    helper _   = Nothing

hexDigit :: (Num a) => Parser a
hexDigit = label "hexadecimal digit" $ token (helper . toLower) Set.empty
  where
    helper '0' = Just  0
    helper '1' = Just  1
    helper '2' = Just  2
    helper '3' = Just  3
    helper '4' = Just  4
    helper '5' = Just  5
    helper '6' = Just  6
    helper '7' = Just  7
    helper '8' = Just  8
    helper '9' = Just  9
    helper 'a' = Just 10
    helper 'b' = Just 11
    helper 'c' = Just 12
    helper 'd' = Just 13
    helper 'e' = Just 14
    helper 'f' = Just 15
    helper _   = Nothing

accumulateToBase :: (Integral a) => a -> [a] -> a
accumulateToBase base = helper . reverse
  where
    helper []     = 0
    helper [d]    = d
    helper (d:ds) = d + base * helper ds

binNumber :: (Integral a) => Parser a
binNumber = label "binary number" $ accumulateToBase 2 <$> some binDigit

octNumber :: (Integral a) => Parser a
octNumber = label "octal number" $ accumulateToBase 8 <$> some octDigit

decNumber :: (Integral a) => Parser a
decNumber = label "decimal number" $ accumulateToBase 10 <$> some decDigit

hexNumber :: (Integral a) => Parser a
hexNumber = label "hexadecimal number" $ accumulateToBase 16 <$> some hexDigit

fixedWidthWithExponent :: (Num a) => a -> Parser a -> Int -> Parser a
fixedWidthWithExponent e digit width = do
  digits <- count width digit
  pure $ helper $ reverse digits
  where
    helper []     = 0
    helper (x:xs) = x + e * helper xs

fixedWidthBin :: (Num a) => Int -> Parser a
fixedWidthBin = fixedWidthWithExponent 2 binDigit

fixedWidthOct :: (Num a) => Int -> Parser a
fixedWidthOct = fixedWidthWithExponent 8 octDigit

fixedWidthDec :: (Num a) => Int -> Parser a
fixedWidthDec = fixedWidthWithExponent 10 decDigit

fixedWidthHex :: (Num a) => Int -> Parser a
fixedWidthHex = fixedWidthWithExponent 16 hexDigit

asBoundedValue :: (Show a, Ord a) => a -> a -> Parser a -> Parser a
asBoundedValue lower upper parser =
  label ("value within bounds " ++ show (lower, upper)) $ do
    value <- parser
    if lower <= value && value <= upper
      then pure value
      else empty

asWord :: Parser Integer -> Parser MimaWord
asWord parser =
  let bound = fromIntegral (maxBound :: MimaWord)
  in  fromIntegral <$> asBoundedValue (-bound) bound parser

asLargeValue :: Parser Integer -> Parser LargeValue
asLargeValue parser =
  let bound = fromIntegral (maxBound :: LargeValue)
  in  fromIntegral <$> asBoundedValue (-bound) bound parser

asSmallValue :: Parser Integer -> Parser SmallValue
asSmallValue parser =
  let bound = fromIntegral (maxBound :: SmallValue)
  in  fromIntegral <$> asBoundedValue (-bound) bound parser

fixedWidthHexAddress :: Parser MimaAddress
fixedWidthHexAddress = label "fixed-width hexadecimal address"
                     $ asLargeValue
                     $ fixedWidthHex 5
