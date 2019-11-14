module Mima.Parse.Common
  ( Parser
  -- * Basic parsers
  , whitespace
  -- ** Number literals
  , binDigit
  , decDigit
  , hexDigit
  , fixedWidthBin
  , fixedWidthDec
  , fixedWidthHex
  -- ** MiMa types
  , asWord
  , asLargeValue
  , asSmallValue
  , fixedWidthHexAddress
  -- * Nice error messages
  , defaultPosState
  , WeedError
  , WeedErrorBundle
  -- ** Remembering an element's offset
  , WithOffset
  , errorAt
  , errorAt'
  ) where

import           Data.Char
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec

import           Mima.Word

type Parser = Parsec Void T.Text

whitespace :: Parser Char
whitespace = label "whitespace" $ satisfy isWhitespace
  where
    isWhitespace '\n' = False
    isWhitespace c    = isSpace c

binDigit :: (Num a) => Parser a
binDigit = label "binary digit" $ token helper Set.empty
  where
    helper '0' = Just 0
    helper '1' = Just 1
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

fixedWidthWithExponent :: (Num a) => a -> Parser a -> Int -> Parser a
fixedWidthWithExponent e digit width = do
  digits <- count width digit
  pure $ helper $ reverse digits
  where
    helper []     = 0
    helper (x:xs) = x + e * helper xs

fixedWidthBin :: (Num a) => Int -> Parser a
fixedWidthBin = fixedWidthWithExponent 2 binDigit

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

{- Nice error messages -}

defaultPosState :: FilePath -> T.Text -> PosState T.Text
defaultPosState filename input = PosState
  { pstateInput      = input
  , pstateOffset     = 0
  , pstateSourcePos  = initialPos filename
  , pstateTabWidth   = defaultTabWidth
  , pstateLinePrefix = ""
  }

type WeedError = ParseError T.Text Void
type WeedErrorBundle = ParseErrorBundle T.Text Void

data WithOffset a = WithOffset Int a
  deriving (Show)

errorAt :: WithOffset a -> String -> WeedError
errorAt wo errorMsg = errorAt' wo [errorMsg]

errorAt' :: WithOffset a -> [String] -> WeedError
errorAt' (WithOffset o _) = FancyError o . Set.fromList . map ErrorFail
