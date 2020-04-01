{-# LANGUAGE OverloadedStrings #-}

module Mima.Format
  ( ToText(..)
  , toBin
  , toDec
  , toHex
  , negative
  , chunkedBy
  , chunkyBin
  , chunkyDec
  , chunkyHex
  , fixWidthBin
  , fixWidthDec
  , fixWidthHex
  ) where

import qualified Data.Text as T
import           Numeric

-- | A class for types that can be converted to 'T.Text'.
--
-- This class does not mean to convert elements to text in a
-- standardized way. It is just to reduce the clutter of functions
-- with names like @somethingToText@.
--
-- Only create an instance of this class when there is an obvious,
-- preferrable way of converting something to text! If there are
-- multiple "obvious" options, create no instance of this class and
-- instead name the functions individually.
class ToText a where
  toText :: a -> T.Text

toBin :: (Integral a, Show a) => a -> T.Text
toBin a
  | a < 0 = "-" <> toBin (- a)
toBin 0 = "0"
toBin a = T.reverse $ T.pack $ toBin' a
  where
    toBin' :: (Integral a, Show a) => a -> String
    toBin' 0 = []
    toBin' n = (if n `mod` 2 == 0 then '0' else '1') : toBin' (n `div` 2)

toDec :: (Integral a, Show a ) => a -> T.Text
toDec a = T.pack $ showInt a ""

toHex :: (Integral a, Show a) => a -> T.Text
toHex a = T.pack $ showHex a ""

-- | @'negative' a@ interprets @a@ as @a - 'maxBound' - 1@ if @a@ is greater
-- than @'maxBound' `div` 2@, and as a positive number otherwise. 'minBound' is
-- ignored.
negative :: (Integral a, Bounded a, Show a) => (a -> T.Text) -> a -> T.Text
negative f a
  | a > maxBound `div` 2 = "-" <> f (-(a - maxBound - 1))
  | otherwise            = f a

chunkedBy :: T.Text -> Int -> T.Text -> T.Text
chunkedBy sep n = T.reverse . T.intercalate sep . T.chunksOf n . T.reverse

chunkyBin :: T.Text -> T.Text
chunkyBin = chunkedBy " " 4

chunkyDec :: T.Text -> T.Text
chunkyDec = chunkedBy " " 3

chunkyHex :: T.Text -> T.Text
chunkyHex = chunkedBy " " 2

fixWidthBin :: Int -> T.Text -> T.Text
fixWidthBin n = T.justifyRight n '0'

fixWidthDec :: Int -> T.Text -> T.Text
fixWidthDec n = T.justifyRight n ' '

fixWidthHex :: Int -> T.Text -> T.Text
fixWidthHex n = T.justifyRight n '0'
