{-# LANGUAGE OverloadedStrings #-}

module Mima.Format.Common
  ( toBin
  , toDec
  , toHex
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
toDec = T.pack . show

toHex :: (Integral a, Show a) => a -> T.Text
toHex a = T.pack $ showHex a ""

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
