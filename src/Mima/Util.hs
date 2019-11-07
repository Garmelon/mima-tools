{-# LANGUAGE OverloadedStrings #-}

module Mima.Util
  ( ToText(..)
  , HexLike(..)
  , groupByTwoChars
  , integralToDec
  , integralToHex
  ) where

import qualified Data.Text as T
import qualified Numeric as N

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

class HexLike a where
  toDec :: a -> T.Text
  toHex :: a -> T.Text

  toHexBytes :: a -> T.Text
  toHexBytes = T.intercalate " " . groupByTwoChars . toHex

groupByTwoChars :: T.Text -> [T.Text]
groupByTwoChars = reverse . helper . T.unpack . T.reverse
  where
    helper (c1:c2:cs) = T.pack [c2, c1] : helper cs
    helper [c] = [T.singleton c]
    helper []  = []

integralToDec :: (Integral a, Show a) => Int -> a -> T.Text
integralToDec digits a = T.justifyRight digits ' ' $ T.pack $ show a

integralToHex :: (Integral a, Show a) => Int -> a -> T.Text
integralToHex digits a = T.justifyRight digits '0' $ T.pack $ N.showHex a ""
