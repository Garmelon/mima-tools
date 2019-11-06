module Mima.Util
  ( ToText(..)
  , toDec
  , toHex
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

toDec :: (Integral a, Show a) => Int -> a -> T.Text
toDec digits a = T.justifyRight digits ' ' $ T.pack $ show a

toHex :: (Integral a, Show a) => Int -> a -> T.Text
toHex digits a = T.justifyRight digits '0' $ T.pack $ N.showHex a ""
