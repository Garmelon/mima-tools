module Mima.Format.Common
  ( toHex
  , fixedWidthHex
  , fixedWidthHexAddress
  ) where

import qualified Data.Text as T
import           Numeric

import           Mima.Word

toHex :: (Integral a, Show a) => a -> T.Text
toHex a = T.pack $ showHex a ""

fixedWidthHex :: (Integral a, Show a) => Int -> a -> T.Text
fixedWidthHex n = T.justifyRight n '0' . toHex

fixedWidthHexAddress :: MimaAddress -> T.Text
fixedWidthHexAddress = fixedWidthHex 5
