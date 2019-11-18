{-# LANGUAGE OverloadedStrings #-}

module Mima.Format.FlagFile
  ( formatFlagFile
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Mima.Flag
import           Mima.Format.Common

fFlagSet :: Set.Set Char -> T.Text
fFlagSet = T.pack . Set.toAscList

fRange :: AddressRange -> T.Text
fRange r
  | lower == upper = fixedWidthHexAddress lower
  | otherwise      = fixedWidthHexAddress lower <> "-" <> fixedWidthHexAddress upper
  where
    lower = lowerAddress r
    upper = upperAddress r

fLine :: (AddressRange, Set.Set Char) -> T.Text
fLine (r, s) = fRange r <> ": " <> fFlagSet s <> "\n"

formatFlagFile :: AllFlags -> T.Text
formatFlagFile = mconcat . map fLine . Map.assocs
