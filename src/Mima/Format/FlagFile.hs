{-# LANGUAGE OverloadedStrings #-}

module Mima.Format.FlagFile
  ( formatFlagFile
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Mima.Flag
import           Mima.Format.Common
import           Mima.Word

fAddress :: MimaAddress -> T.Text
fAddress = fixWidthHex 5 . toHex

fFlagSet :: Set.Set Char -> T.Text
fFlagSet = T.pack . Set.toAscList

fRange :: AddressRange -> T.Text
fRange r
  | lower == upper = fAddress lower
  | otherwise      = fAddress lower <> "-" <> fAddress upper
  where
    lower = lowerAddress r
    upper = upperAddress r

fLine :: (AddressRange, Set.Set Char) -> T.Text
fLine (r, s) = fRange r <> ": " <> fFlagSet s <> "\n"

formatFlagFile :: RawFlags -> T.Text
formatFlagFile = mconcat . map fLine . Map.assocs
