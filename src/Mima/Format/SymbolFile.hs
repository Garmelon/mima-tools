{-# LANGUAGE OverloadedStrings #-}

module Mima.Format.SymbolFile
  ( formatSymbolFile
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Mima.Format.Common
import           Mima.Word
import           Mima.Label

fAddress :: MimaAddress -> T.Text
fAddress = fixWidthHex 5 . toHex

fLabels :: Set.Set LabelName -> T.Text
fLabels = T.intercalate " " . Set.toAscList

fLine :: (MimaAddress, Set.Set LabelName) -> T.Text
fLine (a, s) = fAddress a <> ": " <> fLabels s <> "\n"

formatSymbolFile :: LabelSpec -> T.Text
formatSymbolFile = mconcat . map fLine . Map.assocs . labelsByAddress
