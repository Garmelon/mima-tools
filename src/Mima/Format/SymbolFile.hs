{-# LANGUAGE OverloadedStrings #-}

module Mima.Format.SymbolFile
  ( formatSymbolFile
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Mima.Format.Common
import           Mima.Word

type LabelName = T.Text

combineByAddress :: Map.Map LabelName MimaAddress -> Map.Map MimaAddress (Set.Set LabelName)
combineByAddress = ($ Map.empty )
                 . mconcat
                 . reverse
                 . map (\(l, a) -> Map.insertWith Set.union a (Set.singleton l))
                 . Map.assocs

fLabels :: Set.Set LabelName -> T.Text
fLabels = T.intercalate " " . Set.toAscList

fLine :: (MimaAddress, Set.Set LabelName) -> T.Text
fLine (a, s) = fixedWidthHexAddress a <> ": " <> fLabels s <> "\n"

formatSymbolFile :: Map.Map LabelName MimaAddress -> T.Text
formatSymbolFile = mconcat . map fLine . Map.assocs . combineByAddress
