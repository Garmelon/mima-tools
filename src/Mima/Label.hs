module Mima.Label
  ( LabelName
  , LabelSpec
  , labelsByAddress
  , noLabels
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Mima.Word

type LabelName = T.Text
type LabelSpec = Map.Map LabelName MimaAddress

labelsByAddress :: LabelSpec -> Map.Map MimaAddress (Set.Set LabelName)
labelsByAddress = ($ Map.empty)
                . mconcat
                . reverse
                . map (\(l, a) -> Map.insertWith Set.union a (Set.singleton l))
                . Map.assocs

noLabels :: LabelSpec
noLabels = Map.empty