module Mima.Vm.Memory
  ( MimaMemory
  , mapToMemory
  , wordsToMemory
  , memoryToWords
  , maxAddress
  , usedAddresses
  , continuousUsedAddresses
  , readAt
  , writeAt
  ) where

import           Data.Bits
import qualified Data.Map.Strict as Map

import           Mima.Vm.Word

newtype MimaMemory = MimaMemory (Map.Map MimaAddress MimaWord)
  deriving (Show)

mapToMemory :: Map.Map MimaAddress MimaWord -> MimaMemory
mapToMemory = MimaMemory . Map.filter (/= zeroBits)

wordsToMemory :: [MimaWord] -> MimaMemory
wordsToMemory = mapToMemory
              . Map.fromAscList
              . zip [minBound..]

memoryToWords :: MimaMemory -> [MimaWord]
memoryToWords mem = map (`readAt` mem) $ continuousUsedAddresses mem

maxAddress :: MimaMemory -> MimaAddress
maxAddress (MimaMemory m) = maybe minBound fst $ Map.lookupMax m

usedAddresses :: MimaMemory -> [MimaAddress]
usedAddresses (MimaMemory m) = Map.keys m

continuousUsedAddresses :: MimaMemory -> [MimaAddress]
continuousUsedAddresses mem = [minBound..maxAddress mem]

readAt :: MimaAddress -> MimaMemory -> MimaWord
readAt addr (MimaMemory m) = Map.findWithDefault zeroBits addr m

writeAt :: MimaAddress -> MimaWord -> MimaMemory -> MimaMemory
writeAt addr word (MimaMemory m)
  | word == zeroBits = MimaMemory $ Map.delete addr m
  | otherwise        = MimaMemory $ Map.insert addr word m
