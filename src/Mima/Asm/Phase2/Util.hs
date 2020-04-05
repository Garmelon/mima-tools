{-# LANGUAGE ScopedTypeVariables #-}

module Mima.Asm.Phase2.Util
  ( intToBounded
  ) where

import           Control.Monad

import           Mima.Asm.Weed

intToBounded :: forall s n. (Bounded n, Integral n) => s -> Integer -> Weed (WeedError s) n
intToBounded s val = do
  when (val < minVal || val > maxVal) $
    harmless $ errorWith s "value out of bounds"
  pure $ fromInteger val
  where
    maxVal = toInteger (maxBound :: n)
    minVal = -maxVal - 1
