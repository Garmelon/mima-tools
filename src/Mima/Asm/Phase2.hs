module Mima.Asm.Phase2
  ( phase1To2
  ) where

import qualified Mima.Asm.Phase1 as P1
import           Mima.Asm.Weed
import qualified Mima.Vm.State   as Vm

phase1To2 :: P1.Phase1 s -> Weed (WeedError s) Vm.MimaState
phase1To2 = error "to be implemented"
