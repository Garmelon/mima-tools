module Mima.Asm.Phase2
  ( phase1To2
  ) where

import qualified Mima.Asm.Phase1           as P1
import           Mima.Asm.Phase2.Subphase1
import           Mima.Asm.Phase2.Subphase2
import           Mima.Asm.Phase2.Subphase3
import           Mima.Asm.Phase2.Subphase4
import           Mima.Asm.Phase2.Subphase5
import           Mima.Asm.Weed
import qualified Mima.Vm.Metadata          as Vm
import qualified Mima.Vm.State             as Vm

phase1To2 :: P1.Phase1 s -> Weed (WeedError s) (Vm.MimaState, Vm.Metadata)
phase1To2 phase1 = do
  s1 <- subphase1 phase1
  s2 <- subphase2 s1
  (s3, labelMap, metadata) <- subphase3 s2
  s4 <- subphase4 labelMap s3
  state <- subphase5 s4
  pure (state, metadata)
