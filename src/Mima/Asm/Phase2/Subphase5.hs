{-# LANGUAGE DataKinds #-}

module Mima.Asm.Phase2.Subphase5
  ( subphase5
  ) where

import           Control.Monad.Trans.State
import           Data.Void

import           Mima.Asm.Phase2.Types
import           Mima.Asm.Weed
import qualified Mima.Vm.Memory            as Vm
import qualified Mima.Vm.State             as Vm
import qualified Mima.Vm.Word              as Vm

type StateS5 = Vm.MimaState

type WeedS5 s = StateT StateS5 (Weed (WeedError s))

wordToVmWord :: MimaWord 'S4 s -> Vm.MimaWord
wordToVmWord (WordLocation addr) = Vm.largeValueToWord addr
wordToVmWord (WordRaw word)      = word

addRegister :: RegisterDirective 'S4 s -> WeedS5 s ()
addRegister (RegIar _ loc)  = modify $ \s5 -> s5{Vm.msIar = loc}
addRegister (RegAcc _ word) = modify $ \s5 -> s5{Vm.msAcc = wordToVmWord word}
addRegister (RegRa  _ loc)  = modify $ \s5 -> s5{Vm.msRa = loc}
addRegister (RegSp  _ loc)  = modify $ \s5 -> s5{Vm.msSp = loc}
addRegister (RegFp  _ loc)  = modify $ \s5 -> s5{Vm.msFp = loc}

addToMemory :: Vm.MimaAddress -> Vm.MimaWord -> WeedS5 s ()
addToMemory addr word = do
  s5 <- get
  let mem = Vm.msMemory s5
  put s5{Vm.msMemory = Vm.writeAt addr word mem}

updateToken :: AsmToken 'S4 s -> WeedS5 s ()
updateToken (TokenOrg _ v)         = absurd v
updateToken (TokenLabel _ _ v)     = absurd v
updateToken (TokenMeta _ _ v)      = absurd v
updateToken (TokenLit _ addr word) = addToMemory addr (wordToVmWord word)
updateToken (TokenInstr _ _ v)     = absurd v
updateToken (TokenReg _ _ reg)     = addRegister reg

subphase5 :: Phase2 'S4 s -> Weed (WeedError s) Vm.MimaState
subphase5 phase2 = execStateT (traverse updateToken phase2) initialS
  where
    initialS = Vm.basicState (Vm.mapToMemory mempty)
