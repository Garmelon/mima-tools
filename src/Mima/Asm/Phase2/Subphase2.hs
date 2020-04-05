{-# LANGUAGE DataKinds #-}

module Mima.Asm.Phase2.Subphase2
  ( subphase2
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Maybe

import           Mima.Asm.Phase2.Types
import           Mima.Asm.Phase2.Util
import           Mima.Asm.Weed
import qualified Mima.Vm.Word              as Vm

data StateS2 s = StateS2
  { s2CurrentAddress :: Vm.MimaAddress
  , s2AddressFilled  :: Bool
  } deriving (Show)

type WeedS2 s = StateT (StateS2 s) (Weed (WeedError s))

addAddress :: s -> Int -> WeedS2 s ()
addAddress s amount = do
  s2 <- get
  setAddress s (s2CurrentAddress s2 + fromIntegral amount)

setAddress :: s -> Vm.MimaAddress -> WeedS2 s ()
setAddress s newAddress = do
  s2 <- get
  let oldAddress = s2CurrentAddress s2
  when (oldAddress > newAddress) $
    lift $ harmless $
    errorWith s "new address must not be smaller than current address"
  put $ s2{s2CurrentAddress = newAddress}
  when (newAddress /= oldAddress) $
    modify $ \s2' -> s2'{s2AddressFilled = False}

nextAddress :: s -> WeedS2 s Vm.MimaAddress
nextAddress s = do
  s2 <- get
  when (s2AddressFilled s2) $ addAddress s 1
  pure $ s2CurrentAddress s2

convertLocation :: Vm.MimaAddress -> LocationX 'S1 s -> WeedS2 s (LocationX 'S2 s)
convertLocation _ (Loc1Absolute s addr)  = pure $ Loc2Absolute s addr
convertLocation _ (Loc1Label name)       = pure $ Loc2Label name
convertLocation _ (Loc1LabelRel s name s' offset) =
  pure $ Loc2LabelRel s name s' offset
convertLocation baseAddr (Loc1Relative s delta) = do
  let newAddr = toInteger baseAddr + delta
  val <- lift $ intToBounded s newAddr
  pure $ Loc2Absolute s val

convertMimaWord :: Vm.MimaAddress -> MimaWord 'S1 s -> WeedS2 s (MimaWord 'S2 s)
convertMimaWord baseAddr (WordLocation loc) =
  WordLocation <$> convertLocation baseAddr loc
convertMimaWord _ (WordRaw word) = pure $ WordRaw word

convertInstruction :: Vm.MimaAddress -> Instruction 'S1 s -> WeedS2 s (Instruction 'S2 s)
convertInstruction baseAddr (SmallInstruction opcode loc) =
  SmallInstruction opcode <$> convertLocation baseAddr loc
convertInstruction _ (LargeInstruction opcode val) =
  pure $ LargeInstruction opcode val

convertRegisterDirective :: Vm.MimaAddress -> RegisterDirective 'S1 s -> WeedS2 s (RegisterDirective 'S2 s)
convertRegisterDirective baseAddr (RegIar s loc) =
  RegIar s <$> convertLocation baseAddr loc
convertRegisterDirective baseAddr (RegAcc s word) =
  RegAcc s <$> convertMimaWord baseAddr word
convertRegisterDirective baseAddr (RegRa s loc) =
  RegRa s <$> convertLocation baseAddr loc
convertRegisterDirective baseAddr (RegSp s loc) =
  RegSp s <$> convertLocation baseAddr loc
convertRegisterDirective baseAddr (RegFp s loc) =
  RegFp s <$> convertLocation baseAddr loc

convertP2Token :: AsmToken 'S1 s -> WeedS2 s (Maybe (AsmToken 'S2 s))
convertP2Token (TokenOrg _ (OrgAddrAbsolute s address))
  = Nothing <$ setAddress s address
convertP2Token (TokenOrg _ (OrgAddrRelative s address))
  | address < 0 = Nothing <$ setAddress s (maxBound + fromIntegral address)
  | otherwise = Nothing <$ addAddress s (fromIntegral address)
convertP2Token (TokenLabel s _ name) = do
  address <- s2CurrentAddress <$> get
  pure $ Just $ TokenLabel s address name
convertP2Token (TokenMeta s _ meta) = do
  address <- s2CurrentAddress <$> get
  pure $ Just $ TokenMeta s address meta
convertP2Token (TokenLit s _ word) = do
  address <- nextAddress s
  newWord <- convertMimaWord address word
  pure $ Just $ TokenLit s address newWord
convertP2Token (TokenInstr s _ instr) = do
  address <- nextAddress s
  Just . TokenInstr s address <$> convertInstruction address instr
convertP2Token (TokenReg s _ reg) = do
  address <- s2CurrentAddress <$> get
  Just . TokenReg s address <$> convertRegisterDirective address reg

subphase2 :: Phase2 'S1 s -> Weed (WeedError s) (Phase2 'S2 s)
subphase2 s1 = do
  let initialS = StateS2 0 False
  catMaybes <$> evalStateT (traverse convertP2Token s1) initialS
