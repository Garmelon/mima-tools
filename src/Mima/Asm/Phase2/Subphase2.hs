{-# LANGUAGE DataKinds #-}

module Mima.Asm.Phase2.Subphase2
  ( subphase2
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Maybe

import           Mima.Asm.Phase2.Types
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

unfilledAddress :: s -> WeedS2 s Vm.MimaAddress
unfilledAddress s = do
  s2 <- get
  when (s2AddressFilled s2) $ addAddress s 1
  s2CurrentAddress <$> get

filledAddress :: s -> WeedS2 s Vm.MimaAddress
filledAddress s = do
  addr <- unfilledAddress s
  modify $ \s2' -> s2'{s2AddressFilled = True}
  pure addr

currentAddress :: WeedS2 s Vm.MimaAddress
currentAddress = s2CurrentAddress <$> get

convertP2Token :: AsmToken 'S1 s -> WeedS2 s (Maybe (AsmToken 'S2 s))
convertP2Token (TokenOrg _ (OrgAddrAbsolute s address))
  = Nothing <$ setAddress s address
convertP2Token (TokenOrg _ (OrgAddrRelative s address))
  | address < 0 = Nothing <$ setAddress s (maxBound + fromIntegral address)
  | otherwise = Nothing <$ addAddress s (fromIntegral address)
convertP2Token (TokenLabel s _ name) = do
  address <- unfilledAddress s
  pure $ Just $ TokenLabel s address name
convertP2Token (TokenMeta s _ meta@(MetaStop _ _)) = do
  address <- currentAddress
  pure $ Just $ TokenMeta s address meta
convertP2Token (TokenMeta s _ meta) = do
  address <- unfilledAddress s
  pure $ Just $ TokenMeta s address meta
convertP2Token (TokenLit s _ word) = do
  address <- filledAddress s
  pure $ Just $ TokenLit s address $ idWord word
convertP2Token (TokenInstr s _ instr) = do
  address <- filledAddress s
  pure $ Just $ TokenInstr s address $ idInstruction instr
convertP2Token (TokenReg s _ reg) = do
  address <- currentAddress
  pure $ Just $ TokenReg s address $ idRegDir reg

subphase2 :: Phase2 'S1 s -> Weed (WeedError s) (Phase2 'S2 s)
subphase2 s1 = do
  let initialS = StateS2 0 False
  catMaybes <$> evalStateT (traverse convertP2Token s1) initialS
