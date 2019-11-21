{-# LANGUAGE RecordWildCards #-}

module Mima.Parse.Assembly.Weed.Resolve
  ( resolveLabels
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.Map as Map

import           Mima.Label
import           Mima.Parse.Assembly.Common
import           Mima.Parse.Assembly.RawInstruction
import           Mima.Parse.Assembly.Weed.Common
import           Mima.Parse.Weed
import           Mima.Word

type RWeed a = ReaderT LabelSpec (Weed WeedError) a

resolve :: Address -> RWeed MimaAddress
resolve (Direct a) = pure a
resolve (Indirect wo) = do
  labels <- ask
  case labels Map.!? woValue wo of
    Just a  -> pure a
    Nothing -> 0 <$ lift (harmless $ errorAt wo "Could not resolve label")

rRegisters :: Registers Address -> RWeed (Registers MimaAddress)
rRegisters Registers{..} = Registers
  <$> resolveMaybe rIAR
  <*> pure rACC
  <*> resolveMaybe rRA
  <*> resolveMaybe rSP
  <*> resolveMaybe rFP
  where
    resolveMaybe :: Maybe Address -> RWeed (Maybe MimaAddress)
    resolveMaybe ma = sequenceA $ resolve <$> ma

rRawInstruction :: RawInstruction Address -> RWeed (RawInstruction MimaAddress)
rRawInstruction (RawSmallInstruction so  a) = RawSmallInstruction so <$> resolve a
rRawInstruction (RawLargeInstruction lo sv) = pure $ RawLargeInstruction lo sv

rAlmostWord :: AlmostWord Address -> RWeed (AlmostWord MimaAddress)
rAlmostWord (AInstruction i) = AInstruction <$> rRawInstruction i
rAlmostWord (ALiteral w)     = pure $ ALiteral w

rWeedResult :: WeedResult Address -> RWeed (WeedResult MimaAddress)
rWeedResult WeedResult{..} = WeedResult
  <$> rRegisters wrRegisters
  <*> traverse rAlmostWord wrMemory
  <*> pure wrLabels
  <*> pure wrFlags

resolveLabels :: WeedResult Address -> Weed WeedError (WeedResult MimaAddress)
resolveLabels wr = runReaderT (rWeedResult wr) (wrLabels wr)
