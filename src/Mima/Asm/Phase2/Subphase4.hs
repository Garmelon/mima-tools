{-# LANGUAGE DataKinds #-}

module Mima.Asm.Phase2.Subphase4
  ( subphase4
  , throughThePhases
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Void
import           Text.Megaparsec

import           Mima.Asm.Phase1.Parse
import           Mima.Asm.Phase2.Subphase1
import           Mima.Asm.Phase2.Subphase2
import           Mima.Asm.Phase2.Subphase3
import           Mima.Asm.Phase2.Types
import           Mima.Asm.Weed
import qualified Mima.Vm.Instruction        as Vm
import qualified Mima.Vm.Word               as Vm

type WeedS4 s = ReaderT (Map.Map T.Text Vm.MimaAddress) (Weed (WeedError s))

resolveLabel :: Name s -> WeedS4 s Vm.MimaAddress
resolveLabel (Name s nameText) = do
  labels <- ask
  case labels Map.!? nameText of
    Nothing   -> lift $ critical $ errorWith s "unknown label"
    Just addr -> pure addr

resolveRelative :: s -> Vm.MimaAddress -> Integer -> WeedS4 s Vm.MimaAddress
resolveRelative s base offset = do
  let maxValue = toInteger (maxBound :: Vm.MimaAddress)
      res      = toInteger base + offset
  when (res < 0 || res > maxValue) $
    lift $ harmless $ errorWith s $ "address " ++ show res ++ " out of bounds"
  pure $ fromInteger res

resolveLocation :: Vm.MimaAddress -> Location s -> WeedS4 s Vm.MimaAddress
resolveLocation _ (LocAbsolute _ addr) = pure addr
resolveLocation baseAddr (LocRelative s offset) =
  resolveRelative s baseAddr offset
resolveLocation _ (LocLabel name) = resolveLabel name
resolveLocation _ (LocLabelRel s name _ offset) = do
  baseAddr <- resolveLabel name
  resolveRelative s baseAddr offset

convertInstruction :: Vm.MimaAddress -> Instruction 'S3 s -> WeedS4 s Vm.Instruction
convertInstruction baseAddr (SmallInstruction so loc) =
  Vm.SmallInstruction so <$> resolveLocation baseAddr loc
convertInstruction _ (LargeInstruction lo mv) =
  pure $ Vm.LargeInstruction lo $ fromMaybe 0 mv

resolveWord :: Vm.MimaAddress -> MimaWord 'S3 s -> WeedS4 s (MimaWord 'S4 s)
resolveWord _ (WordRaw w) = pure $ WordRaw w
resolveWord baseAddr (WordLocation loc) =
  WordLocation <$> resolveLocation baseAddr loc

resolveReg :: Vm.MimaAddress -> RegisterDirective 'S3 s -> WeedS4 s (RegisterDirective 'S4 s)
resolveReg baseAddr (RegAcc s word) = RegAcc s <$> resolveWord baseAddr word
resolveReg baseAddr (RegIar s loc)  = RegIar s <$> resolveLocation baseAddr loc
resolveReg baseAddr (RegRa s loc)   = RegRa s <$> resolveLocation baseAddr loc
resolveReg baseAddr (RegSp s loc)   = RegSp s <$> resolveLocation baseAddr loc
resolveReg baseAddr (RegFp s loc)   = RegFp s <$> resolveLocation baseAddr loc

updateToken :: AsmToken 'S3 s -> WeedS4 s (AsmToken 'S4 s)
updateToken (TokenOrg _ v)            = absurd v
updateToken (TokenLabel _ _ v)        = absurd v
updateToken (TokenMeta _ _ v)         = absurd v
updateToken (TokenLit s addr word)    =
  TokenLit s addr <$> resolveWord addr word
updateToken (TokenInstr s addr instr) =
  TokenLit s addr . WordRaw . Vm.instructionToWord <$> convertInstruction addr instr
updateToken (TokenReg s addr reg)     = TokenReg s addr <$> resolveReg addr reg

subphase4 :: Map.Map T.Text Vm.MimaAddress -> Phase2 'S3 s -> Weed (WeedError s) (Phase2 'S4 s)
subphase4 labelMap phase2 = runReaderT (traverse updateToken phase2) labelMap

throughThePhases :: String -> IO (Phase2 'S4 Span)
throughThePhases name = do
  text <- T.readFile name
  let Right res1 = parse parsePhase1 name text
  let Right s1 = runWeed $ subphase1 res1
  let Right s2 = runWeed $ subphase2 s1
  _ <- traverse print s2
  putStrLn "HEY"
  let Right (s3, m, _) = runWeed $ subphase3 s2
  let Right s4 = runWeed $ subphase4 m s3
  pure s4
