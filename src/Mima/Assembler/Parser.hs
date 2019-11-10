{-# LANGUAGE TupleSections #-}

module Mima.Assembler.Parser
  ( parseState
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import           Text.Megaparsec

import           Mima.Assembler.Parser.Basic
import           Mima.Assembler.Parser.Instruction
import           Mima.Assembler.Parser.Label
import           Mima.Assembler.Parser.RawInstruction
import           Mima.Assembler.Parser.Register
import           Mima.State
import           Mima.Word

data MyState = MyState
  { sCurrentPos   :: MimaAddress
  , sLabels       :: Map.Map MimaLabel MimaAddress
  , sInstructions :: Map.Map MimaAddress (RawInstruction Address)
  } deriving (Show)

initialState :: MyState
initialState = MyState 0 Map.empty Map.empty

type SParser a = StatefulParser MyState a

incrementCurrentPos :: SParser ()
incrementCurrentPos = do
  s <- get
  when (sCurrentPos s == maxBound) empty
  put s{sCurrentPos = succ $ sCurrentPos s}

parseInstructions' :: SParser ()
parseInstructions' = sepBy parseInstruction' incrementCurrentPos >> lift eof
  where
    parseInstruction' :: SParser ()
    parseInstruction' = do
      s <- get
      let currentPos = sCurrentPos s
          knownLabels = Map.keysSet $ sLabels s
      (actualPos, instruction, labels) <- lift $ parseInstruction currentPos knownLabels
      let newLabels = Map.fromList [(l, actualPos) | l <- Set.toList labels]
      put s { sCurrentPos = actualPos
            , sLabels = Map.union newLabels $ sLabels s
            , sInstructions = Map.insert actualPos instruction $ sInstructions s
            }

parseInstructions :: Parser (Map.Map MimaLabel MimaAddress, Map.Map MimaAddress (RawInstruction Address))
parseInstructions = do
  (_, s) <- runStatefulParser parseInstructions' initialState
  pure (sLabels s, sInstructions s)

resolveRegisters :: Map.Map MimaLabel MimaAddress
                 -> Registers Address
                 -> Parser (Registers MimaAddress)
resolveRegisters labels reg = do
  iar <- resolveMaybeAddress $ regIAR reg
  ra  <- resolveMaybeAddress $ regRA reg
  sp  <- resolveMaybeAddress $ regSP reg
  fp  <- resolveMaybeAddress $ regFP reg
  pure reg{regIAR = iar, regRA = ra, regSP = sp, regFP = fp}
  where
    resolveMaybeAddress :: Maybe Address -> Parser (Maybe MimaAddress)
    resolveMaybeAddress (Just addr) = Just <$> resolveAddress labels addr
    resolveMaybeAddress Nothing     = pure Nothing

resolveRawInstruction :: Map.Map MimaLabel MimaAddress
                      -> RawInstruction Address
                      -> Parser (RawInstruction MimaAddress)
resolveRawInstruction _      (RawLIT word)               = pure $ RawLIT word
resolveRawInstruction _      (RawLargeInstruction lo sv) = pure $ RawLargeInstruction lo sv
resolveRawInstruction labels (RawSmallInstruction so lv) = do
  addr <- resolveAddress labels lv
  pure $ RawSmallInstruction so addr

resolveLabels :: Map.Map MimaLabel MimaAddress
              -> Map.Map MimaAddress (RawInstruction Address)
              -> Parser (Map.Map MimaAddress (RawInstruction MimaAddress))
resolveLabels labels rawLabeledInstructions = do
  let labeledInstrList = Map.toList rawLabeledInstructions
      resolve = resolveRawInstruction labels
  instrList <- forM labeledInstrList $ \(addr, instr) -> (addr,) <$> resolve instr
  let rawInstructions = Map.fromList instrList
  pure rawInstructions

stateFromRegisters :: Registers MimaAddress -> MimaMemory -> MimaState
stateFromRegisters reg mem =
  MimaState { msIAR = fromMaybe 0 $ regIAR reg
            , msACC = fromMaybe 0 $ regACC reg
            , msRA  = fromMaybe 0 $ regRA reg
            , msSP  = fromMaybe 0 $ regSP reg
            , msFP  = fromMaybe 0 $ regFP reg
            , msMemory = mem
            }

parseState :: Parser (MimaState, Map.Map T.Text MimaAddress)
parseState = do
  space
  unresolvedRegisters <- parseRegisters
  (labels, unresolvedRawInstructions) <- parseInstructions
  registers <- resolveRegisters labels unresolvedRegisters
  rawInstructions <- resolveLabels labels unresolvedRawInstructions
  let mem = mapToMemory $ Map.map rawInstructionToWord rawInstructions
      labelNames = Map.fromList $ map (\(k, v) -> (lName k, v)) $ Map.toList labels
  pure (stateFromRegisters registers mem, labelNames)
