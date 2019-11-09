{-# LANGUAGE TupleSections #-}

module Mima.Assembler.Parser
  ( parseState
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Text.Megaparsec

import           Mima.Assembler.Parser.Basic
import           Mima.Assembler.Parser.Instruction
import           Mima.Assembler.Parser.Label
import           Mima.Assembler.Parser.RawInstruction
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
parseInstructions' = sepBy parseInstruction' incrementCurrentPos >> lift (eof <|> fail atMaxAddress)
  where
    atMaxAddress = "already at maximum address (" ++ show (maxBound :: MimaAddress)
                   ++ ") - can't go any further"

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

parseState :: Parser MimaState
parseState = do
  space
  (labels, rawLabeledInstructions) <- parseInstructions
  rawInstructions <- resolveLabels labels rawLabeledInstructions
  let mem = mapToMemory $ Map.map rawInstructionToWord rawInstructions
  pure $ basicState mem
