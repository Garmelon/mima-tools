{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Mima.Vm.State
  ( MimaState(..)
  , basicState
  , AbortReason(..)
  , step
  , run
  , runN
  ) where

import           Data.Bits
import qualified Data.Text           as T

import           Mima.Format
import           Mima.Vm.Instruction
import           Mima.Vm.Memory
import           Mima.Vm.Word

data MimaState = MimaState
  { msIar    :: !MimaAddress
  , msAcc    :: !MimaWord
  , msRa     :: !MimaAddress
  , msSp     :: !MimaAddress
  , msFp     :: !MimaAddress
  , msMemory :: !MimaMemory
  } deriving (Show)

basicState :: MimaMemory -> MimaState
basicState = MimaState zeroBits zeroBits zeroBits zeroBits zeroBits

data AbortReason
  = Halted
  | InvalidInstruction T.Text
  | InvalidNextIarAddress
  | AddressNotExecutable
  | AddressReadOnly
  deriving (Show)

instance ToText AbortReason where
  toText Halted                 = "Halted"
  toText (InvalidInstruction t) = "Exception: Invalid instruction: " <> t
  toText InvalidNextIarAddress  = "Exception: Can't increment IAR: Invalid next address"
  toText AddressNotExecutable   = "Exception: Address is not flagged as excutable"
  toText AddressReadOnly        = "Exception: Address is flagged as read-only"

{- A fancy monad that helps with stepping the MimaState -}

type Execution a = Either AbortReason a

incrementIar :: MimaState -> Execution MimaState
incrementIar ms
  | addr >= maxBound = Left InvalidNextIarAddress
  | otherwise = pure ms{msIar = succ addr}
  where
    addr = msIar ms

decodeInstruction :: MimaWord -> Execution Instruction
decodeInstruction word =
  case wordToInstruction word of
    Right instruction -> pure instruction
    Left errorMsg     -> Left $ InvalidInstruction errorMsg

storeValue :: MimaAddress -> MimaState -> Execution MimaState
storeValue addr ms = pure ms{msMemory = writeAt addr (msAcc ms) (msMemory ms)}

loadValue :: MimaAddress -> MimaState -> Execution MimaState
loadValue addr ms = pure ms{msAcc = readAt addr (msMemory ms)}

accOperation :: (MimaWord -> MimaWord -> MimaWord) -> MimaAddress -> MimaState -> Execution MimaState
accOperation f addr ms = pure ms{msAcc = f (msAcc ms) $ readAt addr (msMemory ms)}

doSmallOpcode :: SmallOpcode -> LargeValue -> MimaState -> Execution MimaState
doSmallOpcode LDC  lv   ms@MimaState{..} = pure ms{msAcc = largeValueToWord lv}
doSmallOpcode LDV  addr ms               = loadValue addr ms
doSmallOpcode STV  addr ms               = storeValue addr ms
doSmallOpcode ADD  addr ms@MimaState{..} = accOperation (+) addr ms
doSmallOpcode AND  addr ms@MimaState{..} = accOperation (.&.) addr ms
doSmallOpcode OR   addr ms@MimaState{..} = accOperation (.|.) addr ms
doSmallOpcode XOR  addr ms@MimaState{..} = accOperation xor addr ms
doSmallOpcode EQL  addr ms@MimaState{..} = accOperation (\a b -> boolToWord $ a == b) addr ms
doSmallOpcode JMP  addr ms@MimaState{..} = pure ms{msIar = addr}
doSmallOpcode JMN  addr ms@MimaState{..} = pure $ if topBit msAcc then ms{msIar = addr} else ms
doSmallOpcode LDIV addr ms@MimaState{..} = loadValue (getLargeValue $ readAt addr msMemory) ms
doSmallOpcode STIV addr ms@MimaState{..} = storeValue (getLargeValue $ readAt addr msMemory) ms
doSmallOpcode CALL addr ms@MimaState{..} = pure ms{msRa = msIar, msIar = addr}
doSmallOpcode ADC  lv   ms@MimaState{..} = pure ms{msAcc = msAcc + signedLargeValueToWord lv}

doLargeOpcode :: LargeOpcode -> SmallValue -> MimaState -> Execution MimaState
doLargeOpcode HALT _  _                = Left Halted
doLargeOpcode NOT  _  ms@MimaState{..} = pure ms{msAcc = complement msAcc}
doLargeOpcode RAR  _  ms@MimaState{..} = pure ms{msAcc = rotateR msAcc 1}
doLargeOpcode RET  _  ms@MimaState{..} = pure ms{msIar = msRa}
doLargeOpcode LDRA _  ms@MimaState{..} = pure ms{msAcc = largeValueToWord msRa}
doLargeOpcode STRA _  ms@MimaState{..} = pure ms{msRa  = getLargeValue msAcc}
doLargeOpcode LDSP _  ms@MimaState{..} = pure ms{msAcc = largeValueToWord msSp}
doLargeOpcode STSP _  ms@MimaState{..} = pure ms{msSp  = getLargeValue msAcc}
doLargeOpcode LDFP _  ms@MimaState{..} = pure ms{msAcc = largeValueToWord msFp}
doLargeOpcode STFP _  ms@MimaState{..} = pure ms{msFp  = getLargeValue msAcc}
doLargeOpcode LDRS sv ms@MimaState{..} = loadValue  (msSp + signedSmallValueToLargeValue sv) ms
doLargeOpcode STRS sv ms@MimaState{..} = storeValue (msSp + signedSmallValueToLargeValue sv) ms
doLargeOpcode LDRF sv ms@MimaState{..} = loadValue  (msFp + signedSmallValueToLargeValue sv) ms
doLargeOpcode STRF sv ms@MimaState{..} = storeValue (msFp + signedSmallValueToLargeValue sv) ms

step :: MimaState -> Execution MimaState
step ms = do
  let addr = msIar ms
      word = readAt addr (msMemory ms)
  instruction <- decodeInstruction word
  ms' <- incrementIar ms
  case instruction of
    (SmallInstruction so lv) -> doSmallOpcode so lv ms'
    (LargeInstruction lo sv) -> doLargeOpcode lo sv ms'

run :: MimaState -> (MimaState, AbortReason, Integer)
run = helper 0
  where
    helper completed s =
      case step s of
        Left e   -> (s, e, completed)
        Right s' -> helper (completed + 1) s'

runN :: Integer -> MimaState -> (MimaState, Maybe AbortReason, Integer)
runN n = helper 0
  where
    helper completed s =
      if completed >= n
      then (s, Nothing, completed)
      else case step s of
        Left e   -> (s, Just e, completed)
        Right s' -> helper (completed + 1) s'
