{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mima.State
  ( MimaMemory
  , mapToMemory
  , wordsToMemory
  , memoryToWords
  , maxAddress
  , usedAddresses
  , continuousUsedAddresses
  , readAt
  , writeAt
  , MimaState(..)
  , basicState
  , AbortReason(..)
  , step
  , run
  , runN
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import           Mima.Flag
import           Mima.Instruction
import           Mima.Util
import           Mima.Word

newtype MimaMemory = MimaMemory (Map.Map MimaAddress MimaWord)
  deriving (Show)

mapToMemory :: Map.Map MimaAddress MimaWord -> MimaMemory
mapToMemory = MimaMemory . Map.filter (/= zeroBits)

wordsToMemory :: [MimaWord] -> MimaMemory
wordsToMemory = mapToMemory
              . Map.fromAscList
              . zip [minBound..]

memoryToWords :: MimaMemory -> [MimaWord]
memoryToWords mem = map (`readAt` mem) $ continuousUsedAddresses mem

maxAddress :: MimaMemory -> MimaAddress
maxAddress (MimaMemory m) = maybe minBound fst $ Map.lookupMax m

usedAddresses :: MimaMemory -> [MimaAddress]
usedAddresses (MimaMemory m) = Map.keys m

continuousUsedAddresses :: MimaMemory -> [MimaAddress]
continuousUsedAddresses mem = [minBound..maxAddress mem]

readAt :: MimaAddress -> MimaMemory -> MimaWord
readAt addr (MimaMemory m) = Map.findWithDefault zeroBits addr m

writeAt :: MimaAddress -> MimaWord -> MimaMemory -> MimaMemory
writeAt addr word (MimaMemory m)
  | word == zeroBits = MimaMemory $ Map.delete addr m
  | otherwise        = MimaMemory $ Map.insert addr word m

data MimaState = MimaState
  { msIAR    :: !MimaAddress
  , msACC    :: !MimaWord
  , msRA     :: !MimaAddress
  , msSP     :: !MimaAddress
  , msFP     :: !MimaAddress
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

type Execution a = ReaderT (Flags (MimaAddress -> Bool)) (Except AbortReason) a

runExecution :: Flags (MimaAddress -> Bool) -> Execution a -> Either AbortReason a
runExecution f exec = runExcept $ runReaderT exec f

failWith :: AbortReason -> Execution a
failWith = lift . throwE

incrementIAR :: MimaState -> Execution MimaState
incrementIAR ms =
  let addr = msIAR ms
  in  if addr >= maxBound
      then failWith InvalidNextIarAddress
      else pure ms{msIAR = succ addr}

decodeInstruction :: MimaWord -> Execution Instruction
decodeInstruction word =
  case wordToInstruction word of
    Right instruction -> pure instruction
    Left errorMsg     -> failWith $ InvalidInstruction errorMsg

storeValue :: MimaAddress -> MimaState -> Execution MimaState
storeValue addr ms = do
  flags <- ask
  if flagReadOnly flags addr
    then failWith AddressReadOnly
    else pure ms{msMemory = writeAt addr (msACC ms) (msMemory ms)}

loadValue :: MimaAddress -> MimaState -> Execution MimaState
loadValue addr ms = pure ms{msACC = readAt addr (msMemory ms)}

accOperation :: (MimaWord -> MimaWord -> MimaWord) -> MimaAddress -> MimaState -> Execution MimaState
accOperation f addr ms = pure ms{msACC = f (msACC ms) $ readAt addr (msMemory ms)}

doSmallOpcode :: SmallOpcode -> LargeValue -> MimaState -> Execution MimaState
doSmallOpcode LDC  lv   ms@MimaState{..} = pure ms{msACC = largeValueToWord lv}
doSmallOpcode LDV  addr ms               = loadValue addr ms
doSmallOpcode STV  addr ms               = storeValue addr ms
doSmallOpcode ADD  addr ms@MimaState{..} = accOperation (+) addr ms
doSmallOpcode AND  addr ms@MimaState{..} = accOperation (.&.) addr ms
doSmallOpcode OR   addr ms@MimaState{..} = accOperation (.|.) addr ms
doSmallOpcode XOR  addr ms@MimaState{..} = accOperation xor addr ms
doSmallOpcode EQL  addr ms@MimaState{..} = accOperation (\a b -> boolToWord $ a == b) addr ms
doSmallOpcode JMP  addr ms@MimaState{..} = pure ms{msIAR = addr}
doSmallOpcode JMN  addr ms@MimaState{..} = pure $ if topBit msACC then ms{msIAR = addr} else ms
doSmallOpcode LDIV addr ms@MimaState{..} = loadValue (getLargeValue $ readAt addr msMemory) ms
doSmallOpcode STIV addr ms@MimaState{..} = storeValue (getLargeValue $ readAt addr msMemory) ms
doSmallOpcode CALL addr ms@MimaState{..} = pure ms{msRA = msIAR, msIAR = addr}
doSmallOpcode ADC  lv   ms@MimaState{..} = pure ms{msACC = msACC + signedLargeValueToWord lv}

doLargeOpcode :: LargeOpcode -> SmallValue -> MimaState -> Execution MimaState
doLargeOpcode HALT _  _                = failWith Halted
doLargeOpcode NOT  _  ms@MimaState{..} = pure ms{msACC = complement msACC}
doLargeOpcode RAR  _  ms@MimaState{..} = pure ms{msACC = rotateR msACC 1}
doLargeOpcode RET  _  ms@MimaState{..} = pure ms{msIAR = msRA}
doLargeOpcode LDRA _  ms@MimaState{..} = pure ms{msACC = largeValueToWord msRA}
doLargeOpcode STRA _  ms@MimaState{..} = pure ms{msRA  = getLargeValue msACC}
doLargeOpcode LDSP _  ms@MimaState{..} = pure ms{msACC = largeValueToWord msSP}
doLargeOpcode STSP _  ms@MimaState{..} = pure ms{msSP  = getLargeValue msACC}
doLargeOpcode LDFP _  ms@MimaState{..} = pure ms{msACC = largeValueToWord msFP}
doLargeOpcode STFP _  ms@MimaState{..} = pure ms{msFP  = getLargeValue msACC}
doLargeOpcode LDRS sv ms@MimaState{..} = loadValue (msSP + signedSmallValueToLargeValue sv) ms
doLargeOpcode STRS sv ms@MimaState{..} = storeValue (msSP + signedSmallValueToLargeValue sv) ms
doLargeOpcode LDRF sv ms@MimaState{..} = loadValue (msFP + signedSmallValueToLargeValue sv) ms
doLargeOpcode STRF sv ms@MimaState{..} = storeValue (msFP + signedSmallValueToLargeValue sv) ms

step :: MimaState -> Execution MimaState
step ms = do
  let addr = msIAR ms
  flags <- ask
  unless (flagExecutable flags addr) $ failWith AddressNotExecutable

  let word = readAt addr (msMemory ms)
  instruction <- decodeInstruction word

  ms' <- incrementIAR ms

  case instruction of
    (SmallInstruction so lv) -> doSmallOpcode so lv ms'
    (LargeInstruction lo sv) -> doLargeOpcode lo sv ms'

step' :: Flags (MimaAddress -> Bool) -> MimaState -> Either AbortReason MimaState
step' flags ms = runExecution flags $ step ms

run :: Flags (MimaAddress -> Bool) -> MimaState -> (MimaState, AbortReason, Integer)
run f = helper 0
  where
    helper completed s =
      case step' f s of
        Left e   -> (s, e, completed)
        Right s' -> helper (completed + 1) s'

runN :: Flags (MimaAddress -> Bool) -> Integer -> MimaState -> (MimaState, Maybe AbortReason, Integer)
runN f n = helper 0
  where
    helper completed s =
      if completed >= n
      then (s, Nothing, completed)
      else case step' f s of
        Left e   -> (s, Just e, completed)
        Right s' -> helper (completed + 1) s'
