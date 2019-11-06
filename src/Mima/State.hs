module Mima.State
  ( MimaMemory
  , wordsToMemory
  , memoryToWords
  , readAt
  , writeAt
  , MimaState(..)
  , initialState
  , AbortReason(..)
  , ExecException(..)
  , step
  ) where

import           Data.Bits
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T

import           Mima.Instruction
import           Mima.Word

newtype MimaMemory = MimaMemory (Map.Map MimaAddress MimaWord)
  deriving (Show)

wordsToMemory :: [MimaWord] -> MimaMemory
wordsToMemory = MimaMemory . Map.fromAscList . zip [minBound..]

memoryToWords :: MimaMemory -> [MimaWord]
memoryToWords mem@(MimaMemory m) =
  let maxAddr = fromMaybe minBound $ fst <$> Map.lookupMax m
  in  map (\addr -> readAt addr mem) [minBound..maxAddr]

readAt :: MimaAddress -> MimaMemory -> MimaWord
readAt addr (MimaMemory m) = Map.findWithDefault zeroBits addr m

writeAt :: MimaAddress -> MimaWord -> MimaMemory -> MimaMemory
writeAt addr word (MimaMemory m)
  | word == zeroBits = MimaMemory $ Map.delete addr m
  | otherwise        = MimaMemory $ Map.insert addr word m

data MimaState = MimaState
  { msIp     :: !MimaAddress
  , msAcc    :: !MimaWord
  , msMemory :: !MimaMemory
  } deriving (Show)

initialState :: MimaMemory -> MimaState
initialState mem = MimaState
  { msIp     = minBound
  , msAcc    = zeroBits
  , msMemory = mem
  }

data AbortReason = Halted | InvalidInstruction T.Text | InvalidNextIpAddress
  deriving (Show)

data ExecException = ExecException MimaAddress MimaWord AbortReason
  deriving (Show)

incrementIp :: MimaState -> Either ExecException MimaState
incrementIp ms =
  let addr = msIp ms
  in  if addr >= maxBound
      then Left $ ExecException addr (readAt addr $ msMemory ms) InvalidNextIpAddress
      else pure ms{msIp = succ addr}

wordToInstruction' :: MimaAddress -> MimaWord -> Either ExecException Instruction
wordToInstruction' addr word =
  case wordToInstruction word of
    Right instruction -> pure instruction
    Left errorMsg     -> Left $ ExecException addr word $ InvalidInstruction errorMsg

step :: MimaState -> Either ExecException MimaState
step ms = do
  let addr = msIp ms
      word = readAt addr (msMemory ms)
  instruction <- wordToInstruction' addr word
  case instruction of
    (SmallInstruction oc instrAddr) -> executeSmallOpcode oc instrAddr ms
    (LargeInstruction oc)           -> executeLargeOpcode oc ms

executeSmallOpcode :: SmallOpcode -> MimaAddress -> MimaState -> Either ExecException MimaState
executeSmallOpcode LDC addr ms = incrementIp ms{msAcc = addressToWord addr}
executeSmallOpcode LDV addr ms = incrementIp ms{msAcc = readAt addr (msMemory ms)}
executeSmallOpcode STV addr ms = incrementIp ms{msMemory = writeAt addr (msAcc ms) (msMemory ms)}
executeSmallOpcode ADD addr ms = incrementIp ms{msAcc = addWords (msAcc ms) (readAt addr $ msMemory ms)}
executeSmallOpcode AND addr ms = incrementIp ms{msAcc = msAcc ms .&.   readAt addr (msMemory ms)}
executeSmallOpcode OR  addr ms = incrementIp ms{msAcc = msAcc ms .|.   readAt addr (msMemory ms)}
executeSmallOpcode XOR addr ms = incrementIp ms{msAcc = msAcc ms `xor` readAt addr (msMemory ms)}
executeSmallOpcode EQL addr ms = incrementIp ms{msAcc = boolToWord $ msAcc ms == readAt addr (msMemory ms)}
executeSmallOpcode JMP addr ms = pure ms{msIp = addr}
executeSmallOpcode JMN addr ms = if topBit (msAcc ms) then pure ms{msIp = addr} else incrementIp ms

executeLargeOpcode :: LargeOpcode -> MimaState -> Either ExecException MimaState
executeLargeOpcode HALT ms =
  let addr = msIp ms
      word = readAt addr (msMemory ms)
  in   Left $ ExecException addr word Halted
executeLargeOpcode NOT  ms = incrementIp ms{msAcc = complement (msAcc ms)}
executeLargeOpcode RAR  ms = incrementIp ms{msAcc = rotateR (msAcc ms) 1}
