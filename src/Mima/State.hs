{-# LANGUAGE OverloadedStrings #-}

module Mima.State
  ( MimaMemory
  , wordsToMemory
  , memoryToWords
  , memoryToText
  , readAt
  , writeAt
  , MimaState(..)
  , initialState
  , AbortReason(..)
  , ExecException(..)
  , step
  , run
  , runN
  ) where

import           Data.Bits
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T

import           Mima.Instruction
import           Mima.Util
import           Mima.Word

newtype MimaMemory = MimaMemory (Map.Map MimaAddress MimaWord)
  deriving (Show)

addressRange :: MimaMemory -> [MimaAddress]
addressRange (MimaMemory m) =
  let maxAddr = fromMaybe minBound $ fst <$> Map.lookupMax m
  in  [minBound..maxAddr]

wordsToMemory :: [MimaWord] -> MimaMemory
wordsToMemory = MimaMemory
              . Map.filter (/= zeroBits)
              . Map.fromAscList
              . zip [minBound..]

memoryToWords :: MimaMemory -> [MimaWord]
memoryToWords mem = map (\addr -> readAt addr mem) $ addressRange mem

addrWordLegend :: T.Text
addrWordLegend = "UO: Upper Opcode (bits 24-21)     LO: Lower Opcode (bits 20-17)\n"
                 <> "Addr  (decimal)    -    Word   ( decimal|UO,LO,   Addr)    -    Instruction\n"

addrWordToText :: MimaAddress -> MimaWord -> T.Text
addrWordToText addr word =
  let separator = "    -    "
      addrText  = addrToHex addr <> " (" <> addrToDec addr <> ")"
      wordSplit = toDec 2 (upperOpcode word) <> ","
                  <> toDec 2 (lowerOpcode word) <> ","
                  <> addrToDec (address word)
      wordText  = wordToHex word <> " (" <> wordToDec word <> "|" <> wordSplit <> ")"
      instrText = case wordToInstruction word of
        Left _  -> ""
        Right i -> separator <> toText i
  in  addrText <> separator <> wordText <> instrText

memoryToText :: Bool -> MimaMemory -> T.Text
memoryToText sparse mem@(MimaMemory m)
  = (addrWordLegend <>)
  $ T.intercalate "\n"
  $ map (\addr -> addrWordToText addr (readAt addr mem))
  $ addresses sparse
  where
    addresses False = addressRange mem
    addresses True  = Map.keys m

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

instance ToText AbortReason where
  toText Halted = "Halted"
  toText (InvalidInstruction t) = "Invalid instruction: " <> t
  toText InvalidNextIpAddress = "Invalid next IP address"

data ExecException = ExecException MimaAddress MimaWord AbortReason
  deriving (Show)

instance ToText ExecException where
  toText (ExecException addr word reason) =
    "Exception at " <> addrToHexDec addr <> " with word " <> wordToHexDec word <> ": " <> toText reason

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
executeSmallOpcode LDC  addr ms = incrementIp ms{msAcc = addrToWord addr}
executeSmallOpcode LDV  addr ms = incrementIp ms{msAcc = readAt addr (msMemory ms)}
executeSmallOpcode STV  addr ms = incrementIp ms{msMemory = writeAt addr (msAcc ms) (msMemory ms)}
executeSmallOpcode ADD  addr ms = incrementIp ms{msAcc = addWords (msAcc ms) (readAt addr $ msMemory ms)}
executeSmallOpcode AND  addr ms = incrementIp ms{msAcc = msAcc ms .&.   readAt addr (msMemory ms)}
executeSmallOpcode OR   addr ms = incrementIp ms{msAcc = msAcc ms .|.   readAt addr (msMemory ms)}
executeSmallOpcode XOR  addr ms = incrementIp ms{msAcc = msAcc ms `xor` readAt addr (msMemory ms)}
executeSmallOpcode EQL  addr ms = incrementIp ms{msAcc = boolToWord $ msAcc ms == readAt addr (msMemory ms)}
executeSmallOpcode JMP  addr ms = pure ms{msIp = addr}
executeSmallOpcode JMN  addr ms = if topBit (msAcc ms) then pure ms{msIp = addr} else incrementIp ms
executeSmallOpcode STIV addr ms =
  let mem = msMemory ms
      indirAddr = address $ readAt addr mem
  in  incrementIp ms{msMemory = writeAt indirAddr (msAcc ms) mem}
executeSmallOpcode LDIV addr ms =
  let mem = msMemory ms
      indirAddr = address $ readAt addr mem
  in  incrementIp ms{msAcc = readAt indirAddr mem}

executeLargeOpcode :: LargeOpcode -> MimaState -> Either ExecException MimaState
executeLargeOpcode HALT ms =
  let addr = msIp ms
      word = readAt addr (msMemory ms)
  in   Left $ ExecException addr word Halted
executeLargeOpcode NOT  ms = incrementIp ms{msAcc = complement (msAcc ms)}
executeLargeOpcode RAR  ms = incrementIp ms{msAcc = rotateR (msAcc ms) 1}

run :: MimaState -> (MimaState, ExecException, Integer)
run ms = helper 0 ms
  where
    helper completed s =
      case step s of
        Left e   -> (s, e, completed)
        Right s' -> helper (completed + 1) s'

runN :: Integer -> MimaState -> (MimaState, Maybe ExecException, Integer)
runN n ms = helper 0 ms
  where
    helper completed s =
      if completed >= n
      then (s, Nothing, completed)
      else case step s of
        Left e   -> (s, Just e, completed)
        Right s' -> helper (completed + 1) s'
