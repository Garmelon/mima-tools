{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mima.State
  (
  -- * Memory
    MimaMemory
  , readAt
  , writeAt
  -- ** Converting
  , wordsToMemory
  , memoryToWords
  -- * State
  , MimaState(..)
  , initialState
  , AbortReason(..)
  , step
  , run
  , runN
  ) where

import           Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import           Mima.Instruction
import           Mima.Util
import           Mima.Word

newtype MimaMemory = MimaMemory (Map.Map MimaAddress MimaWord)
  deriving (Show)

addressRange :: MimaMemory -> [MimaAddress]
addressRange (MimaMemory m) =
  case fst <$> Map.lookupMax m of
    Nothing      -> []
    Just maxAddr -> [minBound..maxAddr]

wordsToMemory :: [MimaWord] -> MimaMemory
wordsToMemory = MimaMemory
              . Map.filter (/= zeroBits)
              . Map.fromAscList
              . zip [minBound..]

memoryToWords :: MimaMemory -> [MimaWord]
memoryToWords mem = map (\addr -> readAt addr mem) $ addressRange mem

{-
addrWordLegend :: T.Text
addrWordLegend = "SO: Small Opcode (bits 23-20)    LO: Large Opcode (bits 19-16)\n"
                 <> "Addr  (decimal)    -    Word   ( decimal|SO,LO,   Addr)    -    Instruction\n"

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
-}

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

-- | A possible initial MiMa state, where every register is
-- zeroed. Thus, execution starts at address 0x00000.
initialState :: MimaMemory -> MimaState
initialState mem = MimaState
  { msIAR    = zeroBits
  , msACC    = zeroBits
  , msRA     = zeroBits
  , msSP     = zeroBits
  , msFP     = zeroBits
  , msMemory = mem
  }

data AbortReason = Halted | InvalidInstruction T.Text | InvalidNextIarAddress
  deriving (Show)

instance ToText AbortReason where
  toText Halted = "Halted"
  toText (InvalidInstruction t) = "Invalid instruction: " <> t
  toText InvalidNextIarAddress = "Can't increment IAR: Invalid next address"

incrementIAR :: MimaState -> Either AbortReason MimaState
incrementIAR ms =
  let addr = msIAR ms
  in  if addr >= maxBound
      then Left InvalidNextIarAddress
      else Right ms{msIAR = succ addr}

wordToInstruction' :: MimaWord -> Either AbortReason Instruction
wordToInstruction' word =
  case wordToInstruction word of
    Right instruction -> Right instruction
    Left errorMsg     -> Left $ InvalidInstruction errorMsg

step :: MimaState -> Either AbortReason MimaState
step ms = do
  let word = readAt (msIAR ms) (msMemory ms)
  ms'         <- incrementIAR ms
  instruction <- wordToInstruction' word
  case instruction of
    (SmallInstruction so lv) -> pure $ doSmallOpcode so lv ms'
    (LargeInstruction lo sv) -> doLargeOpcode lo sv ms'

doSmallOpcode :: SmallOpcode -> LargeValue -> MimaState -> MimaState
doSmallOpcode LDC  lv   ms@MimaState{..} = ms{msACC = largeValueToWord lv}
doSmallOpcode LDV  addr ms@MimaState{..} = ms{msACC = readAt addr msMemory}
doSmallOpcode STV  addr ms@MimaState{..} = ms{msMemory = writeAt addr msACC msMemory}
doSmallOpcode ADD  addr ms@MimaState{..} = ms{msACC = addWords msACC $ readAt addr msMemory}
doSmallOpcode AND  addr ms@MimaState{..} = ms{msACC = msACC .&.   readAt addr msMemory}
doSmallOpcode OR   addr ms@MimaState{..} = ms{msACC = msACC .|.   readAt addr msMemory}
doSmallOpcode XOR  addr ms@MimaState{..} = ms{msACC = msACC `xor` readAt addr msMemory}
doSmallOpcode EQL  addr ms@MimaState{..} = ms{msACC = boolToWord $ msACC == readAt addr msMemory}
doSmallOpcode JMP  addr ms@MimaState{..} = ms{msIAR = addr}
doSmallOpcode JMN  addr ms@MimaState{..} = if topBit msACC then ms{msIAR = addr} else ms
doSmallOpcode LDIV addr ms@MimaState{..} =
  let indirAddr = getAddress $ readAt addr msMemory
  in  ms{msACC = readAt indirAddr msMemory}
doSmallOpcode STIV addr ms@MimaState{..} =
  let indirAddr = getAddress $ readAt addr msMemory
  in  ms{msMemory = writeAt indirAddr msACC msMemory}
doSmallOpcode CALL addr ms@MimaState{..} = ms{msRA = msIAR, msIAR = addr}
doSmallOpcode LDVR addr ms@MimaState{..} = ms{msACC = readAt (addLargeValues msSP addr) msMemory}
doSmallOpcode STVR addr ms@MimaState{..} = ms{msMemory = writeAt (addLargeValues msSP addr) msACC msMemory}

doLargeOpcode :: LargeOpcode -> SmallValue -> MimaState -> Either AbortReason MimaState
doLargeOpcode HALT _  _                = Left Halted
doLargeOpcode NOT  _  ms@MimaState{..} = pure ms{msACC = complement msACC}
doLargeOpcode RAR  _  ms@MimaState{..} = pure ms{msACC = rotateR msACC 1}
doLargeOpcode RET  _  ms@MimaState{..} = pure ms{msIAR = msRA}
doLargeOpcode LDRA _  ms@MimaState{..} = pure ms{msACC = largeValueToWord msRA}
doLargeOpcode STRA _  ms@MimaState{..} = pure ms{msRA  = getAddress msACC}
doLargeOpcode LDSP _  ms@MimaState{..} = pure ms{msACC = largeValueToWord msSP}
doLargeOpcode STSP _  ms@MimaState{..} = pure ms{msSP  = getAddress msACC}
doLargeOpcode LDFP _  ms@MimaState{..} = pure ms{msACC = largeValueToWord msFP}
doLargeOpcode STFP _  ms@MimaState{..} = pure ms{msFP  = getAddress msACC}
doLargeOpcode ADC  sv ms@MimaState{..} = pure ms{msACC = addWords msACC $ signedSmallValueToWord sv}

run :: MimaState -> (MimaState, AbortReason, Integer)
run ms = helper 0 ms
  where
    helper completed s =
      case step s of
        Left e   -> (s, e, completed)
        Right s' -> helper (completed + 1) s'

runN :: Integer -> MimaState -> (MimaState, Maybe AbortReason, Integer)
runN n ms = helper 0 ms
  where
    helper completed s =
      if completed >= n
      then (s, Nothing, completed)
      else case step s of
        Left e   -> (s, Just e, completed)
        Right s' -> helper (completed + 1) s'
