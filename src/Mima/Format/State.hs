{-# LANGUAGE OverloadedStrings #-}

module Mima.Format.State
  ( FormatConfig(..)
  , FormatEnv(..)
  , FormatReader
  , Formatter
  -- * Flags
  , fRegisterFlags
  , fMemoryFlags
  , fFlags
  -- * Addresses
  , fAddress
  -- * Words
  , fWord
  -- * Memory
  , fMemory
  -- * Registers
  , fRegisters
  -- * The whole state
  , formatState
  ) where

import           Control.Monad.Trans.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Mima.Flag
import           Mima.Format.Common
import           Mima.Format.Instruction
import           Mima.Instruction
import           Mima.Label
import           Mima.State
import           Mima.Word

data FormatConfig = FormatConfig
  { fcSparse            :: Bool
  , fcShowRegisters     :: Bool
  , fcShowMemory        :: Bool
  , fcShowMemoryFlags   :: Bool
  , fcShowRegisterFlags :: Bool
  , fcShowAddressDec    :: Bool
  , fcShowAddressHex    :: Bool
  , fcShowAddressBin    :: Bool
  , fcShowWordDec       :: Bool
  , fcShowWordHex       :: Bool
  , fcShowWordBin       :: Bool
  , fcShowInstructions  :: Bool -- Currently unused
  , fcShowLabels        :: Bool -- Currently unused
  } deriving (Show)

data FormatEnv = FormatEnv
  { feState  :: MimaState
  , feFlags  :: Flags (MimaAddress -> Bool)
  , feLabels :: Map.Map MimaAddress (Set.Set LabelName)
  , feConf   :: FormatConfig
  }

type FormatReader a = Reader FormatEnv a
type Formatter = FormatReader T.Text

{- Flags -}

flagAt :: (MimaState -> MimaAddress) -> Char -> MimaState -> MimaAddress -> T.Text
flagAt f c s a = T.singleton $ if f s == a then c else ' '

iarFlag :: MimaState -> MimaAddress -> T.Text
iarFlag = flagAt msIAR '>'

raFlag :: MimaState -> MimaAddress -> T.Text
raFlag = flagAt msRA 'R'

spFlag :: MimaState -> MimaAddress -> T.Text
spFlag = flagAt msSP 'S'

fpFlag :: MimaState -> MimaAddress -> T.Text
fpFlag = flagAt msFP 'F'

fRegisterFlags :: MimaState -> MimaAddress -> T.Text
fRegisterFlags s a = mconcat $ [fpFlag, spFlag, raFlag, iarFlag] <*> pure s <*> pure a

fMemoryFlags :: Flags (MimaAddress -> Bool) -> MimaAddress -> T.Text
fMemoryFlags flags a =
  let b = if flagBreakpoint flags a then 'b' else ' '
      e = if flagExecutable flags a then 'e' else ' '
      r = if flagReadOnly   flags a then 'r' else ' '
  in  T.pack [b, e, r]

fFlags :: MimaAddress -> Formatter
fFlags a = do
  env <- ask
  let conf = feConf env
      s = feState env
      f = feFlags env
      memoryFlags   = if fcShowMemoryFlags   conf then fMemoryFlags   f a else ""
      registerFlags = if fcShowRegisterFlags conf then fRegisterFlags s a else ""
      space = if fcShowMemoryFlags conf || fcShowRegisterFlags conf then " " else ""
  pure $ memoryFlags <> registerFlags <> space

{- Addresses -}

fAddressBin :: MimaAddress -> T.Text
fAddressBin = chunkyBin . fixWidthBin (4 * 5) . toBin

fAddressDec :: MimaAddress -> T.Text
fAddressDec = fixWidthDec 9 . chunkyDec . toDec

fAddressHex :: MimaAddress -> T.Text
fAddressHex = chunkyHex . fixWidthHex 5 . toHex

fAddress :: MimaAddress -> Formatter
fAddress a = do
  env <- ask
  let conf = feConf env
      dec = if fcShowAddressDec conf then [fAddressDec] else []
      hex = if fcShowAddressHex conf then [fAddressHex] else []
      bin = if fcShowAddressBin conf then [fAddressBin] else []
      formats = (dec ++ hex ++ bin) <*> pure a
  pure $ "[" <> T.intercalate ", " formats <> "]"

{- Words -}

fWordBin :: MimaWord -> T.Text
fWordBin = chunkyBin . fixWidthBin (4 * 6) . toBin

fWordDec :: MimaWord -> T.Text
fWordDec = fixWidthDec 10 . chunkyDec . toDec

fWordHex :: MimaWord -> T.Text
fWordHex = chunkyHex . fixWidthHex 6 . toHex

fWord :: MimaWord -> Formatter
fWord a = do
  env <- ask
  let conf = feConf env
      dec = if fcShowWordDec conf then [fWordDec] else []
      hex = if fcShowWordHex conf then [fWordHex] else []
      bin = if fcShowWordBin conf then [fWordBin] else []
      formats = (dec ++ hex ++ bin) <*> pure a
  pure $ "{" <> T.intercalate ", " formats <> "}"

{- Instructions and Labels -}

fLabels :: Set.Set LabelName -> T.Text
fLabels = mconcat . map (<> ": ") . Set.toAscList

fDecoration :: MimaAddress -> Formatter
fDecoration a = do
  env <- ask
  let conf = feConf env
      -- Labels
      labels = Map.findWithDefault Set.empty a $ feLabels env
      labelsStr = if fcShowLabels conf then fLabels labels else ""
      -- Instruction
      word = readAt a $ msMemory $ feState env
      instrStr = case wordToInstruction word of
        Left _  -> ""
        Right i -> if fcShowInstructions conf then formatInstruction i else ""
  pure $ labelsStr <> instrStr

{- Memory -}

fMemoryLn :: MimaAddress -> Formatter
fMemoryLn a = do
  env <- ask
  let mem = msMemory $ feState env
      w = readAt a mem
  flags <- fFlags a
  addr <- fAddress a
  word <- fWord w
  deco <- fDecoration a
  pure $ flags <> addr <> " " <> word <> " " <> deco <> "\n"

interestingAddresses :: FormatReader (Set.Set MimaAddress)
interestingAddresses = do
  env <- ask
  let conf = feConf env
      s = feState env
  pure $ if fcShowRegisterFlags conf
    then Set.fromList [msIAR s, msRA s, msSP s, msFP s]
    else Set.empty
  
getAddresses :: FormatReader [MimaAddress]
getAddresses = do
  env <- ask
  let conf = feConf env
      mem = msMemory $ feState env
  if fcSparse conf
    then do
      interesting <- interestingAddresses
      pure $ Set.toAscList $ Set.union interesting $ Set.fromList $ usedAddresses mem
    else pure $ continuousUsedAddresses mem

fMemory :: Formatter
fMemory = do
  addrs <- getAddresses
  mconcat <$> mapM fMemoryLn addrs

{- Registers -}

fAddressRegister :: T.Text -> MimaAddress -> Formatter
fAddressRegister name addr = do
  addrText <- fAddress addr
  pure $ name <> ": " <> addrText <> "\n"

fWordRegister :: T.Text -> MimaWord -> Formatter
fWordRegister name word = do
  wordText <- fWord word
  pure $ name <> ": " <> wordText <> "\n"

fRegisters :: Formatter
fRegisters = do
  env <- ask
  let s = feState env
  mconcat <$> sequenceA [ fAddressRegister "IAR" (msIAR s)
                        , fWordRegister    "ACC" (msACC s)
                        , fAddressRegister " RA" (msRA s)
                        , fAddressRegister " SP" (msSP s)
                        , fAddressRegister " FP" (msFP s)
                        ]

{- And finally, the whole state -}

fState :: Formatter
fState = do
  env <- ask
  let conf = feConf env
  regText <- ("--< REGISTERS >--\n" <>) <$> fRegisters
  memText <- ("--< MEMORY >--\n"    <>) <$> fMemory
  pure $ (if fcShowRegisters conf then regText else "")
    <> (if fcShowMemory conf then memText else "")

formatState :: FormatEnv -> T.Text
formatState = runReader fState
