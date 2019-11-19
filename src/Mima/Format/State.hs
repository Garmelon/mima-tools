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
  -- * The whole state
  , formatState
  ) where

import           Control.Monad.Trans.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import           Mima.Flag
import           Mima.Format.Common
import           Mima.Label
import           Mima.State
import           Mima.Word

data FormatConfig = FormatConfig
  { fcSparse            :: Bool
  , fcShowRegisterFlags :: Bool
  , fcShowMemoryFlags   :: Bool
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
  pure $ memoryFlags <> registerFlags

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
  pure $ "[" <> T.intercalate "," formats <> "]"

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
  pure $ "{" <> T.intercalate "," formats <> "}"

{- Memory -}

fMemoryLn :: MimaAddress -> Formatter
fMemoryLn a = do
  env <- ask
  let mem = msMemory $ feState env
      w = readAt a mem
  flags <- fFlags a
  addr <- fAddress a
  word <- fWord w
  pure $ flags <> " " <> addr <> " " <> word <> "\n"

fMemory :: Formatter
fMemory = do
  env <- ask
  let conf = feConf env
      mem = msMemory $ feState env
      addrs = if fcSparse conf then sparseUsedAddresses mem else usedAddresses mem 
  mconcat <$> mapM fMemoryLn addrs

{- And finally, the whole state -}

fState :: Formatter
fState = fMemory

formatState :: FormatEnv -> T.Text
formatState = runReader fState
