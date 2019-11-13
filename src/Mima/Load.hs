{-# LANGUAGE FlexibleInstances #-}

module Mima.Load
  ( loadStateFromFile
  , saveStateToFile
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Binary
import qualified Data.ByteString.Lazy as BS

import           Mima.IO
import           Mima.State
import           Mima.Word

-- To prevent orphan instances and keep the compiler happy
newtype LD t = LD { unLD :: t }

instance Binary (LD MimaWord) where
  put mw = do
    let (w1, w2, w3) = wordToBytes $ unLD mw
    forM_ [w1, w2, w3] put
  get = do
    bytes <- (,,) <$> get <*> get <*> get
    pure $ LD $ bytesToWord bytes

instance Binary (LD LargeValue) where
  put = put . LD . largeValueToWord . unLD
  get = (LD . getLargeValue) <$> unLD <$> get

instance Binary (LD MimaMemory) where
  put = mapM_ (put . LD) . memoryToWords . unLD
  get = (LD . wordsToMemory . map unLD) <$> many get

instance Binary (LD MimaState) where
  put ldms = do
    let ms = unLD ldms
    put $ LD $ msIAR ms
    put $ LD $ msACC ms
    put $ LD $ msRA ms
    put $ LD $ msSP ms
    put $ LD $ msFP ms
    put $ LD $ msMemory ms
  get = do
    iar <- unLD <$> get
    acc <- unLD <$> get
    ra  <- unLD <$> get
    sp  <- unLD <$> get
    fp  <- unLD <$> get
    mem <- unLD <$> get
    pure $ LD $ MimaState iar acc ra sp fp mem

loadStateFromFile :: FilePath -> Run MimaState
loadStateFromFile path = do
  bs <- lift $ BS.readFile path
  case decodeOrFail bs of
    Left  (  _, _, e) -> failWith e
    Right (bs', _, ldms)
      | BS.null bs' -> pure $ unLD ldms
      | otherwise   -> failWith "Input was not consumed fully"

saveStateToFile :: FilePath -> MimaState -> Run ()
saveStateToFile path = lift . BS.writeFile path . encode . LD
