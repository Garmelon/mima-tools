{-# LANGUAGE FlexibleInstances #-}

module Mima.Load
  ( loadStateFromFile
  , saveStateToFile
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy as BS

import           Mima.Word
import           Mima.State

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

loadStateFromFile :: FilePath -> IO (Either String MimaState)
loadStateFromFile path = do
  bs <- BS.readFile path
  pure $ case decodeOrFail bs of
    Left  (  _, _, e) -> Left e
    Right (bs', _, ldms)
      | BS.null bs' -> Right $ unLD ldms
      | otherwise   -> Left "Input was not consumed fully"

saveStateToFile :: FilePath -> MimaState -> IO ()
saveStateToFile path = BS.writeFile path . encode . LD
