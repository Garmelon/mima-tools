{-# LANGUAGE FlexibleInstances #-}

module Mima.Load
  ( loadStateFromFile
  , saveStateToFile
  ) where

import           Control.Applicative
import           Data.Binary
import qualified Data.ByteString.Lazy as BS

import           Mima.Word
import           Mima.State

-- To prevent orphan instances and keep the compiler happy
newtype LD t = LD { unLD :: t }

instance Binary (LD (WB MimaWord_)) where
  put mw = do
    let (w1, w2, w3) = wordToBytes $ unLD mw
    put w1
    put w2
    put w3
  get = do
    w1 <- get
    w2 <- get
    w3 <- get
    pure $ LD $ bytesToWord w1 w2 w3

instance Binary (LD (WB LargeValue_)) where
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
