module Mima.Load
  ( loadMemoryFromFile
  , saveMemoryToFile
  ) where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Word

import           Mima.Word
import           Mima.State

-- These two functions are implemented with explicit recursion. The
-- first because it was easier to write that way, and the second in
-- the hopes of better performance regarding list concatenation.

bytesToWords :: [Word8] -> [MimaWord]
bytesToWords (w1:w2:w3:ws) =  bytesToWord w1 w2 w3 : bytesToWords ws
bytesToWords [w1,w2]       = [bytesToWord w1 w2       zeroBits]
bytesToWords [w1]          = [bytesToWord w1 zeroBits zeroBits]
bytesToWords []            = []

wordsToBytes :: [MimaWord] -> [Word8]
wordsToBytes []     = []
wordsToBytes (w:ws) =
  let (w1, w2, w3) = wordToBytes w
  in  w1 : w2 : w3 : wordsToBytes ws

bsToWords :: BS.ByteString -> [MimaWord]
bsToWords = bytesToWords . BS.unpack

wordsToBs :: [MimaWord] -> BS.ByteString
wordsToBs = BS.pack . wordsToBytes

loadMemoryFromFile :: FilePath -> IO MimaMemory
loadMemoryFromFile path = (wordsToMemory . bsToWords) <$> BS.readFile path

saveMemoryToFile :: FilePath -> MimaMemory -> IO ()
saveMemoryToFile path = BS.writeFile path . wordsToBs . memoryToWords
