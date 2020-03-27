{-# LANGUAGE RecordWildCards #-}

module Mima.Vm.Storage
  (
  -- * Methods for loading/storing 'Metadata'
    loadMetadata
  , saveMetadata
  , saveMimaState
  -- * Test methods
  , roundTripFile
  , saveInterestingState
  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Mima.Run
import           Mima.Vm.Memory
import           Mima.Vm.Metadata
import           Mima.Vm.State
import           Mima.Vm.Word

-- | Loads 'Metadata' from a given file path.
loadMetadata :: FilePath -> Run Metadata
loadMetadata path = do
  file <- readFileBS path
  case eitherDecode $ BSL.fromStrict file of
    Left msg       -> throw (T.pack msg)
    Right metadata -> pure metadata

-- | Stores prettified 'Metadata' in a given file.
saveMetadata :: FilePath -> Metadata -> Run ()
saveMetadata path metadata = writeFileBS path (BSL.toStrict (encodePretty metadata))

saveMimaState :: FilePath -> MimaState -> Run ()
saveMimaState path state = do
  let stateBS = saveMimaSateBS state
  writeFileBS path (BSL.toStrict stateBS)

saveMimaSateBS :: MimaState -> BSL.ByteString
saveMimaSateBS state = mimaRegistersToBS state <> mimaMemoryToBS (msMemory state)

mimaRegistersToBS :: MimaState -> BSL.ByteString
mimaRegistersToBS MimaState{..}
  =  mimaWordToBS (largeValueToWord msIar)
  <> mimaWordToBS                   msAcc
  <> mimaWordToBS (largeValueToWord msRa)
  <> mimaWordToBS (largeValueToWord msSp)
  <> mimaWordToBS (largeValueToWord msFp)

mimaMemoryToBS :: MimaMemory -> BSL.ByteString
mimaMemoryToBS memory = foldl appendWord mempty (memoryToWords memory)
  where
    appendWord string word = string <> mimaWordToBS word

mimaWordToBS :: MimaWord -> BSL.ByteString
mimaWordToBS = BSL.pack . tripleToList . wordToBytes
  where
    tripleToList (a, b, c) = [a, b, c]

-- | A garbage test method that resds the input file, parses it and writes the
--   prettified result back in the output file.
--
-- Can be used with the example file:
--
-- > roundTripFile "test/files/SimpleMetadataFile.json" "/tmp/test.json"
roundTripFile :: FilePath -- ^ The input file
              -> FilePath -- ^ The output file
              -> Run ()
roundTripFile input output = loadMetadata input >>= saveMetadata output

saveInterestingState :: FilePath -> Run ()
saveInterestingState path = saveMimaState path withRegisters
  where
    state = basicState $ mapToMemory $ Map.fromList $ zip [1..100] [1..100]
    withRegisters = state{msIar = 5, msAcc = -2, msRa = 46565, msSp = 20}
