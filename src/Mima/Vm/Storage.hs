module Mima.Vm.Storage
  (
  -- * Methods for loading/storing Metadata
    loadMetadata
  , saveMetadata
  -- * Test methods
  , roundTripFile
  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text                as T
import           Mima.Run
import           Mima.Vm.Metadata

-- | Loads 'Metadata' from a given file path
loadMetadata :: FilePath -> Run Metadata
loadMetadata path = do
  file <- readFileBS path
  let decoded = eitherDecode (BSL.fromStrict file) :: Either String Metadata
  case decoded of
    Left msg       -> throw (T.pack msg)
    Right metadata -> pure metadata

-- | Stores prettified 'Metadata' in a given file.
saveMetadata :: FilePath -> Metadata -> Run ()
saveMetadata path metadata = writeFileBS path (BSL.toStrict (encodePretty metadata))

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
