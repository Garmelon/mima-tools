{-# LANGUAGE OverloadedStrings #-}

module Mima.Vm.Storage
  ( loadMetadata
  , saveMetadata
  , loadMimaState
  , saveMimaState
  ) where

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Binary              as B
import qualified Data.Text                as T

import           Mima.Run
import           Mima.Vm.Metadata
import           Mima.Vm.State

-- | Loads 'Metadata' from a given file path.
loadMetadata :: FilePath -> Run Metadata
loadMetadata path = do
  file <- readFileBS path
  case A.eitherDecode file of
    Left msg       -> throw $ T.pack msg
    Right metadata -> pure metadata

-- | Stores prettified 'Metadata' in a given file.
saveMetadata :: FilePath -> Metadata -> Run ()
saveMetadata path = writeFileBS path . A.encodePretty

loadMimaState :: FilePath -> Run MimaState
loadMimaState path = do
  bs <- readFileBS path
  case B.decodeOrFail bs of
    Right ("", 0, a) -> pure a
    Right _          -> throw "invalid file format"
    Left (_, _, e)   -> throw $ T.pack e

saveMimaState :: FilePath -> MimaState -> Run ()
saveMimaState path = writeFileBS path . B.encode
