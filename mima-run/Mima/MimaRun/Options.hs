module Mima.MimaRun.Options
  ( Options(..)
  , parserInfo
  ) where

import           Data.Maybe
import           Options.Applicative
import           System.FilePath

data Options = Options
  { inputFile    :: FilePath
  , metadataFile :: FilePath
  , steps        :: Maybe Integer
  } deriving (Show)

createOptions :: FilePath -> Maybe FilePath -> Maybe Integer -> Options
createOptions inFile metaFile = Options inFile resolvedMetaFile
  where
    resolvedMetaFile = fromMaybe (replaceExtension inFile "mima-meta") metaFile

parser :: Parser Options
parser = createOptions
  <$> strArgument
      (  help "The .mima file to use"
      <> metavar "INPUTFILE"
      )
  <*> (optional . strOption)
      (  short 'm'
      <> long "metadata"
      <> help "The metadata file to use"
      <> metavar "METAFILE"
      )
  <*> (optional . option auto)
      (  short 'n'
      <> long "steps"
      <> help "Maximum number of steps to execute"
      <> metavar "STEPS"
      )

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper) (failureCode 1)
