module Mima.MimaRun.Options
  ( Options(..)
  , parserInfo
  ) where

import Options.Applicative

data Options = Options
  { inputFile :: FilePath
  , steps :: Maybe Integer
  } deriving (Show)

parser :: Parser Options
parser = Options
  <$> strOption
      (  help "The .mima file to use"
      <> metavar "INPUTFILE"
      )
  <*> (optional . option auto)
      (  short 'n'
      <> long "steps"
      <> help "Maximum number of steps to execute"
      <> metavar "STEPS"
      )

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper) (failureCode 1)
