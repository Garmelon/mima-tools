module MimaRun where

import Options.Applicative

data Settings = Settings
  { file         :: String
  , steps        :: Maybe Integer
  , memoryDump   :: Maybe FilePath
  , quiet        :: Bool
  , sparseOutput :: Bool
  } deriving (Show)

settings :: Parser Settings
settings = Settings
  <$> strArgument
      (metavar "INFILE"
       <> help "The memory dump to load and execute")
  <*> (optional . option auto)
      (long "steps"
       <> short 'n'
       <> metavar "N"
       <> help "How many instructions to execute (if not specified, runs until HALT or execution exception)")
  <*> (optional . strOption)
      (long "dump"
       <> short 'd'
       <> metavar "OUTFILE"
       <> help "If specified, the MiMa's memory is dumped to this file after execution is finished")
  <*> flag False True
      (long "quiet"
       <> short 'q'
       <> help "Whether to print the memory after execution is finished")
  <*> flag False True
      (long "sparse"
       <> short 's'
       <> help "Whether to print memory locations that contain 0")

opts :: ParserInfo Settings
opts = info (helper <*> settings) $ fullDesc <> failureCode 1

main :: IO ()
main = execParser opts >>= print
