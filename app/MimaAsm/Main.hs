module Main where

import           Options.Applicative

import           Mima.Assembler.Parser
import           Mima.IO
import           Mima.Load

data Settings = Settings
  { infile     :: String
  , outfile    :: String
  } deriving (Show)

settingsParser :: Parser Settings
settingsParser = Settings
  <$> strArgument
      (metavar "INFILE"
       <> help "The .mimasm file to assemble")
  <*> strOption
      (long "out"
       <> short 'o'
       <> metavar "OUTFILE"
       <> help "The .mima file to write the assembled result to"
       <> value "out.mima"
       <> showDefault)

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

main :: IO ()
main = do
  settings <- execParser opts

  putStrLn $ "Loading assembly file at " ++ infile settings
  asm <- parseFile parseState (infile settings)
  case asm of
    Nothing -> pure ()
    Just (state, _) -> do
      putStrLn "Parsing successful"
      putStrLn $ "Writing result to " ++ outfile settings
      saveStateToFile (outfile settings) state
