module Main where

import Control.Monad.Trans.Class
import Options.Applicative

import Mima.IO
import Mima.Load
import Mima.Parse.Assembly

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
main = doRun_ $ do
  settings <- lift $ execParser opts

  lift $ putStrLn $ "Loading assembly file at " ++ infile settings
  (state, _, _) <- loadFile' readAssembly (infile settings)
  lift $ putStrLn "Parsing successful"

  lift $ putStrLn $ "Writing result to " ++ outfile settings
  saveStateToFile (outfile settings) state
