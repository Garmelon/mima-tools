module Main where

import           Control.Monad
import qualified Data.Text.IO as T
import           Options.Applicative

import           Mima.Flag
import           Mima.Load
import           Mima.State
import           Mima.Util

import           PrintState

data Settings = Settings
  { infile     :: String
  , steps      :: Maybe Integer
  , memoryDump :: Maybe FilePath
  , quiet      :: Bool
  , sparse     :: Bool
  , norun      :: Bool
  } deriving (Show)

{- Command-line parameters -}

settingsParser :: Parser Settings
settingsParser = Settings
  <$> strArgument
      (metavar "INFILE"
       <> help "The binary memory dump to load and execute")
  <*> (optional . option auto)
      (long "steps"
       <> short 'n'
       <> metavar "N"
       <> help "How many instructions to execute (if not specified, runs until HALT or execution exception)")
  <*> (optional . strOption)
      (long "dump"
       <> short 'd'
       <> metavar "OUTFILE"
       <> help "If specified, write the MiMa's binary memory dump to this file after execution is finished")
  <*> flag False True
      (long "quiet"
       <> short 'q'
       <> help "Don't print the memory dump")
  <*> flag False True
      (long "sparse"
       <> short 's'
       <> help "Don't print memory locations containing only 0x000000 in the memory dump")
  <*> flag False True
      (long "norun"
       <> short 'r'
       <> help "Don't run the MiMa. Use the initial state for all further actions. Roughly equivalent to -n 0")

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

{- Main logic -}

runMima :: Settings -> MimaState -> IO MimaState
runMima settings s =
  case steps settings of
    Nothing -> do
      putStrLn "Running until HALT or execution exception..."
      let (s', e, x) = run impotentChecks s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      T.putStrLn $ toText e
      pure s'
    Just n  -> do
      let (s', me, x) = runN impotentChecks n s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      case me of
        Nothing -> putStrLn "Encountered no exception"
        Just e  -> T.putStrLn $ toText e
      pure s'

-- TODO exception handling
main :: IO ()
main = do
  settings <- execParser opts

  putStrLn $ "Loading memdump at " ++ infile settings
  ms <- loadStateFromFile (infile settings)
  case ms of
    Left errorMsg -> putStrLn errorMsg
    Right s       -> do
      s' <- if norun settings then pure s else runMima settings s

      unless (quiet settings) $ do
        putStrLn ""
        putStrLn "Dump of MiMa state:"
        printStateLn (sparse settings) s'
        putStrLn ""

      forM_ (memoryDump settings) $ \path -> do
        putStrLn $ "Saving memdump at " ++ path
        saveStateToFile path s'
