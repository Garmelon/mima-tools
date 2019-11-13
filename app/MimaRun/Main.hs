module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.Text.IO as T
import           Options.Applicative

import           Mima.Flag
import           Mima.IO
import           Mima.Load
import           Mima.State
import           Mima.Util

import           PrintState

data Settings = Settings
  -- General guff
  { infile       :: FilePath
  , outfile      :: Maybe FilePath
  , autodiscover :: Bool
  , flagfile     :: Maybe FilePath
  , symbolfile   :: Maybe FilePath
  -- Run-specific guff
  , steps        :: Maybe Integer
  , norun        :: Bool
  -- Output format guff
  , quiet        :: Bool
  , sparse       :: Bool
  } deriving (Show)

{- Command-line parameters -}

settingsParser :: Parser Settings
settingsParser = Settings
  <$> strArgument
      (metavar "INFILE"
       <> help "The memory dump to load and execute")
  <*> (optional . strOption)
      (long "out"
       <> short 'o'
       <> metavar "OUTFILE"
       <> help "If specified, write the memory dump to this file after execution is finished")
  <*> flag True False
      (long "autodiscover"
       <> short 'a'
       <> help "Automatically try to find the .mima-flags and .mima-symbols files corresponding to the input files")
  <*> (optional . strOption)
      (long "flags"
       <> short 'f'
       <> metavar "FLAGFILE"
       <> help "A file containing extension memory flags, specified in the .mima-flags format")
  <*> (optional . strOption)
      (long "symbols"
       <> short 's'
       <> metavar "SYMBOLFILE"
       <> help "A file containing label names and addresses, specified in the .mima-symbols format")
  <*> (optional . option auto)
      (long "steps"
       <> short 'n'
       <> metavar "N"
       <> help "How many instructions to execute (if not specified, runs until HALT or execution exception)")
  <*> flag False True
      (long "norun"
       <> short 'r'
       <> help "Don't run the MiMa. Use the initial state for all further actions. Roughly equivalent to -n 0")
  <*> flag False True
      (long "quiet"
       <> short 'q'
       <> help "Don't print the memory dump")
  <*> flag False True
      (long "sparse"
       <> short 's'
       <> help "Don't print memory locations containing only 0x000000 in the memory dump")

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

{- Main logic -}

runMima :: Settings -> MimaState -> IO MimaState
runMima settings s =
  case steps settings of
    Nothing -> do
      putStrLn "Running until HALT or execution exception..."
      let (s', e, x) = run noFlags s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      T.putStrLn $ toText e
      pure s'
    Just n  -> do
      let (s', me, x) = runN noFlags n s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      case me of
        Nothing -> putStrLn "Encountered no exception"
        Just e  -> T.putStrLn $ toText e
      pure s'

-- TODO exception handling
main :: IO ()
main = doRun $ do
  settings <- lift $ execParser opts

  lift $ putStrLn $ "Loading memdump at " ++ infile settings
  ms <- loadStateFromFile (infile settings)
  ms' <- if norun settings then pure ms else lift (runMima settings ms)

  unless (quiet settings) $ do
    lift $ putStrLn ""
    lift $ putStrLn "Dump of MiMa state:"
    lift $ printStateLn (sparse settings) ms'
    lift $ putStrLn ""

  forM_ (outfile settings) $ \path -> do
    lift $ putStrLn $ "Saving memdump at " ++ path
    saveStateToFile path ms'
