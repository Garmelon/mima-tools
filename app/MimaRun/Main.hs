module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.Text.IO as T
import           Options.Applicative
import           System.FilePath

import           Mima.Flag
import           Mima.IO
import           Mima.Load
import           Mima.Parse.FlagFile
import           Mima.State
import           Mima.Util
import           Mima.Word

import           PrintState

data Settings = Settings
  -- General
  { infile     :: FilePath
  , outfile    :: Maybe FilePath
  , discover   :: Bool
  , flagFile   :: Maybe FilePath
  , symbolFile :: Maybe FilePath
  -- Running
  , steps      :: Maybe Integer
  , norun      :: Bool
  -- Output
  , quiet      :: Bool
  , sparse     :: Bool
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
      (long "nodiscover"
       <> help "Disable the automatic loading of the .mima-flags and .mima-symbols files")
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
       <> metavar "N"
       <> help "How many instructions to execute (if not specified, runs until HALT or execution exception)")
  <*> flag False True
      (long "norun"
       <> help "Don't run the MiMa. Use the initial state for all further actions. Roughly equivalent to -n 0")
  <*> flag False True
      (long "quiet"
       <> short 'q'
       <> help "Don't print the memory dump")
  <*> flag False True
      (long "sparse"
       <> help "Don't print memory locations containing only 0x000000 in the memory dump")

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

{- Main logic -}

runMima :: Settings -> MimaState -> Flags (MimaAddress -> Bool) -> IO MimaState
runMima settings s f =
  case steps settings of
    Nothing -> do
      putStrLn "Running until HALT or execution exception..."
      let (s', e, x) = run f s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      T.putStrLn $ toText e
      pure s'
    Just n  -> do
      let (s', me, x) = runN f n s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      case me of
        Nothing -> putStrLn "Encountered no exception"
        Just e  -> T.putStrLn $ toText e
      pure s'

loadFlagFile :: FilePath -> Run (Flags (MimaAddress -> Bool))
loadFlagFile filename = flagChecks <$> parseFile parseFlagFile filename

loadFlags :: Settings -> Run (Flags (MimaAddress -> Bool))
loadFlags settings = do
  case flagFile settings of
    Just filename -> do
      lift $ putStrLn $ "Loading flags from specified file: " ++ filename
      loadFlagFile filename
    Nothing -> do
      maybeFlags <- if discover settings then tryLoadDiscovered else pure Nothing
      case maybeFlags of
        Just flags -> pure flags
        Nothing -> do
          lift $ putStrLn "Not using flags"
          pure noFlags
  where
    discovered = dropExtension (infile settings) ++ ".mima-flags"
    tryLoadDiscovered = do
      lift $ putStrLn $ "Loading flags from file: " ++ discovered
      tryRun (loadFlagFile discovered)

-- TODO exception handling
main :: IO ()
main = doRun_ $ do
  settings <- lift $ execParser opts

  lift $ putStrLn $ "Loading memdump at " ++ infile settings
  ms <- loadStateFromFile (infile settings)

  flags <- loadFlags settings

  ms' <- if norun settings then pure ms else lift (runMima settings ms flags)

  unless (quiet settings) $ do
    lift $ putStrLn ""
    lift $ putStrLn "Dump of MiMa state:"
    lift $ printStateLn (sparse settings) ms'
    lift $ putStrLn ""

  forM_ (outfile settings) $ \path -> do
    lift $ putStrLn $ "Saving memdump at " ++ path
    saveStateToFile path ms'
