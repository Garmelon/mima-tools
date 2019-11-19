module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Text.IO as T
import           Options.Applicative
import           System.FilePath

import           Mima.Flag
import           Mima.Format.State
import           Mima.IO
import           Mima.Label
import           Mima.Load
import           Mima.Options
import           Mima.Parse.FlagFile
import           Mima.State
import           Mima.Util
import           Mima.Word

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
  , formatConf :: FormatConfig
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
  <*> switchWithNo "discover" True
      "Try to load .mima-flags and .mima-symbols corresponding to the .mima input file"
  <*> (optional . strOption)
      (long "flag-file"
       <> short 'f'
       <> metavar "FLAGFILE"
       <> help "A file containing extension memory flags, specified in the .mima-flags format")
  <*> (optional . strOption)
      (long "symbol-file"
       <> short 's'
       <> metavar "SYMBOLFILE"
       <> help "A file containing label names and addresses, specified in the .mima-symbols format")
  <*> (optional . option auto)
      (long "steps"
       <> short 'n'
       <> metavar "N"
       <> help "How many instructions to execute (if not specified, runs until HALT or execution exception)")
  <*> flag False True
      (long "no-run"
       <> help "Don't run the MiMa. Use the initial state for all further actions. Roughly equivalent to --steps 0")
  <*> flag False True
      (long "quiet"
       <> short 'q'
       <> help "Don't print the memory dump")
  <*> formatConfigParser

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1 <> footer flagFooter

{- Loading the flag file -}

-- If explicit file name:
--   Try to load file
--   Fail if loading fails
-- Elif discover:
--   Try to load file
--   Use defaults if loading fails
-- Else:
--   Use defaults

loadFlagFile :: FilePath -> Run (Flags (MimaAddress -> Bool))
loadFlagFile filename = do
  lift $ putStrLn $ "Loading flags from " ++ filename
  (interpretFlagSpec . getFlagSpec) <$> loadFile readFlagFile filename

withDefaultFlags :: Run (Flags (MimaAddress -> Bool)) -> Run (Flags (MimaAddress -> Bool))
withDefaultFlags p = do
  result <- tryRun p
  case result of
    Just flags -> pure flags
    Nothing -> do
      lift $ putStrLn "Using default flags"
      pure noFlags

loadFlags :: Settings -> Run (Flags (MimaAddress -> Bool))
loadFlags settings =
  case flagFile settings of
    Just filename -> loadFlagFile filename
    Nothing -> withDefaultFlags $ if discover settings
      then loadFlagFile discovered
      else throwE "File not specified and discovery turned off"
  where
    discovered = dropExtension (infile settings) ++ ".mima-flags"

{- Other functions -}

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

printState :: MimaState -> Flags (MimaAddress -> Bool) -> LabelSpec -> Settings -> Run ()
printState ms flags labels settings = do
  let formatEnv = FormatEnv
        { feState  = ms
        , feFlags  = flags
        , feLabels = labelsByAddress labels
        , feConf   = formatConf settings
        }
  lift $ putStrLn ""
  lift $ putStrLn "Dump of MiMa state:"
  lift $ T.putStrLn $ formatState formatEnv
  lift $ putStrLn ""

-- TODO exception handling
main :: IO ()
main = doRun_ $ do
  settings <- lift $ execParser opts

  lift $ putStrLn $ "Loading memdump from " ++ infile settings
  ms <- loadStateFromFile (infile settings)

  flags  <- loadFlags settings
  labels <- pure noLabels -- loadSymbolFile settings

  ms' <- if norun settings
    then pure ms
    else lift $ runMima settings ms flags

  unless (quiet settings) $ printState ms' flags labels settings

  forM_ (outfile settings) $ \path -> do
    lift $ putStrLn $ "Saving memdump at " ++ path
    saveStateToFile path ms'
