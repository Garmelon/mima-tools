module MimaRun where

import           Control.Monad
import qualified Data.Text.IO as T
import           Options.Applicative

import           Mima.Load
import           Mima.State
import           Mima.Util

data Settings = Settings
  { infile     :: String
  , steps      :: Maybe Integer
  , memoryDump :: Maybe FilePath
  , quiet      :: Bool
  , sparse     :: Bool
  , norun      :: Bool
  } deriving (Show)

settingsParser :: Parser Settings
settingsParser = Settings
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
  <*> flag False True
      (long "norun"
       <> short 'r'
       <> help "Don't run the MiMa. Continues as if the initial state was the result of running the MiMa.")

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

runMima :: Settings -> MimaState -> IO MimaState
runMima settings s =
  case steps settings of
    Nothing -> do
      putStrLn "Running until HALT or execution exception..."
      let (s', e) = run s
      T.putStrLn $ toText e
      pure s'
    Just _  -> do
      putStrLn "This option is currently not supported"
      undefined

-- TODO exception handling
main :: IO ()
main = do
  settings <- execParser opts

  putStrLn $ "Loading memdump at " ++ infile settings
  mem <- loadMemoryFromFile (infile settings)

  let s = initialState mem
  s' <- if norun settings then pure s else runMima settings s

  unless (quiet settings) $ do
    putStrLn "Dump of memory:"
    T.putStrLn $ memoryToText (sparse settings) (msMemory s')

  forM_ (memoryDump settings) $ \path -> do
    putStrLn $ "Saving memdump at " ++ path
    saveMemoryToFile path $ msMemory s'
