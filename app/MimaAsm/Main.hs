{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative
import           System.FilePath

import           Mima.Flag
import           Mima.Format.FlagFile
import           Mima.Format.SymbolFile
import           Mima.IO
import           Mima.Label
import           Mima.Load
import           Mima.Options
import           Mima.Parse.Assembly

data Settings = Settings
  { infile     :: String
  , outfile    :: String
  , discover   :: Bool
  , flagFile   :: Maybe FilePath
  , symbolFile :: Maybe FilePath
  } deriving (Show)

getFlagFile :: Settings -> File
getFlagFile settings =
  case flagFile settings of
    Just path -> RequiredFile path
    Nothing   -> case discover settings of
      False -> NoFile
      True  -> OptionalFile discoveredPath
  where
    discoveredPath = dropExtension (infile settings) ++ ".mima-flags"

getSymbolFile :: Settings -> File
getSymbolFile settings =
  case symbolFile settings of
    Just path -> RequiredFile path
    Nothing   -> case discover settings of
      False -> NoFile
      True  -> OptionalFile discoveredPath
  where
    discoveredPath = dropExtension (infile settings) ++ ".mima-symbols"

{- Command-line parameters -}

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
  <*> switchWithNo "discover" True
      "Derive the file names for the .mima-flags and .mima-symbols files from the name of the input file"
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

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

{- Saving supplemental files -}

printFile :: T.Text -> File -> Run ()
printFile name NoFile =
  lift $ T.putStrLn $ "Not saving " <> name <> ": No file specified and discovery turned off"
printFile name (OptionalFile path) =
  lift $ T.putStrLn $ "Saving " <> name <> " to " <> T.pack path
printFile name (RequiredFile path) =
  lift $ T.putStrLn $ "Saving " <> name <> " to " <> T.pack path

saveFlags :: RawFlags -> Settings -> Run ()
saveFlags flags settings = do
  let file = getFlagFile settings
  printFile "flags" file
  storeFile' file (formatFlagFile flags)

saveSymbols :: LabelSpec -> Settings -> Run ()
saveSymbols labels settings = do
  let file = getSymbolFile settings
  printFile "symbols" file
  storeFile' file (formatSymbolFile labels)

main :: IO ()
main = doRun_ $ do
  settings <- lift $ execParser opts

  lift $ putStrLn $ "Loading assembly file at " ++ infile settings
  (state, labels, flags) <- loadFile readAssembly (infile settings)
  lift $ putStrLn "Parsing successful"

  lift $ putStrLn $ "Writing result to " ++ outfile settings
  saveStateToFile (outfile settings) state

  saveFlags flags settings
  saveSymbols labels settings
