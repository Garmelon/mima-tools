{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MimaRun where

import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative

import           Mima.Instruction
import           Mima.Load
import           Mima.State
import           Mima.Util
import           Mima.Word

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
       <> help "Don't run the MiMa. Use the initial state for all further actions")

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

runMima :: Settings -> MimaState -> IO MimaState
runMima settings s =
  case steps settings of
    Nothing -> do
      putStrLn "Running until HALT or execution exception..."
      let (s', e, x) = run s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      T.putStrLn $ toText e
      pure s'
    Just n  -> do
      let (s', me, x) = runN n s
      putStrLn $ "Ran for " ++ show x ++ " steps"
      case me of
        Nothing -> putStrLn "Encountered no exception"
        Just e  -> T.putStrLn $ toText e
      pure s'

dumpState :: Bool -> MimaState -> T.Text
dumpState sparse ms
  =  registerLegend
  <> dumpRegisters ms
  <> memoryLegend
  <> dumpMemory sparse (msMemory ms)
  <> footerLegend

showWord :: MimaWord -> T.Text
showWord w =
  case wordToInstruction w of
    Left _  -> wordToHexDec w
    Right i -> wordToHexDec w <> ": " <> toText i

dumpRegisters :: MimaState -> T.Text
dumpRegisters MimaState{..}
  =  "IAR: " <> showAddressRegister msIAR <> "   ->   " <> showWord (readAt msIAR msMemory) <> "\n"
  <> "ACC: " <> showWordRegister    msACC <> "\n"
  <> " RA: " <> showAddressRegister msRA  <> "   ->   " <> showWord (readAt msRA  msMemory) <> "\n"
  <> " SP: " <> showAddressRegister msRA  <> "   ->   " <> showWord (readAt msRA  msMemory) <> "\n"
  <> " FP: " <> showAddressRegister msRA  <> "   ->   " <> showWord (readAt msRA  msMemory) <> "\n"
  where
    showWordRegister w = wordToHex w <> " (" <> wordToDec w <> ")"
    showAddressRegister lv =
      " " <> largeValueToHex lv <> " ( " <> largeValueToDec lv <> ")"

registerLegend :: T.Text
registerLegend = "--------- Register -------------- Target word ---------------\n"
--               "IAR:  00000 (       0)   ->   800008 ( 8388616): JMP        8"

showMemoryLine :: MimaAddress -> MimaWord -> T.Text
showMemoryLine addr word = largeValueToHexDec addr <> "   ->   " <> showWord word <> "\n"

dumpMemory :: Bool -> MimaMemory -> T.Text
dumpMemory sparse mem =
  let addresses = if sparse then sparseAddressRange mem else addressRange mem
      memLines = map (\addr -> showMemoryLine addr $ readAt addr mem) addresses
  in  T.concat memLines

memoryLegend :: T.Text
memoryLegend = "--- Address ---------------- Word ---------------------------\n"
--             "00000 (      0)   ->   800008 ( 8388616): JMP        8"
--             "IAR:  00000 (       0)   ->   800008 ( 8388616): JMP        8"

footerLegend :: T.Text
footerLegend = "------------------------------------------------------\n"
--             "00000 (      0)   ->   800008 ( 8388616): JMP        8"

-- TODO exception handling
main :: IO ()
main = do
  settings <- execParser opts

  putStrLn $ "Loading memdump at " ++ infile settings
  mem <- loadMemoryFromFile (infile settings)

  let s = initialState mem
  s' <- if norun settings then pure s else runMima settings s

  unless (quiet settings) $ do
    putStrLn ""
    putStrLn "Dump of MiMa state:"
    T.putStr $ dumpState (sparse settings) s'
    putStrLn ""

  forM_ (memoryDump settings) $ \path -> do
    putStrLn $ "Saving memdump at " ++ path
    saveMemoryToFile path $ msMemory s'
