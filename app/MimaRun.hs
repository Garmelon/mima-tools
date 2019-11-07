{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MimaRun where

import           Control.Monad
import           Data.Bits
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative
import           System.Console.ANSI

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
       <> help "Don't run the MiMa. Use the initial state for all further actions")

opts :: ParserInfo Settings
opts = info (helper <*> settingsParser) $ fullDesc <> failureCode 1

{- Fancy output -}

printAddress :: Int -> MimaAddress -> IO ()
printAddress n addr = do
  T.putStr $ toHexBytes addr
  putStr " ("
  T.putStr $ T.justifyRight n ' ' $ toDec addr
  putStr ")"

printWord :: Int -> MimaWord -> IO ()
printWord n word = do
  T.putStr $ toHexBytes word
  putStr " ("
  T.putStr $ T.justifyRight n ' ' $ toDec word
  putStr ")"

printInstruction :: Instruction -> IO ()
printInstruction (SmallInstruction so lv) = do
  setSGR [SetConsoleIntensity BoldIntensity]
  if | so `elem` [JMP, JMN, CALL]                        -> setSGR [SetColor Foreground Dull Green]
     | so `elem` [LDC, LDV, STV, LDIV, STIV, LDVR, STVR] -> setSGR [SetColor Foreground Vivid Blue]
     | so `elem` [ADD, AND, OR, XOR, EQL]                -> setSGR [SetColor Foreground Vivid Cyan]
     | otherwise                                         -> pure ()
  T.putStr $ toText so
  putStr " "
  setSGR [SetColor Foreground Vivid Black]
  T.putStr $ toDec lv
  setSGR []
printInstruction (LargeInstruction lo sv) = do
  setSGR [SetConsoleIntensity BoldIntensity]
  if | lo == HALT                                     -> setSGR [SetColor Foreground Vivid Red]
     | lo == RET                                      -> setSGR [SetColor Foreground Dull Green]
     | lo `elem` [NOT, RAR, ADC]                      -> setSGR [SetColor Foreground Vivid Cyan]
     | lo `elem` [LDRA, STRA, LDSP, STSP, LDFP, STFP] -> setSGR [SetColor Foreground Dull Yellow]
     | otherwise                                      -> pure ()
  T.putStr $ toText lo
  when (lo == ADC || sv /= zeroBits) $ do
    putStr " "
    setSGR [SetColor Foreground Vivid Black]
    T.putStr $ toDec sv
  setSGR []

printWordWithInstruction :: Int -> MimaWord -> IO ()
printWordWithInstruction n word = do
  printWord n word
  case wordToInstruction word of
    Left _  -> pure ()
    Right i -> do
      putStr ": "
      printInstruction i

printAddressRegister :: MimaState -> MimaAddress -> IO ()
printAddressRegister ms addr = do
  printAddress 8 addr
  putStr "   ->   "
  printWordWithInstruction 8 $ readAt addr $ msMemory ms

printRegistersLn :: MimaState -> IO ()
printRegistersLn ms = do
  putStr "IAR:  "
  printAddressRegister ms $ msIAR ms
  putStrLn ""

  putStr "ACC: "
  printWord 8 $ msACC ms
  putStrLn ""

  putStr " RA:  "
  printAddressRegister ms $ msRA ms
  putStrLn ""

  putStr " SP:  "
  printAddressRegister ms $ msSP ms
  putStrLn ""

  putStr " FP:  "
  printAddressRegister ms $ msFP ms
  putStrLn ""

printMemoryLocationLn :: MimaAddress -> MimaWord -> IO ()
printMemoryLocationLn addr word = do
  printAddress 7 addr
  putStr "   ->   "
  printWord 8 word
  case wordToInstruction word of
    Left _  -> pure ()
    Right i -> do
      putStr ": "
      printInstruction i
  putStrLn ""

printMemoryLn :: Bool -> MimaMemory -> IO ()
printMemoryLn sparse mem = do
  let addresses = if sparse then sparseAddressRange mem else addressRange mem
  forM_ addresses $ \addr -> do
    printMemoryLocationLn addr (readAt addr mem)

printStateLn :: Bool -> MimaState -> IO ()
printStateLn sparse ms = do
  printRegistersLn ms
  printMemoryLn sparse $ msMemory ms

{- Main logic -}

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
