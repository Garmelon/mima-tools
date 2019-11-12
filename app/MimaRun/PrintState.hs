{-# LANGUAGE MultiWayIf #-}

module PrintState
  ( printStateLn
  ) where

import           Control.Monad
import           Data.Bits
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Console.ANSI

import           Mima.Instruction
import           Mima.State
import           Mima.Util
import           Mima.Word

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

-- Color scheme:
-- Red:    HALT
-- Yellow: Instructions for accessing other registers
-- Green:  Instructions that can modify the IAR (jumps)
-- Blue:   Instructions that read from or write to memory
-- Cyan:   Logical operations and calculations that modify the ACC
printInstruction :: Instruction -> IO ()
printInstruction (SmallInstruction so lv) = do
  setSGR [SetConsoleIntensity BoldIntensity]
  if | so `elem` [JMP, JMN, CALL]              -> setSGR [SetColor Foreground Dull Green]
     | so `elem` [LDC, LDV, STV, LDIV, STIV]   -> setSGR [SetColor Foreground Vivid Blue]
     | so `elem` [ADD, AND, OR, XOR, EQL, ADC] -> setSGR [SetColor Foreground Vivid Cyan]
     | otherwise                               -> pure ()
  T.putStr $ toText so
  putStr " "
  setSGR [SetColor Foreground Vivid Black]
  T.putStr $ toDec lv
  setSGR []
printInstruction (LargeInstruction lo sv) = do
  setSGR [SetConsoleIntensity BoldIntensity]
  if | lo == HALT                                     -> setSGR [SetColor Foreground Vivid Red]
     | lo `elem` [LDRA, STRA, LDSP, STSP, LDFP, STFP] -> setSGR [SetColor Foreground Dull Yellow]
     | lo == RET                                      -> setSGR [SetColor Foreground Dull Green]
     | lo `elem` [LDRS, STRS, LDRF, STRF]             -> setSGR [SetColor Foreground Vivid Blue]
     | lo `elem` [NOT, RAR]                           -> setSGR [SetColor Foreground Vivid Cyan]
     | otherwise                                      -> pure ()
  T.putStr $ toText lo
  when (lo `elem` [LDRS, STRS, LDRF, STRF] || sv /= zeroBits) $ do
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
  let addresses = if sparse then sparseUsedAddresses mem else usedAddresses mem
  forM_ addresses $ \addr -> do
    printMemoryLocationLn addr (readAt addr mem)

printStateLn :: Bool -> MimaState -> IO ()
printStateLn sparse ms = do
  printRegistersLn ms
  printMemoryLn sparse $ msMemory ms
