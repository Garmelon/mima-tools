module Main where

import           Control.Monad.IO.Class
import qualified Data.Text.IO           as T
import           Options.Applicative

import           Mima.Format
import           Mima.MimaRun.Options
import           Mima.Run
import           Mima.Vm.State
import           Mima.Vm.Storage

main :: IO ()
main = runOrExit 2 $ do
  opts <- liftIO $ execParser parserInfo
  initialState <- loadMimaState $ inputFile opts
  finalState <- liftIO $ case steps opts of
    Nothing -> do
      let (finalState, abortReason, stepsMade) = execute initialState
      putStrLn $ "Stopped after " ++ show stepsMade ++ " steps, reason:"
      T.putStrLn $ toText abortReason
      pure finalState
    Just n -> do
      let (finalState, mAbortReason, stepsMade) = executeN n initialState
      putStrLn $ "Stopped after " ++ show stepsMade ++ " steps, reason:"
      case mAbortReason of
        Nothing          -> putStrLn "Ran out of steps"
        Just abortReason -> T.putStrLn $ toText abortReason
      pure finalState
  liftIO $ putStrLn ""
  liftIO $ print finalState
