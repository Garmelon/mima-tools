module Main where

import           Control.Monad.IO.Class
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Options.Applicative

import           Mima.Format
import           Mima.MimaRun.Options
import           Mima.Run
import           Mima.Vm.Flags
import           Mima.Vm.Metadata
import           Mima.Vm.State
import           Mima.Vm.Storage

loadMetadataOrEmpty :: FilePath -> Run Metadata
loadMetadataOrEmpty path = catch (loadMetadata path) $ \e -> do
  liftIO $ putStrLn $ "Metafile could not be loaded: " ++ T.unpack e
  pure mempty

main :: IO ()
main = runOrExit 2 $ do
  opts <- liftIO $ execParser parserInfo
  initialState <- loadMimaState $ inputFile opts
  metadata <- loadMetadataOrEmpty $ metadataFile opts
  let flags = flagsFromMetadata metadata

  finalState <- liftIO $ case steps opts of
    Nothing -> do
      let (finalState, abortReason, stepsMade) = execute flags initialState
      putStrLn $ "Stopped after " ++ show stepsMade ++ " steps, reason:"
      T.putStrLn $ toText abortReason
      pure finalState
    Just n -> do
      let (finalState, mAbortReason, stepsMade) = executeN n flags initialState
      putStrLn $ "Stopped after " ++ show stepsMade ++ " steps, reason:"
      case mAbortReason of
        Nothing          -> putStrLn "Ran out of steps"
        Just abortReason -> T.putStrLn $ toText abortReason
      pure finalState

  liftIO $ putStrLn ""
  liftIO $ print finalState
