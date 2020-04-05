{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mima.Run
  (
  -- * The 'Run' monad
    Run
  , run
  , runOrExit
  , throw
  , catch
  , handle
  -- * File operations
  , readFileT
  , writeFileT
  , readFileBS
  , writeFileBS
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.Exit
import           System.IO.Error

newtype Run a = Run (ExceptT T.Text IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: Run a -> IO (Either T.Text a)
run (Run e) = runExceptT e

runOrExit :: Int -> Run () -> IO ()
runOrExit exitCode r = do
  result <- run r
  case result of
    Right () -> pure ()
    Left e -> do
      putStrLn "Encountered an exception:"
      T.putStrLn e
      exitWith $ ExitFailure exitCode

throw :: T.Text -> Run a
throw = Run . throwE

catch :: Run a -> (T.Text -> Run a) -> Run a
catch (Run a) f = Run $ catchE a (\x -> let (Run result) = f x in result)

handle :: (T.Text -> Run a) -> Run a -> Run a
handle = flip catch

protect :: IO a -> Run a
protect m = do
  result <- liftIO $ tryIOError m
  case result of
    Right a -> pure a
    Left e  -> throw $ T.pack $ show e

readFileT :: FilePath -> Run T.Text
readFileT = protect . T.readFile

writeFileT :: FilePath -> T.Text -> Run ()
writeFileT path = protect . T.writeFile path

readFileBS :: FilePath -> Run BS.ByteString
readFileBS = protect . BS.readFile

writeFileBS :: FilePath -> BS.ByteString -> Run ()
writeFileBS path = protect . BS.writeFile path
