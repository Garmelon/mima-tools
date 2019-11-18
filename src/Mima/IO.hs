{-# LANGUAGE MultiWayIf #-}

module Mima.IO
  ( Run
  , doRun
  , doRun_
  , tryRun
  , readTextFile
  , loadFile
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO.Error
import           Text.Megaparsec

import           Mima.Parse.Weed

type Run a = ExceptT String IO a

doRun :: Run a -> IO (Either String a)
doRun = runExceptT

doRun_ :: Run () -> IO ()
doRun_ r = do
  result <- doRun r
  case result of
    Right () -> pure ()
    Left e   -> putStrLn e

tryRun :: Run a -> Run (Maybe a)
tryRun r = do
  result <- lift $ runExceptT r
  case result of
    Right a -> pure $ Just a
    Left e -> do
      lift $ putStrLn e
      pure Nothing

readTextFile :: FilePath -> Run T.Text
readTextFile filepath = do
  eitherContent <- lift $ catchIOError (Right <$> T.readFile filepath) handleError
  except eitherContent
  where
    isRelevantError e = isAlreadyInUseError e || isDoesNotExistError e || isPermissionError e
    handleError e = if isRelevantError e
      then pure $ Left $ "Can't load file " ++ filepath ++ ": " ++ ioeGetErrorString e
      else ioError e -- This error does not concern us

loadFile :: (FilePath -> T.Text -> Either WeedErrorBundle a) -> FilePath -> Run a
loadFile f filepath = do
  content <- readTextFile filepath
  case f filepath content of
    Left errorBundle  -> throwE $ errorBundlePretty errorBundle
    Right a           -> pure a
