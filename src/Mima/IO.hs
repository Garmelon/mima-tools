{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Mima.IO
  ( Run
  , doRun
  , doRun_
  , tryRun
  , File(..)
  , readTextFile
  , loadFile
  , loadFile'
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

data File
  = NoFile
  | OptionalFile FilePath
  | RequiredFile FilePath
  deriving (Show)

readTextFile :: FilePath -> Run T.Text
readTextFile filepath = do
  eitherContent <- lift $ catchIOError (Right <$> T.readFile filepath) handleError
  except eitherContent
  where
    isRelevantError e = isAlreadyInUseError e || isDoesNotExistError e || isPermissionError e
    handleError e = if isRelevantError e
      then pure $ Left $ "Can't load file " ++ filepath ++ ": " ++ ioeGetErrorString e
      else ioError e -- This error does not concern us

loadTextFile :: File -> Run (Maybe (FilePath, T.Text))
loadTextFile NoFile              = pure Nothing
loadTextFile (OptionalFile path) = do
  mContent <- tryRun $ readTextFile path
  pure $ (path,) <$> mContent
loadTextFile (RequiredFile path) = do
  content <- readTextFile path
  pure $ Just (path, content)

loadFile :: (FilePath -> T.Text -> Either WeedErrorBundle a) -> File -> Run (Maybe a)
loadFile f file = do
  mContent <- loadTextFile file
  case mContent of
    Nothing              -> pure Nothing
    Just (path, content) -> case f path content of
      Left errorBundle -> throwE $ errorBundlePretty errorBundle
      Right result     -> pure $ Just result

loadFile' :: (FilePath -> T.Text -> Either WeedErrorBundle a) -> FilePath -> Run a
loadFile' f path = do
  content <- readTextFile path
  case f path content of
    Left errorBundle -> throwE $ errorBundlePretty errorBundle
    Right result     -> pure result
