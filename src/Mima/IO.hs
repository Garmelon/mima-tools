{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Mima.IO
  ( Run
  , doRun
  , doRun_
  , tryRun
  , readTextFile
  , writeTextFile
  , loadFile
  , storeFile
  , File(..)
  , loadFile'
  , storeFile'
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

handleOpenFileError :: FilePath -> IOError -> IO (Either String a)
handleOpenFileError filepath e = if isRelevantError
  then pure $ Left $ "Can't open file " <> filepath <> ": " <> ioeGetErrorString e
  else ioError e
  where
    isRelevantError = isAlreadyInUseError e || isDoesNotExistError e || isPermissionError e

readTextFile :: FilePath -> Run T.Text
readTextFile filepath = do
  eitherContent <- lift $ catchIOError (Right <$> T.readFile filepath) (handleOpenFileError filepath)
  except eitherContent

writeTextFile :: FilePath -> T.Text -> Run ()
writeTextFile filepath content = do
  result <- lift $ catchIOError (Right <$> T.writeFile filepath content) (handleOpenFileError filepath)
  except result

loadFile :: (FilePath -> T.Text -> Either WeedErrorBundle a) -> FilePath -> Run a
loadFile f path = do
  content <- readTextFile path
  case f path content of
    Left errorBundle -> throwE $ errorBundlePretty errorBundle
    Right result     -> pure result

-- To have a consistent naming scheme
storeFile :: FilePath -> T.Text -> Run ()
storeFile = writeTextFile

data File
  = NoFile
  | OptionalFile FilePath
  | RequiredFile FilePath
  deriving (Show)

loadTextFile :: File -> Run (Maybe (FilePath, T.Text))
loadTextFile NoFile              = pure Nothing
loadTextFile (OptionalFile path) = do
  mContent <- tryRun $ readTextFile path
  pure $ (path,) <$> mContent
loadTextFile (RequiredFile path) = do
  content <- readTextFile path
  pure $ Just (path, content)

loadFile' :: (FilePath -> T.Text -> Either WeedErrorBundle a) -> File -> Run (Maybe a)
loadFile' f file = do
  mContent <- loadTextFile file
  case mContent of
    Nothing              -> pure Nothing
    Just (path, content) -> case f path content of
      Left errorBundle -> throwE $ errorBundlePretty errorBundle
      Right result     -> pure $ Just result

storeFile' :: File -> T.Text -> Run ()
storeFile' NoFile              _       = pure ()
storeFile' (OptionalFile path) content = () <$ tryRun (writeTextFile path content)
storeFile' (RequiredFile path) content = writeTextFile path content
