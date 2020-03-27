{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mima.Run
  (
  -- * The 'Run' monad
    Run
  , run
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
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.IO.Error

newtype Run a = Run (ExceptT T.Text IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: Run a -> IO (Either T.Text a)
run (Run e) = runExceptT e

throw :: T.Text -> Run a
throw = Run . throwE

catch :: Run a -> (T.Text -> Run a) -> Run a
catch (Run a) f = Run $ catchE a (\x -> let (Run result) = f x in result)

handle :: (T.Text -> Run a) -> Run a -> Run a
handle = flip catch

isRelevantError :: IOError -> Bool
isRelevantError e = isAlreadyInUseError e || isDoesNotExistError e || isPermissionError e

formatRelevantError :: IOError -> Maybe T.Text
formatRelevantError e
  | isRelevantError e = Just $ T.pack $
    "Can't open file " <> ioeGetLocation e <> ": " <> ioeGetErrorString e
  | otherwise = Nothing

protect :: IO a -> Run a
protect m = do
  result <- liftIO $ tryIOError m
  case result of
    Right a -> pure a
    Left e -> case formatRelevantError e of
      Nothing  -> liftIO $ ioError e
      Just msg -> throw msg

readFileT :: FilePath -> Run T.Text
readFileT = protect . T.readFile

writeFileT :: FilePath -> T.Text -> Run ()
writeFileT path = protect . T.writeFile path

readFileBS :: FilePath -> Run BS.ByteString
readFileBS = protect . BS.readFile

writeFileBS :: FilePath -> BS.ByteString -> Run ()
writeFileBS path = protect . BS.writeFile path
