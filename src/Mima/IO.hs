module Mima.IO
  ( Run
  , doRun
  , failWith
  , parseFile
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Text.IO as T
import           Text.Megaparsec

import           Mima.Parser.Common

type Run a = ExceptT String IO a

doRun :: Run () -> IO ()
doRun r = do
  result <- runExceptT r
  case result of
    Left errorMsg -> putStrLn errorMsg
    Right ()      -> pure ()

failWith :: String -> Run a
failWith = except . Left

parseFile :: Parser a -> FilePath -> Run a
parseFile parser filepath = do
  content <- lift $ T.readFile filepath
  case parse parser filepath content of
    Right a           -> pure a
    Left errorBundle  -> failWith $ errorBundlePretty errorBundle
