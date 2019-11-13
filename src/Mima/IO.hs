module Mima.IO
  ( parseFile
  ) where

import qualified Data.Text.IO as T
import           Text.Megaparsec

import           Mima.Parser.Common

parseFile :: Parser a -> FilePath -> IO (Maybe a)
parseFile parser filepath = do
  content <- T.readFile filepath
  case parse parser filepath content of
    Right a           -> pure $ Just a
    Left errorBundle  -> do
      putStrLn $ errorBundlePretty errorBundle
      pure Nothing
