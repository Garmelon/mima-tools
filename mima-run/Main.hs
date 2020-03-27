module Main where

import Options.Applicative

import Mima.MimaRun.Options

main :: IO ()
main = do
  opts <- execParser parserInfo
  putStrLn $ "The options are: " ++ show opts
