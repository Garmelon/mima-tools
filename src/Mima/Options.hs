module Mima.Options
  ( switchWithNo
  , formatConfigParser
  ) where

import Options.Applicative

import Mima.Format.State

switchWithNo :: String -> Bool -> Mod FlagFields Bool -> Parser Bool
switchWithNo name True  fields = flag' False (long ("no-" ++ name) <> fields)
                                 <|> flag True True (long name <> fields)
switchWithNo name False fields = flag' True (long name <> fields)
                                 <|> flag False False (long ("no-" ++ name) <> fields)

formatConfigParser :: Parser FormatConfig
formatConfigParser = FormatConfig
  <$> switchWithNo "sparse" False mempty
  <*> switchWithNo "register-flags" True mempty
  <*> switchWithNo "memory-flags" True mempty
  <*> switchWithNo "address-dec" True mempty
  <*> switchWithNo "address-hex" True mempty
  <*> switchWithNo "address-bin" False mempty
  <*> switchWithNo "word-dec" True mempty
  <*> switchWithNo "word-hex" True mempty
  <*> switchWithNo "word-bin" False mempty
  <*> switchWithNo "instructions" True mempty
  <*> switchWithNo "labels" True mempty
