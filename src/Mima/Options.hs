module Mima.Options
  ( switchWithNo
  , formatConfigHelp
  , formatConfigParser
  ) where

import Options.Applicative

import Mima.Format.State

switchWithNo :: String -> Bool -> Mod FlagFields Bool -> Parser Bool
switchWithNo name defaultValue fields
  | defaultValue = flag' False noMod <|> flag True True yesMod
  | otherwise    = flag' True yesMod <|> flag False False noMod
  where
    yesMod = long name <> hidden <> fields
    noMod  = long ("no-" ++ name) <> hidden

formatConfigHelp :: String
formatConfigHelp = "All options labeled with 'Formatting:' can be negated by prepending 'no-' to their name (e. g. '--sparse' becomes '--no-sparse')."

formatConfigParser :: Parser FormatConfig
formatConfigParser = FormatConfig
  <$> switchWithNo "sparse" False
      (help "Formatting: Omit uninteresting addresses")
  <*> switchWithNo "register-flags" True
      (help "Formatting: For each address, show all the memory flags that are active for that address")
  <*> switchWithNo "memory-flags" True
      (help "Formatting: For each address, show all registers currently pointing to that address")
  <*> switchWithNo "address-dec" True
      (help "Formatting: Display addresses in decimal")
  <*> switchWithNo "address-hex" True
      (help "Formatting: Display addresses in hexadecimal")
  <*> switchWithNo "address-bin" False
      (help "Formatting: Display addresses in binary")
  <*> switchWithNo "word-dec" True
      (help "Formatting: Display words in decimal")
  <*> switchWithNo "word-hex" True
      (help "Formatting: Display words in hexadecimal")
  <*> switchWithNo "word-bin" False
      (help "Formatting: Display words in binary")
  <*> switchWithNo "instructions" True
      (help "Formatting: Show instructions")
  <*> switchWithNo "labels" True
      (help "Formatting: Show labels from the symbol file")
