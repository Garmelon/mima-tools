module Mima.Options
  ( flagFooter
  , switchWithNo
  , hiddenSwitchWithNo
  , formatConfigParser
  ) where

import Options.Applicative

import Mima.Format.State

flagFooter :: String
flagFooter =  "To disable an option, prepend 'no-' to its name (e. g. to disable"
           ++ " '--discover', use '--no-discover'). This only applies to options"
           ++ " with a default of 'enabled' or 'disabled'."

enabledOrDisabled :: Bool -> String
enabledOrDisabled False = "disabled"
enabledOrDisabled True  = "enabled"

switchWithNo :: String -> Bool -> String -> Parser Bool
switchWithNo name defaultValue helpText =
  flag' False noMod <|> flag defaultValue True yesMod
  where
    noMod  = long ("no-" ++ name) <> hidden
    yesMod = long name <> help (helpText ++ " (default: " ++ enabledOrDisabled defaultValue ++ ")")

hiddenSwitchWithNo :: String -> Bool -> String -> Parser Bool
hiddenSwitchWithNo name defaultValue helpText =
  flag' False noMod <|> flag defaultValue True yesMod
  where
    noMod  = long ("no-" ++ name) <> hidden
    yesMod = long name <> hidden <> help (helpText ++ " (default: " ++ enabledOrDisabled defaultValue ++ ")")

formatConfigParser :: Parser FormatConfig
formatConfigParser = FormatConfig
  <$> hiddenSwitchWithNo "sparse" True
      "Omit uninteresting addresses"
  <*> hiddenSwitchWithNo "register-flags" True
      "For each address, show all the memory flags that are active for that address"
  <*> hiddenSwitchWithNo "memory-flags" True
      "For each address, show all registers currently pointing to that address"
  <*> hiddenSwitchWithNo "address-dec" True
      "Display addresses in decimal"
  <*> hiddenSwitchWithNo "address-hex" True
      "Display addresses in hexadecimal"
  <*> hiddenSwitchWithNo "address-bin" False
      "Display addresses in binary"
  <*> hiddenSwitchWithNo "word-dec" True
      "Display words in decimal"
  <*> hiddenSwitchWithNo "word-hex" True
      "Display words in hexadecimal"
  <*> hiddenSwitchWithNo "word-bin" False
      "Display words in binary"
  <*> hiddenSwitchWithNo "instructions" True
      "Show instructions"
  <*> hiddenSwitchWithNo "labels" True
      "Show labels from the symbol file"
