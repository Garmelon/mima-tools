module Mima.Parse.Assembly
  ( parseAssembly
  , weedAssembly
  , formatAssembly
  , readAssembly
  ) where

import           Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as T
import           Text.Megaparsec

import           Mima.Flag
import           Mima.Label
import           Mima.Parse.Assembly.Common
import           Mima.Parse.Assembly.Lexeme
import           Mima.Parse.Assembly.Statement
import           Mima.Parse.Assembly.Weed.Common
import           Mima.Parse.Assembly.Weed.Resolve
import           Mima.Parse.Assembly.Weed.Statement
import           Mima.Parse.Common
import           Mima.Parse.Weed
import           Mima.State
import           Mima.Word

parseAssembly :: Parser [WithOffset (Statement Address)]
parseAssembly = space *> many lNewline *> lStatements <* eof

weedAssembly :: [WithOffset (Statement Address)] -> Weed WeedError (WeedResult MimaAddress)
weedAssembly = weedStatements >=> resolveLabels

formatAssembly :: WeedResult MimaAddress -> (MimaState, LabelSpec, Map.Map Char AddressRange)
formatAssembly = undefined

readAssembly :: FilePath -> T.Text -> Either WeedErrorBundle (MimaState, LabelSpec, Map.Map Char AddressRange)
readAssembly filename input = do
  unweeded <- parse parseAssembly filename input
  weeded <- runWeedBundle filename input $ weedAssembly unweeded
  pure $ formatAssembly weeded
