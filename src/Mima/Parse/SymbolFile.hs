{-# LANGUAGE OverloadedStrings #-}

module Mima.Parse.SymbolFile
  ( parseSymbolFile
  , weedSymbolFile
  , readSymbolFile
  ) where

import           Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as T
import           Text.Megaparsec

import           Mima.Label
import           Mima.Parse.Common
import           Mima.Parse.Lexeme
import           Mima.Parse.Weed
import           Mima.Word

{- Parsing -}

lWhitespace :: Parser Char
lWhitespace = lexeme whitespace

lAddress :: Parser MimaAddress
lAddress = lexeme fixedWidthHexAddress

lLabels :: Parser [WithOffset LabelName]
lLabels = lexeme $ sepBy1 (withOffset labelName) lWhitespace

lLine :: Parser (MimaAddress, [WithOffset LabelName])
lLine = do
  addr <- lAddress
  void $ symbol ":"
  labels <- lLabels
  lNewlines
  pure (addr, labels)

-- Does not keep the last list to appear for a certain key, but concatenates
-- them all.
combineLines :: [(MimaAddress, [WithOffset LabelName])]
             -> Map.Map MimaAddress [WithOffset LabelName]
combineLines = ($ Map.empty) . mconcat . reverse . map (uncurry $ Map.insertWith (++))

parseSymbolFile :: Parser (Map.Map MimaAddress [WithOffset LabelName])
parseSymbolFile = space *> many lNewline *> (combineLines <$> many lLine) <* eof

{- Weeding -}

wBuildMap :: [(WithOffset LabelName, MimaAddress)]
          -> Weed WeedError (Map.Map LabelName MimaAddress)
wBuildMap = foldM helper Map.empty
  where
    helper :: Map.Map LabelName MimaAddress
           -> (WithOffset LabelName, MimaAddress)
           -> Weed WeedError (Map.Map LabelName MimaAddress)
    helper m (l, addr)
      | name `Map.member` m = do
          harmless $ errorAt l "label was specified more than once"
          pure m
      | otherwise = pure $ Map.insert name addr m
        where name = woValue l

weedSymbolFile :: Map.Map MimaAddress [WithOffset LabelName]
               -> Weed WeedError (Map.Map LabelName MimaAddress)
weedSymbolFile m =
  let pairs = [(l, a) | (a, ls) <- Map.assocs m, l <- ls]
  in  wBuildMap pairs

readSymbolFile :: FilePath -> T.Text -> Either WeedErrorBundle (Map.Map LabelName MimaAddress)
readSymbolFile filename input = do
  unweeded <- parse parseSymbolFile filename input
  runWeedBundle filename input $ weedSymbolFile unweeded
