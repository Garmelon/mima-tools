module Mima.Assembler.Parser.Label
  ( MimaLabel
  , mimaLabel
  , mimaLabel'
  , failAtLabel
  , resolveLabel
  , Address
  , address
  , resolveAddress
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import           Text.Megaparsec

import           Mima.Assembler.Parser.Basic
import           Mima.Word

{- Labels -}

data MimaLabel = MimaLabel { lName :: T.Text, lOffset :: Int }
  deriving (Show)

instance Eq MimaLabel where
  a == b = lName a == lName b

instance Ord MimaLabel where
  compare a b = compare (lName a) (lName b)

mimaLabel :: Parser MimaLabel
mimaLabel = lexeme mimaLabel'

mimaLabel' :: Parser MimaLabel
mimaLabel' = label "label" $ do
  name <- takeWhile1P Nothing (\c -> isAlphabet c || isConnecting c)
  offset <- getOffset
  pure MimaLabel{lName = name, lOffset = offset}

failAtLabel :: MimaLabel -> String -> Parser a
failAtLabel l = failAt (lOffset l)

resolveLabel :: Map.Map MimaLabel MimaAddress -> MimaLabel -> Parser MimaAddress
resolveLabel lmap l =
  case lmap Map.!? l of
    Just addr -> pure addr
    Nothing   -> failAtLabel l "could not resolve label"

{- Addresses -}

data Address = Direct LargeValue | Indirect MimaLabel
  deriving (Show)

address :: Parser Address
address = try (Direct <$> largeValue) <|> (Indirect <$> mimaLabel)

resolveAddress :: Map.Map MimaLabel MimaAddress -> Address -> Parser MimaAddress
resolveAddress _      (Direct addr) = pure addr
resolveAddress labels (Indirect l)  = resolveLabel labels l
