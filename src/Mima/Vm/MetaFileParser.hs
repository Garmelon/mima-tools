{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Mima.Vm.MetaFileParser
  ( Metadata(..)
  , GlobalData
  , LocalData
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict          as HM
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as T
import           Data.Traversable
import           Numeric
import           Text.ParserCombinators.ReadP

import           Mima.Format
import           Mima.Vm.Word

data Metadata = Metadata
 { global :: GlobalData
 , local  :: LocalData
 } deriving Show

type GlobalData = Map.Map T.Text Value
type LocalData = Map.Map MimaAddress (Map.Map T.Text Value)

instance FromJSON Metadata where
  parseJSON = withObject "root" $ \obj -> do
    globalData <- obj .: "global"
    localData <- parseLocalData =<< obj .: "local"
    return $ Metadata globalData localData

instance ToJSON Metadata where
  toJSON Metadata{local, global} = object
   [ "global" .= toJSON global
   , "local" .= toJSON (localDataToJson local)
   ]

parseLocalData :: Value -> Parser LocalData
parseLocalData = withObject "local" $ \obj -> do
  v <- for (HM.toList obj) unpackInner
  return $ Map.fromList (concat v)
  where
    unpackInner (key, nestedValue) = do
      (start, end) <- readMimaAddress (T.unpack key)
      innerValues <- parseJSON nestedValue
      return $ zip [start..end] (repeat innerValues)

readMimaAddress :: String -> Parser (MimaAddress, MimaAddress)
readMimaAddress input = case readP_to_S readFromHex input of
  [(addresses, [])] -> pure addresses
  xs                -> fail $ "'" ++ input ++ "' is no address, " ++ show xs

readFromHex :: ReadP (MimaAddress,MimaAddress)
readFromHex = (range <|> single) <* eof
  where
    range = (,) <$> (boundedAddress <* char '-') <*> boundedAddress
    single = (\x -> (x, x)) <$> boundedAddress

boundedAddress :: ReadP MimaAddress
boundedAddress = do
  value <- readS_to_P readHex :: ReadP Integer
  if value > maxVal || value < minVal then
    fail $ T.unpack $ "Value '" <> toHex value <> "' out of bounds for an address."
  else
    pure $ fromIntegral value
  where
    maxVal = fromIntegral (maxBound :: MimaAddress)
    minVal = fromIntegral (minBound :: MimaAddress)

localDataToJson :: LocalData -> Value
localDataToJson outerMap = object $ map formatInnerEntry (Map.toList outerMap)
  where
    formatInnerEntry (address, innerMap) = fixWidthHex 5 (toHex address) .= toJSON innerMap
