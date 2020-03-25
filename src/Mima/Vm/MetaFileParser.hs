{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Mima.Vm.MetaFileParser
  ( Metadata(..)
  , GlobalData
  , LocalData
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import           Data.Traversable
import           Mima.Vm.Word
import           Numeric

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
  return $ Map.fromList v
  where
    unpackInner (key, nestedValue) = do
      address <- readMimaAddress (T.unpack key)
      innerValues <- parseJSON nestedValue
      return (address, innerValues)

readMimaAddress :: String -> Parser MimaAddress
readMimaAddress input = case readHex input of
  [(num, "")] -> boundedAddress input num
  _           -> fail $ "Couldn't read " ++ input ++ "' as a mima address!"

boundedAddress :: String -> Integer -> Parser MimaAddress
boundedAddress rawInput value =
  if value > maxVal || value < minVal then
    fail $ "Value '" ++ rawInput ++ "' out of bounds for an address."
  else
    pure $ fromIntegral value
  where
    maxVal = fromIntegral (maxBound :: MimaAddress)
    minVal = fromIntegral (minBound :: MimaAddress)

localDataToJson :: LocalData -> Value
localDataToJson outerMap = object $ map formatInnerEntry (Map.toList outerMap)
  where
    formatInnerEntry (address, innerMap) = T.pack (showHex address "") .= toJSON innerMap
