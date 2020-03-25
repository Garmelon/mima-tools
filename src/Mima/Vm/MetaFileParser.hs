{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Mima.Vm.MetaFileParser
  ( Metadata(..)
  , AddressRange(..)
  , Range(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T

import           Mima.Format
import           Mima.Vm.Word

data Metadata = Metadata
 { global :: Map.Map T.Text Value
 , local  :: [Range]
 } deriving Show

data AddressRange
  = RangeFromTo MimaAddress MimaAddress
  | RangeAt     MimaAddress
  deriving Show

data Range = Range AddressRange (Map.Map T.Text Value)
  deriving Show

instance FromJSON AddressRange where
  parseJSON = withObject "address range" $ \obj -> at obj <|> range obj
    where
      at obj = RangeAt <$> ((obj .: "at") >>= boundedAddress)
      range obj = do
        start <- obj .: "start" >>= boundedAddress
        stop <- obj .: "stop" >>= boundedAddress
        when (start > stop) (fail "start must be <= stop")
        return $ RangeFromTo start stop

instance ToJSON AddressRange where
  toJSON = Object . addressRangeToMap

addressRangeToMap :: AddressRange -> Object
addressRangeToMap (RangeAt addr) = HM.fromList ["at" .= T.unpack (toHex addr)]
addressRangeToMap (RangeFromTo start stop) = HM.fromList
  [ "start" .= T.unpack (toHex start)
  , "stop" .= T.unpack (toHex stop)
  ]

instance FromJSON Range where
  parseJSON value = do
    addressRange <- parseJSON value
    obj <- parseJSON value :: Parser Object
    values <- obj .: "values"
    pure $ Range addressRange values

instance ToJSON Range where
  toJSON (Range addressRange value) = Object (addressRangeToMap addressRange <> HM.fromList (Map.toList value))

instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \obj ->
    Metadata <$> obj .: "global" <*> obj .: "local"

instance ToJSON Metadata where
  toJSON Metadata{local, global} = object
   [ "global" .= toJSON global
   , "local" .= toJSON local
   ]

boundedAddress :: Value -> Parser MimaAddress
boundedAddress value = do
  n <- parseJSON value :: Parser Integer
  unless (n >= minVal && n <= maxVal) $
    fail $ T.unpack $
    "Value '" <> toHex n <> "' out of bounds for an address."
  pure $ fromIntegral n
  where
    maxVal = fromIntegral (maxBound :: MimaAddress)
    minVal = fromIntegral (minBound :: MimaAddress)
