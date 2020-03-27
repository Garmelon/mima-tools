{-# LANGUAGE OverloadedStrings #-}

module Mima.Vm.Metadata
  ( MetaInfo
  , Range(..)
  , getMetaInfo
  , Metadata(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T

import           Mima.Vm.Word

type MetaInfo = Map.Map T.Text Value

data Range
  = RangeAt     MetaInfo MimaAddress
  | RangeFromTo MetaInfo MimaAddress MimaAddress
  deriving Show

getMetaInfo :: Range -> MetaInfo
getMetaInfo (RangeAt     info _)   = info
getMetaInfo (RangeFromTo info _ _) = info

data Metadata = Metadata
 { mdGlobal :: MetaInfo
 , mdLocal  :: [Range]
 } deriving Show

instance Semigroup Metadata where
  md1 <> md2 =
    Metadata (mdGlobal md1 <> mdGlobal md2) (mdLocal md1 <> mdLocal md2)

instance Monoid Metadata where
  mempty = Metadata mempty mempty

boundedAddress :: Parser Value -> Parser MimaAddress
boundedAddress p = do
  n <- parseJSON =<< p :: Parser Integer
  unless (n >= minVal && n <= maxVal) $
    fail $ "value " ++ show n ++  " out of bounds for an address"
  pure $ fromIntegral n
  where
    maxVal = fromIntegral (maxBound :: MimaAddress)
    minVal = fromIntegral (minBound :: MimaAddress)

instance FromJSON Range where
  parseJSON = withObject "range" $ \o -> do
    info <- parseJSON =<< o .: "info"
    rangeAt info o <|> rangeFromTo info o
    where
      rangeAt info o = RangeAt info <$> boundedAddress (o .: "at")
      rangeFromTo info o = do
        start <- boundedAddress (o .: "start")
        stop <- boundedAddress (o .: "stop")
        when (start > stop) (fail "start must not be greater than stop")
        pure $ RangeFromTo info start stop

instance ToJSON Range where
  toJSON (RangeAt info at) = object
    [ "at"   .= toJSON (toInteger at)
    , "info" .= toJSON info
    ]
  toJSON (RangeFromTo info start stop) = object
    [ "start" .= toJSON (toInteger start)
    , "stop"  .= toJSON (toInteger stop)
    , "info" .= toJSON info
    ]

instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \obj ->
    Metadata <$> obj .: "global" <*> obj .: "local"

instance ToJSON Metadata where
  toJSON md = object
    [ "global" .= toJSON (mdGlobal md)
    , "local" .= toJSON (mdLocal md)
    ]
