module Mima.Parse.Assembly.Weed.Common
  ( Registers(..)
  , emptyRegisters
  , AlmostWord(..)
  , WeedResult(..)
  , emptyResult
  ) where

import qualified Data.Map as Map

import           Mima.Flag
import           Mima.Label
import           Mima.Parse.Assembly.RawInstruction
import           Mima.Word

data Registers a = Registers
  { rIAR :: Maybe a
  , rACC :: Maybe MimaWord
  , rRA  :: Maybe a
  , rSP  :: Maybe a
  , rFP  :: Maybe a
  } deriving (Show)

emptyRegisters :: Registers a
emptyRegisters = Registers
  { rIAR = Nothing
  , rACC = Nothing
  , rRA  = Nothing
  , rSP  = Nothing
  , rFP  = Nothing
  }

data AlmostWord a
  = AInstruction (RawInstruction a)
  | ALiteral MimaWord
  deriving (Show)

data WeedResult a = WeedResult
  { wrRegisters :: Registers a
  , wrMemory    :: Map.Map MimaAddress (AlmostWord a)
  , wrLabels    :: Map.Map LabelName MimaAddress
  , wrFlags     :: Map.Map Char [AddressRange]
  } deriving (Show)

emptyResult :: WeedResult a
emptyResult = WeedResult
  { wrRegisters = emptyRegisters
  , wrMemory    = Map.empty
  , wrLabels    = Map.empty
  , wrFlags     = Map.empty
  }
