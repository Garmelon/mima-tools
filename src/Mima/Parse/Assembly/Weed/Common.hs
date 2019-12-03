module Mima.Parse.Assembly.Weed.Common
  ( Registers(..)
  , emptyRegisters
  , registersToState
  , AlmostWord(..)
  , WeedResult(..)
  , emptyResult
  ) where

import qualified Data.Map as Map
import           Data.Maybe

import           Mima.Flag
import           Mima.Label
import           Mima.Parse.Assembly.RawInstruction
import           Mima.State
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

registersToState :: Registers MimaAddress -> MimaMemory -> MimaState
registersToState r = MimaState (fromMaybe 0 $ rIAR r) (fromMaybe 0 $ rACC r)
  (fromMaybe 0 $ rRA r) (fromMaybe 0 $ rSP r) (fromMaybe 0 $ rFP r)

data AlmostWord a
  = AInstruction (RawInstruction a)
  | ALiteral MimaWord
  deriving (Show)

data WeedResult a = WeedResult
  { wrRegisters :: Registers a
  , wrMemory    :: Map.Map MimaAddress (AlmostWord a)
  , wrLabels    :: Map.Map LabelName MimaAddress
  , wrFlags     :: RawFlags
  } deriving (Show)

emptyResult :: WeedResult a
emptyResult = WeedResult
  { wrRegisters = emptyRegisters
  , wrMemory    = Map.empty
  , wrLabels    = Map.empty
  , wrFlags     = Map.empty
  }
