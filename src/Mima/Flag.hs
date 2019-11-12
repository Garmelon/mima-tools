{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mima.Flag
  ( AddressRange
  , lowerAddress
  , upperAddress
  , range
  , rangeToAddresses
  , rangeContains
  , simplifyRanges
  , AddressSpec
  , rangesToSpec
  , specToRanges
  , specNull
  , specContains
  , Flag(..)
  , allFlags
  , flagChar
  , Flags(..)
  , rawFlags
  , flagChecks
  , impotentChecks
  ) where

import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Mima.Word

data AddressRange = AddressRange
  { lowerAddress :: MimaAddress
  , upperAddress :: MimaAddress
  } deriving (Show, Eq, Ord)

range :: MimaAddress -> MimaAddress -> AddressRange
range a b
  | a <= b    = AddressRange a b
  | otherwise = AddressRange b a

rangeToAddresses :: AddressRange -> [MimaAddress]
rangeToAddresses r = [lowerAddress r..upperAddress r]

rangeContains :: AddressRange -> MimaAddress -> Bool
rangeContains (AddressRange a b) c = a <= c && c <= b

simplifyRanges :: [AddressRange] -> [AddressRange]
simplifyRanges = helper . sort
  where
    helper :: [AddressRange] -> [AddressRange]
    helper (r1:r2:rs)
      | upperAddress r1 >= lowerAddress r2 = helper (merge r1 r2 : rs)
      | otherwise                          = r1 : helper (r2:rs)
    helper a = a

    merge :: AddressRange -> AddressRange -> AddressRange
    merge (AddressRange a1 b1) (AddressRange _ b2) = AddressRange a1 (max b1 b2)


newtype AddressSpec = AddressSpec [AddressRange]
  deriving (Show)

rangesToSpec :: [AddressRange] -> AddressSpec
rangesToSpec = AddressSpec . simplifyRanges

specToRanges :: AddressSpec -> [AddressRange]
specToRanges (AddressSpec ranges) = ranges

specNull :: AddressSpec -> Bool
specNull = null . specToRanges

specContains :: AddressSpec -> MimaAddress -> Bool
specContains as addr = any (`rangeContains` addr) $ specToRanges as

{- Enough preamble, let's get to the flags -}

data Flag = Breakpoint | Executable | ReadOnly
  deriving (Show, Eq, Ord)

allFlags :: [Flag]
allFlags = [Breakpoint, Executable, ReadOnly]

flagChar :: Flag -> Char
flagChar Breakpoint = 'b'
flagChar Executable = 'e'
flagChar ReadOnly   = 'r'

data Flags a = Flags
  { flagBreakpoint :: a
  , flagExecutable :: a
  , flagReadOnly   :: a
  } deriving (Show)

instance Functor Flags where
  fmap f Flags{..} = Flags
    { flagBreakpoint = f flagBreakpoint
    , flagExecutable = f flagExecutable
    , flagReadOnly   = f flagReadOnly
    }

instance Applicative Flags where
  pure a = Flags a a a
  f <*> a = Flags
    { flagBreakpoint = flagBreakpoint f $ flagBreakpoint a
    , flagExecutable = flagExecutable f $ flagExecutable a
    , flagReadOnly   = flagReadOnly f   $ flagReadOnly a
    }

rawFlags :: Flags Flag
rawFlags = Flags
  { flagBreakpoint = Breakpoint
  , flagExecutable = Executable
  , flagReadOnly   = ReadOnly
  }

flagChecks :: Map.Map AddressRange (Set.Set Flag) -> Flags (MimaAddress -> Bool)
flagChecks m =
  let getAddressSpec :: Flag -> AddressSpec
      getAddressSpec f = rangesToSpec $ map fst $ filter (Set.member f . snd) $ Map.assocs m

      conditions :: Flags (AddressSpec -> MimaAddress -> Bool)
      conditions = Flags
        { flagBreakpoint = specContains
        , flagExecutable = \as -> if specNull as then const True else specContains as
        , flagReadOnly = specContains
        }
  in  conditions <*> (getAddressSpec <$> rawFlags)

-- | Flag checks that should not alter the behaviour of the MiMa.
impotentChecks :: Flags (MimaAddress -> Bool)
impotentChecks = Flags
  { flagBreakpoint = const False
  , flagExecutable = const True
  , flagReadOnly   = const False
  }
