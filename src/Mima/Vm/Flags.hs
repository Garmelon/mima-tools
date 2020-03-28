{-# LANGUAGE OverloadedStrings #-}
module Mima.Vm.Flags
  ( Flags(..)
  -- * Methods for interacting with 'Flags'
  , readonlyAt
  , executableAt
  , breakpointAt
  -- * Conversion methods for 'Metadata'
  , flagsFromMetadata
  , sampleMeta
  ) where

import qualified Data.Aeson.Types as A
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text        as T

import           Mima.Run
import           Mima.Vm.Metadata
import           Mima.Vm.Storage
import           Mima.Vm.Word

-- | A collection of parsed flags in a more efficient representation than
--   'Metadata'
data Flags = Flags
  { flagReadonly   :: Set.Set MimaAddress
  , flagExecutable :: Set.Set MimaAddress
  , flagBreakpoint :: Set.Set MimaAddress
  } deriving Show

instance Semigroup Flags where
  (Flags a1 b1 c1) <> (Flags a2 b2 c2) = Flags (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid Flags where
  mempty = Flags mempty mempty mempty

-- | Checks if a given address has the "readonly" flag set.
readonlyAt :: Flags -> MimaAddress -> Bool
readonlyAt flags address = Set.member address (flagReadonly flags)

-- | Checks if a given address has the "executable" flag set.
--   If the given 'Flags' has no 'flagExecutable', this method will
--   return 'True'.
executableAt :: Flags -> MimaAddress -> Bool
executableAt flags address
  | Set.null set = True
  | otherwise    = Set.member address set
  where
    set = flagExecutable flags

-- | Checks if a given address has a the "breakpoint" flag set.
breakpointAt :: Flags -> MimaAddress -> Bool
breakpointAt flags address = Set.member address (flagBreakpoint flags)


{- Conversion from Metadata -}

flagsFromMetadata :: Metadata -> Flags
flagsFromMetadata metadata =
  Flags (flagSet "readonly") (flagSet "executable") (flagSet "breakpoint")
  where
    ranges = mdLocal metadata
    rangesToMap key = mconcat . reverse . map (rangeToMap key) $ ranges
    flagSet = Map.keysSet . Map.filter valueToBool . rangesToMap

rangeToMap :: T.Text -> Range -> Map.Map MimaAddress A.Value
rangeToMap key range = case getMetaInfo range Map.!? key of
  Nothing -> mempty
  Just v  -> Map.fromList $ zip (getAddresses range) (repeat v)

valueToBool :: A.Value -> Bool
valueToBool (A.Bool False) = False
valueToBool _              = True

sampleMeta :: Run Flags
sampleMeta = do
  m <- loadMetadata "test/files/FlaggyMetadataFile.json"
  pure $ flagsFromMetadata m
