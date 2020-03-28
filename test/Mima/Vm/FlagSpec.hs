{-# LANGUAGE OverloadedStrings #-}
module Mima.Vm.FlagSpec (spec) where

import qualified Data.Aeson.Types as A
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import           Test.Hspec
import           Test.QuickCheck

import           Mima.Vm.Flags
import           Mima.Vm.Metadata
import           Mima.Vm.Word

readOnlyFlags :: [MimaAddress] -> Flags
readOnlyFlags addresses = Flags (Set.fromList addresses) mempty mempty

executeFlags :: [MimaAddress] -> Flags
executeFlags addresses = Flags mempty (Set.fromList addresses) mempty

breakpointFlags :: [MimaAddress] -> Flags
breakpointFlags addresses = Flags mempty mempty (Set.fromList addresses)

executableBetween :: Bool -> MimaAddress -> MimaAddress -> Metadata
executableBetween executable start stop = Metadata mempty
  [
    RangeFromTo (Map.fromList [("executable", A.Bool executable)]) start stop
  ]

overlappingExecutableFlags :: Flags
overlappingExecutableFlags = flags
  where
    baseMetadata = executableBetween True 1 20
    metadata = baseMetadata <> executableBetween False 4 6
    flags = flagsFromMetadata metadata

tripleOverlappingExecutableFlags :: Flags
tripleOverlappingExecutableFlags = flags
  where
    baseMetadata = executableBetween True 1 20
    metadata = baseMetadata
             <> executableBetween False 4 6
             <> executableBetween True 5 6
    flags = flagsFromMetadata metadata

spec :: Spec
spec = do
  describe "readonly getter works" $
    it "returns the correct set value" $ do
      readonlyAt (readOnlyFlags [2, 5]) 2 `shouldBe` True
      readonlyAt (readOnlyFlags [2, 5]) 5 `shouldBe` True
      readonlyAt (readOnlyFlags [2, 5]) 3 `shouldBe` False

  describe "execute getter works" $ do
    it "returns the correct set value" $ do
      executableAt (executeFlags [20, 200]) 2 `shouldBe` False
      executableAt (executeFlags [20, 200]) 20 `shouldBe` True
      executableAt (executeFlags [20, 200]) 200 `shouldBe` True
    it "returns true if none are set" $ property $ \x ->
      let word = fromInteger x
      in executableAt mempty word

  describe "breakpoint getter works" $
    it "returns the correct set value" $ do
      breakpointAt (breakpointFlags [20, 200]) 2 `shouldBe` False
      breakpointAt (breakpointFlags [20, 200]) 20 `shouldBe` True
      breakpointAt (breakpointFlags [20, 200]) 200 `shouldBe` True

  context "with nested ranges" $ do
    it "returns the correct value for unaffected areas" $ do
      let flags = overlappingExecutableFlags
      executableAt flags  1 `shouldBe` True
      executableAt flags  3 `shouldBe` True
      executableAt flags  7 `shouldBe` True
      executableAt flags 20 `shouldBe` True
    it "returns the correct value for affected areas" $ do
      let flags = overlappingExecutableFlags
      executableAt flags 4 `shouldBe` False
      executableAt flags 5 `shouldBe` False
      executableAt flags 6 `shouldBe` False
    it "returns the correct value for triple affected areas" $ do
      let flags = tripleOverlappingExecutableFlags
      executableAt flags  1 `shouldBe` True
      executableAt flags  4 `shouldBe` False
      executableAt flags  5 `shouldBe` True
      executableAt flags  6 `shouldBe` True
      executableAt flags 20 `shouldBe` True
