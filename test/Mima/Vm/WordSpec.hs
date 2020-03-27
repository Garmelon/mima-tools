module Mima.Vm.WordSpec (spec) where

import           Data.Word
import           Test.Hspec
import           Test.QuickCheck

import           Mima.Vm.Word

spec :: Spec
spec = do
  describe "topBit" $ do
    it "returns the top bit of 0x00" $
      topBit (0x00 :: Word8) `shouldBe` False
    it "returns the top bit of 0xff" $
      topBit (0xff :: Word8) `shouldBe` True
    it "returns the top bit of 0x7f" $
      topBit (0x7f :: Word8) `shouldBe` False
    it "returns the top bit of 0x80" $
      topBit (0x80 :: Word8) `shouldBe` True

  describe "bytesToWord" $ do
    it "converts (0xAB, 0xCD, 0xEF) to a word" $
      bytesToWord (0xAB, 0xCD, 0xEF) `shouldBe` 0xABCDEF
    it "reverses wordToBytes" $ property $ \x ->
      let word = fromInteger x
      in bytesToWord (wordToBytes word) == word

  describe "wordToBytes" $ do
    it "converts 0xABCDEF to bytes" $
      wordToBytes 0xABCDEF `shouldBe` (0xAB, 0xCD, 0xEF)
    it "reverses bytesToWord" $ property $ \x ->
      wordToBytes (bytesToWord x) == x

  describe "boolToWord" $ do
    it "converts to words correctly" $ do
      boolToWord False `shouldBe` 0x000000
      boolToWord True `shouldBe` 0xFFFFFF
    it "is reversed by topBit" $ property $ \x ->
      topBit (boolToWord x) `shouldBe` x

  describe "largeValueToWord" $ do
    it "converts values correctly" $ do
      largeValueToWord 0x00000 `shouldBe` 0x000000
      largeValueToWord 0x12345 `shouldBe` 0x012345
      largeValueToWord 0xABCDE `shouldBe` 0x0ABCDE
      largeValueToWord 0xFFFFF `shouldBe` 0x0FFFFF
    it "is reversed by getLargeValue" $ property $ \x ->
      let lv = fromInteger x
      in  getLargeValue (largeValueToWord lv) `shouldBe` lv

  describe "signedLargeValueToWord" $
    it "converts values correctly" $ do
      signedLargeValueToWord 0x00000 `shouldBe` 0x000000
      signedLargeValueToWord 0x12345 `shouldBe` 0x012345
      signedLargeValueToWord 0xABCDE `shouldBe` 0xFABCDE
      signedLargeValueToWord 0xFFFFF `shouldBe` 0xFFFFFF

  describe "signedSmallValueToLargeValue" $
    it "converts values correctly" $ do
      signedSmallValueToLargeValue 0x0000 `shouldBe` 0x00000
      signedSmallValueToLargeValue 0x1234 `shouldBe` 0x01234
      signedSmallValueToLargeValue 0xABCD `shouldBe` 0xFABCD
      signedSmallValueToLargeValue 0xFFFF `shouldBe` 0xFFFFF

  describe "wordFromSmallOpcode" $ do
    it "composes the words correctly" $ do
      wordFromSmallOpcode 0x0 0x00000 `shouldBe` 0x000000
      wordFromSmallOpcode 0x0 0xFFFFF `shouldBe` 0x0FFFFF
      wordFromSmallOpcode 0xF 0x00000 `shouldBe` 0xF00000
      wordFromSmallOpcode 0xF 0xFFFFF `shouldBe` 0xFFFFFF
      wordFromSmallOpcode 0x1 0x23456 `shouldBe` 0x123456
    it "is reversed by getLargeOpcode and getSmallValue" $ property $ \(x, y) ->
      let so = fromInteger x
          lv = fromInteger y
          word = wordFromSmallOpcode so lv
      in  getSmallOpcode word == so && getLargeValue word == lv

  describe "wordFromLargeOpcode" $ do
    it "composes the words correctly" $ do
      wordFromLargeOpcode 0x0 0x0000 `shouldBe` 0xF00000
      wordFromLargeOpcode 0x0 0xFFFF `shouldBe` 0xF0FFFF
      wordFromLargeOpcode 0xF 0x0000 `shouldBe` 0xFF0000
      wordFromLargeOpcode 0xF 0xFFFF `shouldBe` 0xFFFFFF
      wordFromLargeOpcode 0x1 0x2345 `shouldBe` 0xF12345
    it "is reversed by getLargeOpcode and getSmallValue" $ property $ \(x, y) ->
      let lo = fromInteger x
          sv = fromInteger y
          word = wordFromLargeOpcode lo sv
      in  getLargeOpcode word == lo && getSmallValue word == sv
