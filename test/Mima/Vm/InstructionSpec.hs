module Mima.Vm.InstructionSpec (spec) where

import           Data.Either
import           Data.Foldable
import           Test.Hspec

import           Mima.Vm.Instruction
import           Mima.Vm.Word

instructions :: [Instruction]
instructions =
  [ SmallInstruction ADD 0x00001
  , SmallInstruction JMN 0xAB332
  , LargeInstruction HALT 0x0000
  , LargeInstruction LDRF 0x0920
  ] ++ map fst (smallInstructions ++ largeInstructions)

smallInstructions :: [(Instruction, MimaWord)]
smallInstructions =
  [ (SmallInstruction LDC  0x0CAFE, 0x00CAFE)
  , (SmallInstruction LDV  0x0CAFE, 0x10CAFE)
  , (SmallInstruction STV  0x0CAFE, 0x20CAFE)
  , (SmallInstruction ADD  0x0CAFE, 0x30CAFE)
  , (SmallInstruction AND  0x0CAFE, 0x40CAFE)
  , (SmallInstruction OR   0x0CAFE, 0x50CAFE)
  , (SmallInstruction XOR  0x0CAFE, 0x60CAFE)
  , (SmallInstruction EQL  0x0CAFE, 0x70CAFE)
  , (SmallInstruction JMP  0x0CAFE, 0x80CAFE)
  , (SmallInstruction JMN  0x0CAFE, 0x90CAFE)
  , (SmallInstruction LDIV 0x0CAFE, 0xA0CAFE)
  , (SmallInstruction STIV 0x0CAFE, 0xB0CAFE)
  , (SmallInstruction CALL 0x0CAFE, 0xC0CAFE)
  , (SmallInstruction ADC  0x0CAFE, 0xD0CAFE)
  ]

largeInstructions :: [(Instruction, MimaWord)]
largeInstructions =
  [ (LargeInstruction HALT 0xCAFE, 0xF0CAFE)
  , (LargeInstruction NOT  0xCAFE, 0xF1CAFE)
  , (LargeInstruction RAR  0xCAFE, 0xF2CAFE)
  , (LargeInstruction RET  0xCAFE, 0xF3CAFE)
  , (LargeInstruction LDRA 0xCAFE, 0xF4CAFE)
  , (LargeInstruction STRA 0xCAFE, 0xF5CAFE)
  , (LargeInstruction LDSP 0xCAFE, 0xF6CAFE)
  , (LargeInstruction STSP 0xCAFE, 0xF7CAFE)
  , (LargeInstruction LDFP 0xCAFE, 0xF8CAFE)
  , (LargeInstruction STFP 0xCAFE, 0xF9CAFE)
  , (LargeInstruction LDRS 0xCAFE, 0xFACAFE)
  , (LargeInstruction STRS 0xCAFE, 0xFBCAFE)
  , (LargeInstruction LDRF 0xCAFE, 0xFCCAFE)
  , (LargeInstruction STRF 0xCAFE, 0xFDCAFE)
  ]

spec :: Spec
spec = do
  describe "wordToInstruction" $ do
    it "correctly recognizes all small instructions" $
      for_ smallInstructions $ \(i, w) ->
        wordToInstruction w `shouldBe` Right i
    it "correctly recognizes all large instructions" $
      for_ largeInstructions $ \(i, w) ->
        wordToInstruction w `shouldBe` Right i
    it "correctly recognizes invalid instructions" $ do
      wordToInstruction 0xE00000 `shouldSatisfy` isLeft
      wordToInstruction 0xFE0000 `shouldSatisfy` isLeft
      wordToInstruction 0xFF0000 `shouldSatisfy` isLeft
    it "reverses instructionToWord" $
      for_ instructions $ \i ->
        wordToInstruction (instructionToWord i) `shouldBe` Right i

  describe "instructionToWord" $ do
    it "correctly converts all small instructions" $
      for_ smallInstructions $ \(i, w) ->
        instructionToWord i `shouldBe` w
    it "correctly converts all large instructions" $
      for_ largeInstructions $ \(i, w) ->
        instructionToWord i `shouldBe` w
