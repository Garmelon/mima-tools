{-# LANGUAGE OverloadedStrings #-}

module Mima.Format.Instruction
  ( formatLargeValue
  , formatSmallValue
  , formatSmallOpcode
  , formatLargeOpcode
  , formatInstruction
  ) where

import qualified Data.Text as T

import           Mima.Format.Common
import           Mima.Instruction
import           Mima.Word

formatLargeValue :: LargeValue -> T.Text
formatLargeValue = negative toDec

formatSmallValue :: SmallValue -> T.Text
formatSmallValue = negative toDec

formatSmallOpcode :: SmallOpcode -> T.Text
formatSmallOpcode = T.pack . show

formatLargeOpcode :: LargeOpcode -> T.Text
formatLargeOpcode = T.pack . show

formatInstruction :: Instruction -> T.Text
formatInstruction (SmallInstruction so lv) = formatSmallOpcode so <> " " <> formatLargeValue lv
formatInstruction (LargeInstruction lo sv) = formatLargeOpcode lo <> " " <> formatSmallValue sv
