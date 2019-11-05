module Mima.Word where

import           Data.Bits
import           Data.Word

import           Mima.Util

class Word32Based t where
  fromWord32 :: Word32 -> t
  toWord32 :: t -> Word32

-- The MiMa's words are 24 bits long. The smallest word they fit in is
-- 'Word32'.

newtype MimaWord = MimaWord Word32
  deriving (Show, Eq)

instance ToText MimaWord where
  toText (MimaWord w) = toHex 6 w

instance Word32Based MimaWord where
  fromWord32 w = MimaWord $ w .&. 0x00FFFFFF
  toWord32 (MimaWord w) = w

-- The MiMa's addresses are 20 bits long. The smallest word they fit
-- in is 'Word32'.

newtype MimaAddress = MimaAddress Word32
  deriving (Show, Eq)

instance ToText MimaAddress where
  toText (MimaAddress w) = toHex 5 w

instance Word32Based MimaAddress where
  fromWord32 w = MimaAddress $ w .&. 0x000FFFFF
  toWord32 (MimaAddress w) = w
