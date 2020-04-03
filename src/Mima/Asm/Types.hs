module Mima.Asm.Types
  ( Onion(..)
  ) where

class Onion o where
  peel :: o a -> a
