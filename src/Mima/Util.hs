{-# LANGUAGE FlexibleInstances #-}

module Mima.Util
  (
  -- * Formatting
    ToText(..)
  ) where

import qualified Data.Text as T

{- Formatting -}

-- | A class for types that can be converted to 'T.Text'.
--
-- This class does not mean to convert elements to text in a
-- standardized way. It is just to reduce the clutter of functions
-- with names like @somethingToText@.
--
-- Only create an instance of this class when there is an obvious,
-- preferrable way of converting something to text! If there are
-- multiple "obvious" options, create no instance of this class and
-- instead name the functions individually.
class ToText a where
  toText :: a -> T.Text
