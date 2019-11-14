{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mima.Parse.Weed
  ( Weed
  , runWeed
  , critical
  , harmless
  ) where

import qualified Data.List.NonEmpty as NE
import           Data.Monoid

data Weed e a = Weed (Endo [e]) (Either e a)

instance Functor (Weed e) where
  fmap f (Weed e a) = Weed e $ fmap f a

instance Applicative (Weed e) where
  pure = Weed mempty . pure
  (Weed es1 (Left e1)) <*> (Weed es2 (Left e2)) = Weed (es1 <> Endo (e1:) <> es2) (Left e2)
  (Weed es1 f)         <*> (Weed es2 a)         = Weed (es1 <> es2) (f <*> a)

instance Monad (Weed e) where
  (Weed es1 v) >>= f =
    case f <$> v of
      Left e -> Weed es1 (Left e)
      Right (Weed es2 a) -> Weed (es1 <> es2) a

runWeed :: Weed e a -> Either (NE.NonEmpty e) a
-- Since the Endos never remove an element and we add an extra
-- element, this list is never empty.
--
-- I've tried to figure out nicer types for this, but if I want to
-- keep the Endo trick, the tradeoff isn't worth it. The problem here
-- is that I can't easily check if 'es' is 'mempty' with these
-- endofunctors.
runWeed (Weed es (Left e))  = Left $ NE.fromList $ appEndo es [e]
runWeed (Weed es (Right a)) =
  case appEndo es [] of
    (x:xs) -> Left $ x NE.:| xs
    []     -> Right a

critical :: e -> Weed e a
critical e = Weed mempty (Left e)

harmless :: e -> Weed e ()
harmless e = Weed (Endo (e:)) (Right ())
