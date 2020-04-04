module Mima.Asm.Weed
  ( Weed
  , runWeed
  , transformErrors
  , critical
  , harmless
  , WeedError(..)
  , errorWith
  -- * Megaparsec compatibility
  , defaultPosState
  , asParseErrors
  , runWeedBundle
  ) where

import qualified Data.List.NonEmpty as NE
import           Data.Monoid
import qualified Data.Set           as Set
import           Text.Megaparsec

-- The star of the show
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
      Left e             -> Weed es1 (Left e)
      Right (Weed es2 a) -> Weed (es1 <> es2) a

runWeed :: Weed e a -> Either (NE.NonEmpty e) a
-- Since the Endos never remove an element and we add an extra
-- element, this list is never empty.
--
-- I've tried to figure out nicer types for this, but if I want to
-- keep the Endo trick, the tradeoff isn't worth it. The problem here
-- is that I can't easily check if 'es' is 'mempty' with these
-- endofunctors.
runWeed (Weed es (Left e))  = Left $ e NE.:| appEndo es []
runWeed (Weed es (Right a)) =
  case appEndo es [] of
    (x:xs) -> Left $ x NE.:| xs
    []     -> Right a

transformErrors :: (e1 -> e2) -> Weed e1 a -> Weed e2 a
transformErrors f (Weed es result) = Weed es' result'
 where
   es' = Endo $ (++) $ map f $ appEndo es []
   result' = case result of
     Right a -> Right a
     Left e  -> Left $ f e

critical :: e -> Weed e a
critical e = Weed mempty (Left e)

harmless :: e -> Weed e ()
harmless e = Weed (Endo (e:)) (Right ())

data WeedError a = WeedError a String
  deriving (Show)

instance Functor WeedError where
  fmap f (WeedError a s) = WeedError (f a) s

errorWith :: a -> String -> WeedError a
errorWith = WeedError

{- Megaparsec compatibility -}

defaultPosState :: FilePath -> s -> PosState s
defaultPosState filename input = PosState
  { pstateInput      = input
  , pstateOffset     = 0
  , pstateSourcePos  = initialPos filename
  , pstateTabWidth   = defaultTabWidth
  , pstateLinePrefix = ""
  }

asParseErrors :: Weed (WeedError Int) a -> Weed (ParseError s e) a
asParseErrors = transformErrors toParseError
  where
    toParseError (WeedError offset msg) =
      FancyError offset $ Set.singleton $ ErrorFail msg

runWeedBundle :: FilePath -> s -> Weed (ParseError s e) a -> Either (ParseErrorBundle s e) a
runWeedBundle filename input w = case runWeed w of
  Left errors -> Left $ ParseErrorBundle errors $ defaultPosState filename input
  Right a     -> Right a
