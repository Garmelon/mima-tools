module Mima.Asm.Phase1
  ( Onion(..)
  -- * Types
  , Name(..)
  , Address(..)
  , Location(..)
  , SmallOpcode(..)
  , LargeOpcode(..)
  , MimaWord(..)
  , SmallValue(..)
  , Instruction(..)
  , RegisterDirective(..)
  , JsonValue(..)
  , Directive(..)
  -- * Parsers
  , Phase1
  , parsePhase1
  ) where

import qualified Data.Aeson          as A
import           Data.Char
import qualified Data.Text           as T
import           Data.Void
import           Text.Megaparsec

import qualified Mima.Vm.Instruction as Vm
import qualified Mima.Vm.Word        as Vm

{-
<value>       := <word> | <address>
<address>     := <absolute address> | <relative address> | <label>
<name>        := [a-z]([a-z0-9_]*)
<label>       := <name>:
<instruction> := as usual

.reg ACC <value>
.reg <other register> <address>

.org (<absolute address> | <positive relative address> | <negative address>) ???
.lit <value>
.arr [<value>]

.meta <name> <json value>
.meta-start <name> [<json value>]
.meta-stop <name>
-}

class Onion o where
  peel :: o a -> a

{- Types -}

data Name a = Name a T.Text
  deriving (Show)

instance Onion Name where
  peel (Name a _) = a

data Address a
  = AddressAbsolute a Vm.MimaAddress
  | AddressRelative a Int
  deriving (Show)

instance Onion Address where
  peel (AddressAbsolute a _) = a
  peel (AddressRelative a _) = a

data Location a
  = LocationAddress (Address a)
  | LocationLabel (Name a)
  deriving (Show)

instance Onion Location where
  peel (LocationAddress a) = peel a
  peel (LocationLabel a)   = peel a

data SmallOpcode a = SmallOpcode a Vm.SmallOpcode
  deriving (Show)

instance Onion SmallOpcode where
  peel (SmallOpcode a _) = a

data LargeOpcode a = LargeOpcode a Vm.LargeOpcode
  deriving (Show)

instance Onion LargeOpcode where
  peel (LargeOpcode a _) = a

data MimaWord a
  = WordRaw a Vm.MimaWord
  | WordLocation (Location a)
  deriving (Show)

instance Onion MimaWord where
  peel (WordRaw a _)    = a
  peel (WordLocation a) = peel a

data SmallValue a = SmallValue a Vm.SmallValue
  deriving (Show)

instance Onion SmallValue where
  peel (SmallValue a _) = a

data Instruction a
  = SmallInstruction a (SmallOpcode a) (Location a)
  | LargeInstruction a (LargeOpcode a) (SmallValue a)
  deriving (Show)

instance Onion Instruction where
  peel (SmallInstruction a _ _) = a
  peel (LargeInstruction a _ _) = a

data RegisterDirective a
  = RegIar a (Location a)
  | RegAcc a (MimaWord a)
  | RegRa a (Location a)
  | RegSp a (Location a)
  | RegFp a (Location a)
  deriving (Show)

instance Onion RegisterDirective where
  peel (RegIar a _) = a
  peel (RegAcc a _) = a
  peel (RegRa  a _) = a
  peel (RegSp  a _) = a
  peel (RegFp  a _) = a

data JsonValue a = JsonValue a A.Value
  deriving (Show)

instance Onion JsonValue where
  peel (JsonValue a _) = a

-- | The first @a@ parameter represents the span of the whole thing. The second
-- @a@ parameter represents the span of the directive literal (e. g. @.org@).
data Directive a
  = Reg       a a (RegisterDirective a)
  | Org       a a (Address a)
  | Lit       a a (MimaWord a)
  | Arr       a a [MimaWord a]
  | Meta      a a (Name a) (JsonValue a)
  | MetaStart a a (Name a) (JsonValue a)
  | MetaStop  a a (Name a)
  deriving (Show)

instance Onion Directive where
  peel (Reg       a _ _)   = a
  peel (Org       a _ _)   = a
  peel (Lit       a _ _)   = a
  peel (Arr       a _ _)   = a
  peel (Meta      a _ _ _) = a
  peel (MetaStart a _ _ _) = a
  peel (MetaStop  a _ _)   = a

data AsmToken a
  = TokenLabel (Name a)
  | TokenInstruction (Instruction a)
  | TokenDirective (Directive a)
  | TokenComment a T.Text
  deriving (Show)

instance Onion AsmToken where
  peel (TokenLabel a)       = peel a
  peel (TokenInstruction a) = peel a
  peel (TokenDirective a)   = peel a
  peel (TokenComment a _)   = a

{- Parsers -}

type Parser = Parsec Void T.Text

data Span = Span SourcePos SourcePos
  deriving (Show)

withSpan :: Parser a -> Parser (a, Span)
withSpan f = do
  start <- getSourcePos
  result <- f
  stop <- getSourcePos
  pure (result, Span start stop)

name :: Parser (Name Span)
name = do
  (a, s) <- withSpan $ do
    firstChar <- satisfy isLower <?> "lowercase character"
    otherChars <- takeWhileP (Just "alphanumeric character") isAlphaNum
    pure $ T.pack [firstChar] <> otherChars
  pure $ Name s a

type Phase1 = [AsmToken Span]

parsePhase1 :: Parser Phase1
parsePhase1 = undefined name
