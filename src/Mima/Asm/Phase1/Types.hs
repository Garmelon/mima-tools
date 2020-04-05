{-# LANGUAGE DeriveFunctor #-}

module Mima.Asm.Phase1.Types
  ( Name(..)
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
  , AsmToken(..)
  , Phase1
  ) where

import qualified Data.Aeson          as A
import qualified Data.Text           as T

import           Mima.Asm.Types
import qualified Mima.Vm.Instruction as Vm

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

{- Types -}

data Name a = Name a T.Text
  deriving (Show, Functor)

instance Onion Name where
  peel (Name a _) = a

data Address a
  = AddressAbsolute a Integer
  | AddressRelative a Integer
  deriving (Show, Functor)

instance Onion Address where
  peel (AddressAbsolute a _) = a
  peel (AddressRelative a _) = a

data Location a
  = LocationAddress (Address a)
  | LocationLabel (Name a)
  | LocationLabelRel a (Name a) a Integer
  -- ^ @Lo LocationLabelRel completeSpan name offsetSpan offset@
  deriving (Show, Functor)

instance Onion Location where
  peel (LocationAddress a)        = peel a
  peel (LocationLabel a)          = peel a
  peel (LocationLabelRel a _ _ _) = a

data SmallOpcode a = SmallOpcode a Vm.SmallOpcode
  deriving (Show, Functor)

instance Onion SmallOpcode where
  peel (SmallOpcode a _) = a

data LargeOpcode a = LargeOpcode a Vm.LargeOpcode
  deriving (Show, Functor)

instance Onion LargeOpcode where
  peel (LargeOpcode a _) = a

data MimaWord a
  = WordRaw a Integer
  | WordLocation (Location a)
  deriving (Show, Functor)

instance Onion MimaWord where
  peel (WordRaw a _)    = a
  peel (WordLocation a) = peel a

data SmallValue a = SmallValue a Integer
  deriving (Show, Functor)

instance Onion SmallValue where
  peel (SmallValue a _) = a

data Instruction a
  = SmallInstruction a (SmallOpcode a) (Location a)
  | LargeInstruction a (LargeOpcode a) (Maybe (SmallValue a))
  deriving (Show, Functor)

instance Onion Instruction where
  peel (SmallInstruction a _ _) = a
  peel (LargeInstruction a _ _) = a

-- | The first @a@ parameter represents the span of the whole thing. The second
-- @a@ parameter represents the span of the directive literal (e. g. @.org@).
data RegisterDirective a
  = RegIar a a (Location a)
  | RegAcc a a (MimaWord a)
  | RegRa  a a (Location a)
  | RegSp  a a (Location a)
  | RegFp  a a (Location a)
  deriving (Show, Functor)

instance Onion RegisterDirective where
  peel (RegIar a _ _) = a
  peel (RegAcc a _ _) = a
  peel (RegRa  a _ _) = a
  peel (RegSp  a _ _) = a
  peel (RegFp  a _ _) = a

data JsonValue a = JsonValue a A.Value
  deriving (Show, Functor)

instance Onion JsonValue where
  peel (JsonValue a _) = a

-- | The first @a@ parameter represents the span of the whole thing. The second
-- @a@ parameter represents the span of the directive literal (e. g. @.org@).
data Directive a
  = Reg        a a (RegisterDirective a)
  | Org        a a (Address a)
  | Lit        a a (MimaWord a)
  | Arr        a a [MimaWord a]
  | MetaGlobal a a (Name a) (JsonValue a)
  | MetaStart  a a (Name a) (JsonValue a)
  | MetaStop   a a (Name a)
  | Meta       a a (Name a) (JsonValue a)
  deriving (Show, Functor)

instance Onion Directive where
  peel (Reg        a _ _)   = a
  peel (Org        a _ _)   = a
  peel (Lit        a _ _)   = a
  peel (Arr        a _ _)   = a
  peel (MetaGlobal a _ _ _) = a
  peel (MetaStart  a _ _ _) = a
  peel (MetaStop   a _ _)   = a
  peel (Meta       a _ _ _) = a

-- | A single token. The @s@ type parameter is the type of location annotations.
data AsmToken a
  = TokenLabel (Name a)
  | TokenInstruction (Instruction a)
  | TokenDirective (Directive a)
  | TokenComment a T.Text Bool
  -- ^ @'TokenComment' a text inline@ represents a comment.
  -- @inline@ is true if the comment is on the same line as an instruction or a label.
  deriving (Show, Functor)

instance Onion AsmToken where
  peel (TokenLabel a)       = peel a
  peel (TokenInstruction a) = peel a
  peel (TokenDirective a)   = peel a
  peel (TokenComment a _ _) = a

-- | Representation of an assembly file in phase 1. The @s@ type parameter is
-- the type of location annotations.
type Phase1 s = [AsmToken s]
