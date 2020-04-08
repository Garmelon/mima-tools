{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Mima.Asm.Phase2.Types
  ( Phase2
  , Subphase(..)
  -- * Utility types
  , Name(..)
  , AddressX
  -- * Locations
  , Location(..)
  , LocationX
  -- * Tokens
  , AsmToken(..)
  -- ** Org token
  , OrgAddress(..)
  , TokenOrgX
  -- ** Label token
  , TokenLabelX
  -- ** Meta token
  , JsonValue(..)
  , Meta(..)
  , TokenMetaX
  -- ** Instruction token
  , MimaWord(..)
  , idWord
  , Instruction(..)
  , idInstruction
  , TokenInstrX
  -- ** Register token
  , RegisterDirective(..)
  , idRegDir
  , TokenRegX
  ) where

import qualified Data.Aeson          as A
import qualified Data.Text           as T
import           Data.Void
import           Mima.Asm.Types
import qualified Mima.Vm.Instruction as Vm
import qualified Mima.Vm.Word        as Vm

data Subphase
  = S1
  -- ^ Freshly converted from 'Phase1'. Arrays are converted into multiple
  -- literal values. Comments are removed.
  | S2
  -- ^ After resolving all @.org@s and relative positions and assigning each
  -- token an address.
  | S3
  -- ^ After extracting and removing all labels and @.meta@s. This step results
  -- in a map to resolve labels and a complete set of @.meta-*@ metadata.
  | S4
  -- ^ After resolving all labels. Instructions are converted into literal
  -- values.
  | S5
  -- ^ After extracting all initial register values.

-- | The name of a label or a meta tag.
data Name s = Name s T.Text
  deriving (Show, Functor)

instance Onion Name where
  peel (Name s _) = s

-- | A location defined by an absolute or relative address or by a label.
data Location s
  = LocAbsolute s Vm.MimaAddress
  | LocRelative s Integer
  | LocLabel (Name s)
  | LocLabelRel s (Name s) s Integer
  deriving (Show, Functor)

instance Onion Location where
  peel (LocAbsolute s _)     = s
  peel (LocRelative s _)     = s
  peel (LocLabel l)          = peel l
  peel (LocLabelRel s _ _ _) = s

-- | A type family for locations in various stages of resolution.
type family LocationX (t :: Subphase) (s :: *)
type instance LocationX 'S1 s = Location s
type instance LocationX 'S2 s = Location s
type instance LocationX 'S3 s = Location s
type instance LocationX 'S4 s = Vm.MimaAddress
type instance LocationX 'S5 s = Vm.MimaAddress

-- | A type family for addresses of various tokens.
type family AddressX (t :: Subphase) (s :: *)
type instance AddressX 'S1 s = ()
type instance AddressX 'S2 s = Vm.MimaAddress
type instance AddressX 'S3 s = Vm.MimaAddress
type instance AddressX 'S4 s = Vm.MimaAddress
type instance AddressX 'S5 s = Vm.MimaAddress

-- | A representation for .org addresses.
data OrgAddress s
 = OrgAddrAbsolute s Vm.MimaAddress
 | OrgAddrRelative s Integer
 deriving (Show, Functor)

instance Onion OrgAddress where
  peel (OrgAddrAbsolute s _) = s
  peel (OrgAddrRelative s _) = s

type family TokenOrgX (t :: Subphase) (s :: *)
type instance TokenOrgX 'S1 s = OrgAddress s
type instance TokenOrgX 'S2 s = Void
type instance TokenOrgX 'S3 s = Void
type instance TokenOrgX 'S4 s = Void
type instance TokenOrgX 'S5 s = Void

type family TokenLabelX (t :: Subphase) (s :: *)
type instance TokenLabelX 'S1 s = Name s
type instance TokenLabelX 'S2 s = Name s
type instance TokenLabelX 'S3 s = Void
type instance TokenLabelX 'S4 s = Void
type instance TokenLabelX 'S5 s = Void

-- | A wrapper that annotates a 'A.Value' with an @s@ value.
data JsonValue s = JsonValue s A.Value
  deriving (Show, Functor)

instance Onion JsonValue where
  peel (JsonValue s _) = s

-- | A representation for .meta-start and .meta-stop directives.
data Meta s
  = Meta       s (Name s) (JsonValue s)
  | MetaStart  s (Name s) (JsonValue s)
  | MetaStop   s (Name s)
  | MetaGlobal s (Name s) (JsonValue s)
  deriving (Show, Functor)

instance Onion Meta where
  peel (Meta       s _ _) = s
  peel (MetaStart  s _ _) = s
  peel (MetaStop   s _)   = s
  peel (MetaGlobal s _ _) = s

type family TokenMetaX (t :: Subphase) (s :: *)
type instance TokenMetaX 'S1 s = Meta s
type instance TokenMetaX 'S2 s = Meta s
type instance TokenMetaX 'S3 s = Void
type instance TokenMetaX 'S4 s = Void
type instance TokenMetaX 'S5 s = Void

-- | A stripped-down representation of Mima words that does not have an 'Onion'
-- instance because none is required.
data MimaWord (t :: Subphase) (s :: *)
  = WordRaw Vm.MimaWord
  | WordLocation (LocationX t s)

idWord :: (LocationX a s ~ LocationX b s) => MimaWord a s -> MimaWord b s
idWord (WordRaw word)     = WordRaw word
idWord (WordLocation loc) = WordLocation loc

deriving instance Show s => Show (MimaWord 'S1 s)
deriving instance Show s => Show (MimaWord 'S2 s)
deriving instance Show s => Show (MimaWord 'S3 s)
deriving instance Show s => Show (MimaWord 'S4 s)
deriving instance Show s => Show (MimaWord 'S5 s)

-- | A stripped-down representation of Mima instructions that does not have an
-- 'Onion' instance because none is required.
data Instruction (t :: Subphase) (s :: *)
  = SmallInstruction Vm.SmallOpcode (LocationX t s)
  | LargeInstruction Vm.LargeOpcode (Maybe Vm.SmallValue)

idInstruction :: (LocationX a s ~ LocationX b s) => Instruction a s -> Instruction b s
idInstruction (SmallInstruction so loc) = SmallInstruction so loc
idInstruction (LargeInstruction lo sv)  = LargeInstruction lo sv

deriving instance Show s => Show (Instruction 'S1 s)
deriving instance Show s => Show (Instruction 'S2 s)
deriving instance Show s => Show (Instruction 'S3 s)
deriving instance Show s => Show (Instruction 'S4 s)
deriving instance Show s => Show (Instruction 'S5 s)

type family TokenInstrX (t :: Subphase) (s :: *)
type instance TokenInstrX 'S1 s = Instruction 'S1 s
type instance TokenInstrX 'S2 s = Instruction 'S2 s
type instance TokenInstrX 'S3 s = Instruction 'S3 s
type instance TokenInstrX 'S4 s = Void
type instance TokenInstrX 'S5 s = Void

data RegisterDirective (t :: Subphase) (s :: *)
  = RegIar s (LocationX t s)
  | RegAcc s (MimaWord t s)
  | RegRa  s (LocationX t s)
  | RegSp  s (LocationX t s)
  | RegFp  s (LocationX t s)

idRegDir :: (LocationX a s ~ LocationX b s) => RegisterDirective a s -> RegisterDirective b s
idRegDir (RegIar s loc)  = RegIar s loc
idRegDir (RegAcc s word) = RegAcc s $ idWord word
idRegDir (RegRa s loc)   = RegRa s loc
idRegDir (RegSp s loc)   = RegSp s loc
idRegDir (RegFp s loc)   = RegFp s loc

deriving instance Show s => Show (RegisterDirective 'S1 s)
deriving instance Show s => Show (RegisterDirective 'S2 s)
deriving instance Show s => Show (RegisterDirective 'S3 s)
deriving instance Show s => Show (RegisterDirective 'S4 s)
deriving instance Show s => Show (RegisterDirective 'S5 s)

instance Onion (RegisterDirective t) where
  peel (RegIar s _) = s
  peel (RegAcc s _) = s
  peel (RegRa  s _) = s
  peel (RegSp  s _) = s
  peel (RegFp  s _) = s

type family TokenRegX (t :: Subphase) (s :: *)
type instance TokenRegX 'S1 s = RegisterDirective 'S1 s
type instance TokenRegX 'S2 s = RegisterDirective 'S2 s
type instance TokenRegX 'S3 s = RegisterDirective 'S3 s
type instance TokenRegX 'S4 s = RegisterDirective 'S4 s
type instance TokenRegX 'S5 s = Void

data AsmToken (t :: Subphase) (s :: *)
 = TokenOrg   s (TokenOrgX t s)
 | TokenLabel s (AddressX t s) (TokenLabelX t s)
 | TokenMeta  s (AddressX t s) (TokenMetaX t s)
 | TokenLit   s (AddressX t s) (MimaWord t s)
 | TokenInstr s (AddressX t s) (TokenInstrX t s)
 | TokenReg   s (AddressX t s) (TokenRegX t s)

deriving instance Show s => Show (AsmToken 'S1 s)
deriving instance Show s => Show (AsmToken 'S2 s)
deriving instance Show s => Show (AsmToken 'S3 s)
deriving instance Show s => Show (AsmToken 'S4 s)
deriving instance Show s => Show (AsmToken 'S5 s)

instance Onion (AsmToken t) where
  peel (TokenOrg   s _)   = s
  peel (TokenLabel s _ _) = s
  peel (TokenMeta  s _ _) = s
  peel (TokenLit   s _ _) = s
  peel (TokenInstr s _ _) = s
  peel (TokenReg   s _ _) = s

type Phase2 t s = [AsmToken t s]
