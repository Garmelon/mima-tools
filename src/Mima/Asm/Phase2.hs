{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Mima.Asm.Phase2
  ( phaseS1 -- TODO only leave the proper types
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Aeson                as A
import           Data.Foldable
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T
import           Data.Void

import qualified Mima.Asm.Phase1           as P1
import           Mima.Asm.Types
import           Mima.Asm.Weed
import qualified Mima.Vm.Instruction       as Vm
import qualified Mima.Vm.Word              as Vm

data Subphase
  = S1
  -- ^ Freshly converted from 'Phase1'. Arrays are converted into multiple
  -- literal values. Comments are removed.
  | S2
  -- ^ After resolving all .org-s and relative positions and assigning each
  -- token an address.
  | S3
  -- ^ After extracting and removing all labels and .meta-s. This step results
  -- in a map to resolve labels and a complete set of .meta-* metadata.
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
data Location1 s
  = Loc1Absolute s Vm.MimaAddress
  | Loc1Relative s Integer
  | Loc1Label (Name s)
  deriving (Show, Functor)

instance Onion Location1 where
  peel (Loc1Absolute s _) = s
  peel (Loc1Relative s _) = s
  peel (Loc1Label l)      = peel l

-- | A location defined by an absolute address or by a label.
data Location2 s
  = Loc2Absolute s Vm.MimaAddress
  | Loc2Label (Name s)
  deriving (Show, Functor)

instance Onion Location2 where
  peel (Loc2Absolute s _) = s
  peel (Loc2Label l)      = peel l

-- | A type family for locations in various stages of resolution.
type family LocationX (t :: Subphase) (s :: *)
type instance LocationX 'S1 s = Location1 s
type instance LocationX 'S2 s = Location2 s
type instance LocationX 'S3 s = Location2 s
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
  = MetaStart s (Name s) (JsonValue s)
  | MetaStop  s (Name s)
  deriving (Show, Functor)

instance Onion Meta where
  peel (MetaStart s _ _) = s
  peel (MetaStop s _)    = s

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

deriving instance Show s => Show (RegisterDirective 'S1 s)
deriving instance Show s => Show (RegisterDirective 'S2 s)
deriving instance Show s => Show (RegisterDirective 'S3 s)
deriving instance Show s => Show (RegisterDirective 'S4 s)
deriving instance Show s => Show (RegisterDirective 'S5 s)

type family TokenRegX (t :: Subphase) (s :: *)
type instance TokenRegX 'S1 s = RegisterDirective 'S1 s
type instance TokenRegX 'S2 s = RegisterDirective 'S2 s
type instance TokenRegX 'S3 s = RegisterDirective 'S3 s
type instance TokenRegX 'S4 s = RegisterDirective 'S4 s
type instance TokenRegX 'S5 s = Void

data AsmToken (t :: Subphase) (s :: *)
 = TokenOrg   (TokenOrgX t s)
 | TokenLabel (AddressX t s) (TokenLabelX t s)
 | TokenMeta  (AddressX t s) (TokenMetaX t s)
 | TokenLit   (AddressX t s) (MimaWord t s)
 | TokenInstr (AddressX t s) (TokenInstrX t s)
 | TokenReg   (AddressX t s) (TokenRegX t s)

deriving instance Show s => Show (AsmToken 'S1 s)
deriving instance Show s => Show (AsmToken 'S2 s)
deriving instance Show s => Show (AsmToken 'S3 s)
deriving instance Show s => Show (AsmToken 'S4 s)
deriving instance Show s => Show (AsmToken 'S5 s)

type Phase2 t s = [AsmToken t s]

{- Subphase 1 -}

p1ToP2Name :: P1.Name s -> Name s
p1ToP2Name (P1.Name s text) = Name s text

p1ToP2JsonValue :: P1.JsonValue s -> JsonValue s
p1ToP2JsonValue (P1.JsonValue s value) = JsonValue s value

p1ToP2Address :: P1.Address s -> OrgAddress s
p1ToP2Address (P1.AddressAbsolute s addr)   = OrgAddrAbsolute s addr
p1ToP2Address (P1.AddressRelative s offset) = OrgAddrRelative s offset

p1ToP2Location :: P1.Location s -> Location1 s
p1ToP2Location (P1.LocationAddress (P1.AddressAbsolute s addr)) =
  Loc1Absolute s addr
p1ToP2Location (P1.LocationAddress (P1.AddressRelative s offset)) =
  Loc1Relative s offset
p1ToP2Location (P1.LocationLabel name) = Loc1Label $ p1ToP2Name name

p1ToP2Instruction :: P1.Instruction s -> Instruction 'S1 s
p1ToP2Instruction (P1.SmallInstruction _ (P1.SmallOpcode _ so) loc) =
  SmallInstruction so $ p1ToP2Location loc
p1ToP2Instruction (P1.LargeInstruction _ (P1.LargeOpcode _ lo) maybeSv) =
  LargeInstruction lo $ fmap (\(P1.SmallValue _ sv) -> sv) maybeSv

p1ToP2Word :: P1.MimaWord s -> MimaWord 'S1 s
p1ToP2Word (P1.WordRaw _ w)      = WordRaw w
p1ToP2Word (P1.WordLocation loc) = WordLocation $ p1ToP2Location loc

p1ToP2RegDir :: P1.RegisterDirective s -> RegisterDirective 'S1 s
p1ToP2RegDir (P1.RegIar s _ loc)  = RegIar s $ p1ToP2Location loc
p1ToP2RegDir (P1.RegAcc s _ word) = RegAcc s $ p1ToP2Word word
p1ToP2RegDir (P1.RegRa  s _ loc)  = RegRa  s $ p1ToP2Location loc
p1ToP2RegDir (P1.RegSp  s _ loc)  = RegSp  s $ p1ToP2Location loc
p1ToP2RegDir (P1.RegFp  s _ loc)  = RegFp  s $ p1ToP2Location loc

data MetaS1 s = MetaS1 s s (P1.Name s) (P1.JsonValue s)
  deriving (Show)

instance Onion MetaS1 where
  peel (MetaS1 s _ _ _) = s

data StateS1 s = StateS1
  { s1Metas  :: Map.Map T.Text (MetaS1 s)
  , s1Tokens :: [AsmToken 'S1 s]
  } deriving (Show)

type WeedS1 s = StateT (StateS1 s) (Weed (WeedError s))

s1AddMeta :: s -> s -> P1.Name s -> P1.JsonValue s -> WeedS1 s ()
s1AddMeta s1 s2 name@(P1.Name namePos nameText) value = do
  s <- get
  when (nameText `Map.member` s1Metas s) $
    lift $ harmless $ errorWith namePos undefined
  let meta = MetaS1 s1 s2 name value
  put s{s1Metas = Map.insert nameText meta $ s1Metas s}

s1TakeMetas :: WeedS1 s [MetaS1 s]
s1TakeMetas = do
  s <- get
  put s{s1Metas = Map.empty}
  pure $ Map.elems $ s1Metas s

s1WithMetas :: WeedS1 s () -> WeedS1 s ()
s1WithMetas f = do
  metas <- s1TakeMetas
  for_ (reverse metas) $ \(MetaS1 s _ name value) ->
    s1AddToken $ TokenMeta () $
    MetaStart s (p1ToP2Name name) (p1ToP2JsonValue value)
  f
  for_ metas $ \(MetaS1 s _ name _) ->
    s1AddToken $ TokenMeta () $
    MetaStop s (p1ToP2Name name)

s1AddToken :: AsmToken 'S1 s -> WeedS1 s ()
s1AddToken t = modify $ \s -> s{s1Tokens = t : s1Tokens s}

s1AddP1Token :: P1.AsmToken s -> WeedS1 s ()
s1AddP1Token (P1.TokenLabel name) =
  s1AddToken $ TokenLabel () $ p1ToP2Name name
s1AddP1Token (P1.TokenInstruction instr) =
  s1AddToken $ TokenInstr () $ p1ToP2Instruction instr
s1AddP1Token (P1.TokenDirective (P1.Reg _ _ regDir)) =
  s1AddToken $ TokenReg () $ p1ToP2RegDir regDir
s1AddP1Token (P1.TokenDirective (P1.Org _ _ addr)) =
  s1AddToken $ TokenOrg $ p1ToP2Address addr
s1AddP1Token (P1.TokenDirective (P1.Lit _ _ w)) =
  s1WithMetas $ s1AddToken $ TokenLit () $ p1ToP2Word w
s1AddP1Token (P1.TokenDirective (P1.Arr _ _ ws)) =
  s1WithMetas $ for_ ws $ s1AddToken . TokenLit () . p1ToP2Word
s1AddP1Token (P1.TokenDirective (P1.Meta s1 s2 name value)) =
  s1AddMeta s1 s2 name value
s1AddP1Token (P1.TokenDirective (P1.MetaStart s _ name value)) =
  s1AddToken $ TokenMeta () $
  MetaStart s (p1ToP2Name name) (p1ToP2JsonValue value)
s1AddP1Token (P1.TokenDirective (P1.MetaStop s _ name)) =
  s1AddToken $ TokenMeta () $ MetaStop s (p1ToP2Name name)
s1AddP1Token P1.TokenComment{} = pure ()

phaseS1 :: P1.Phase1 s -> Weed (WeedError s) (Phase2 'S1 s)
phaseS1 ts = do
  let initialS = StateS1 Map.empty []
  s <- execStateT (traverse_ s1AddP1Token ts) initialS
  for_ (Map.elems $ s1Metas s) $ \m ->
    harmless $ errorWith (peel m) "unconsumed .meta"
  pure $ s1Tokens s
