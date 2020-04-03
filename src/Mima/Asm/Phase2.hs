{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Mima.Asm.Phase2
  ( phaseS1
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Foldable
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T
import           Data.Void

import qualified Mima.Asm.Phase1           as P1
import           Mima.Asm.Weed
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

data LocationNoRel a
  = LocationNoRelAddress a Vm.MimaAddress
  | LocationNoRelLabel (P1.Name a)
  deriving (Show)

type family LocationX (t :: Subphase) (s :: *)
type instance LocationX 'S1 s = P1.Location s
type instance LocationX 'S2 s = LocationNoRel s
type instance LocationX 'S3 s = LocationNoRel s
type instance LocationX 'S4 s = Vm.MimaAddress
type instance LocationX 'S5 s = Vm.MimaAddress

type family AddressX (t :: Subphase) (s :: *)
type instance AddressX 'S1 s = ()
type instance AddressX 'S2 s = Vm.MimaAddress
type instance AddressX 'S3 s = Vm.MimaAddress
type instance AddressX 'S4 s = Vm.MimaAddress
type instance AddressX 'S5 s = Vm.MimaAddress

type family TokenOrgX (t :: Subphase) (s :: *)
type instance TokenOrgX 'S1 s = P1.Address s
type instance TokenOrgX 'S2 s = Void
type instance TokenOrgX 'S3 s = Void
type instance TokenOrgX 'S4 s = Void
type instance TokenOrgX 'S5 s = Void

type family TokenLabelX (t :: Subphase) (s :: *)
type instance TokenLabelX 'S1 s = P1.Name s
type instance TokenLabelX 'S2 s = P1.Name s
type instance TokenLabelX 'S3 s = Void
type instance TokenLabelX 'S4 s = Void
type instance TokenLabelX 'S5 s = Void

data Meta s
  = MetaStart (P1.Name s) (P1.JsonValue s)
  | MetaStop  (P1.Name s)
  deriving (Show)

type family TokenMetaX (t :: Subphase) (s :: *)
type instance TokenMetaX 'S1 s = Meta s
type instance TokenMetaX 'S2 s = Meta s
type instance TokenMetaX 'S3 s = Void
type instance TokenMetaX 'S4 s = Void
type instance TokenMetaX 'S5 s = Void

data MimaWord (t :: Subphase) (s :: *)
  = WordRaw s Vm.MimaWord
  | WordLocation (LocationX t s)

data Instruction (t :: Subphase) (s :: *)
  = SmallInstruction (P1.SmallOpcode s) (LocationX t s)
  | LargeInstruction (P1.LargeOpcode s) (Maybe (P1.SmallValue s))

type family TokenInstrX (t :: Subphase) (s :: *)
type instance TokenInstrX 'S1 s = Instruction 'S1 s
type instance TokenInstrX 'S2 s = Instruction 'S2 s
type instance TokenInstrX 'S3 s = Instruction 'S3 s
type instance TokenInstrX 'S4 s = Instruction 'S4 s
type instance TokenInstrX 'S5 s = Void

data RegisterDirective (t :: Subphase) (s :: *)
  = RegIar s s (LocationX t s)
  | RegAcc s s (MimaWord t s)
  | RegRa  s s (LocationX t s)
  | RegSp  s s (LocationX t s)
  | RegFp  s s (LocationX t s)

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

type Phase2 t s = [AsmToken t s]

{- Subphase 1 -}

data MetaS1 s = MetaS1 s s (P1.Name s) (P1.JsonValue s)
  deriving (Show)

instance P1.Onion MetaS1 where
  peel (MetaS1 s _ _ _) = s

data StateS1 s = StateS1
  { s1Metas  :: Map.Map T.Text (MetaS1 s)
  , s1Tokens :: [AsmToken 'S1 s]
  }

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
  for_ (reverse metas) $ \(MetaS1 _ _ name value) ->
    s1AddToken $ TokenMeta () $ MetaStart name value
  f
  for_ metas $ \(MetaS1 _ _ name _) ->
    s1AddToken $ TokenMeta () $ MetaStop name

s1AddToken :: AsmToken 'S1 s -> WeedS1 s ()
s1AddToken t = modify $ \s -> s{s1Tokens = t : s1Tokens s}

p1InstrToP2Instr :: P1.Instruction s -> Instruction 'S1 s
p1InstrToP2Instr (P1.SmallInstruction _ so loc) = SmallInstruction so loc
p1InstrToP2Instr (P1.LargeInstruction _ lo sv)  = LargeInstruction lo sv

p1WordToP2Word :: P1.MimaWord s -> MimaWord 'S1 s
p1WordToP2Word (P1.WordRaw s w)      = WordRaw s w
p1WordToP2Word (P1.WordLocation loc) = WordLocation loc

p1RegDirToP2RegDir :: P1.RegisterDirective s -> RegisterDirective 'S1 s
p1RegDirToP2RegDir (P1.RegIar s1 s2 loc)  = RegIar s1 s2 loc
p1RegDirToP2RegDir (P1.RegAcc s1 s2 word) = RegAcc s1 s2 $ p1WordToP2Word word
p1RegDirToP2RegDir (P1.RegRa s1 s2 loc)   = RegRa s1 s2 loc
p1RegDirToP2RegDir (P1.RegSp s1 s2 loc)   = RegSp s1 s2 loc
p1RegDirToP2RegDir (P1.RegFp s1 s2 loc)   = RegFp s1 s2 loc

s1AddP1Token :: P1.AsmToken s -> WeedS1 s ()
s1AddP1Token (P1.TokenLabel name) = s1AddToken $ TokenLabel () name
s1AddP1Token (P1.TokenInstruction instr) =
  s1AddToken $ TokenInstr () $ p1InstrToP2Instr instr
s1AddP1Token (P1.TokenDirective (P1.Reg _ _ regDir)) =
  s1AddToken $ TokenReg () $ p1RegDirToP2RegDir regDir
s1AddP1Token (P1.TokenDirective (P1.Org _ _ addr)) =
  s1AddToken $ TokenOrg addr
s1AddP1Token (P1.TokenDirective (P1.Lit _ _ w)) =
  s1WithMetas $ s1AddToken $ TokenLit () $ p1WordToP2Word w
s1AddP1Token (P1.TokenDirective (P1.Arr _ _ ws)) =
  s1WithMetas $ for_ ws $ s1AddToken . TokenLit () . p1WordToP2Word
s1AddP1Token (P1.TokenDirective (P1.Meta s1 s2 name value)) =
  s1AddMeta s1 s2 name value
s1AddP1Token (P1.TokenDirective (P1.MetaStart _ _ name value)) =
  s1AddToken $ TokenMeta () $ MetaStart name value
s1AddP1Token (P1.TokenDirective (P1.MetaStop _ _ name)) =
  s1AddToken $ TokenMeta () $ MetaStop name
s1AddP1Token P1.TokenComment{} = pure ()

phaseS1 :: P1.Phase1 s -> Weed (WeedError s) (Phase2 'S1 s)
phaseS1 ts = do
  let initialS = StateS1 Map.empty []
  s <- execStateT (traverse_ s1AddP1Token ts) initialS
  for_ (Map.elems $ s1Metas s) $ \m ->
    harmless $ errorWith (P1.peel m) "unconsumed .meta"
  pure $ s1Tokens s
