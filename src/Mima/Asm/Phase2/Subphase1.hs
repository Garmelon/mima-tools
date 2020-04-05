{-# LANGUAGE DataKinds           #-}

module Mima.Asm.Phase2.Subphase1
  ( subphase1
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Foldable
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T

import qualified Mima.Asm.Phase1           as P1
import           Mima.Asm.Phase2.Types
import           Mima.Asm.Phase2.Util
import           Mima.Asm.Types
import           Mima.Asm.Weed

{- Converting phase 1 types to phase 2 types -}

p1ToP2Name :: P1.Name s -> Name s
p1ToP2Name (P1.Name s text) = Name s text

p1ToP2JsonValue :: P1.JsonValue s -> JsonValue s
p1ToP2JsonValue (P1.JsonValue s value) = JsonValue s value

p1ToP2Address :: P1.Address s -> WeedS1 s (OrgAddress s)
p1ToP2Address (P1.AddressAbsolute s addr)   = lift $ OrgAddrAbsolute s <$> intToBounded s addr
p1ToP2Address (P1.AddressRelative s offset) = pure $ OrgAddrRelative s offset

p1ToP2Location :: P1.Location s -> WeedS1 s (Location1 s)
p1ToP2Location (P1.LocationAddress (P1.AddressAbsolute s addr)) =
  lift $ Loc1Absolute s <$> intToBounded s addr
p1ToP2Location (P1.LocationAddress (P1.AddressRelative s offset)) =
  pure $ Loc1Relative s offset
p1ToP2Location (P1.LocationLabel name) = pure $ Loc1Label $ p1ToP2Name name
p1ToP2Location (P1.LocationLabelRel s name s' offset) =
  pure $ Loc1LabelRel s (p1ToP2Name name) s' offset

p1ToP2Instruction :: P1.Instruction s -> WeedS1 s (Instruction 'S1 s)
p1ToP2Instruction (P1.SmallInstruction _ (P1.SmallOpcode _ so) loc) =
  SmallInstruction so <$> p1ToP2Location loc
p1ToP2Instruction (P1.LargeInstruction _ (P1.LargeOpcode _ lo) maybeSv) = do
  val <- case maybeSv of
    Nothing                  -> pure Nothing
    Just (P1.SmallValue s v) -> lift $ Just <$> intToBounded s v
  pure $ LargeInstruction lo val

p1ToP2Word :: P1.MimaWord s -> WeedS1 s (MimaWord 'S1 s)
p1ToP2Word (P1.WordRaw s w)      = lift $ WordRaw <$> intToBounded s w
p1ToP2Word (P1.WordLocation loc) = WordLocation <$> p1ToP2Location loc

p1ToP2RegDir :: P1.RegisterDirective s -> WeedS1 s (RegisterDirective 'S1 s)
p1ToP2RegDir (P1.RegIar s _ loc)  = RegIar s <$> p1ToP2Location loc
p1ToP2RegDir (P1.RegAcc s _ word) = RegAcc s <$> p1ToP2Word word
p1ToP2RegDir (P1.RegRa  s _ loc)  = RegRa  s <$> p1ToP2Location loc
p1ToP2RegDir (P1.RegSp  s _ loc)  = RegSp  s <$> p1ToP2Location loc
p1ToP2RegDir (P1.RegFp  s _ loc)  = RegFp  s <$> p1ToP2Location loc

{- Subphase 1 -}

data SingleMeta s = SingleMeta s (P1.Name s) (P1.JsonValue s)
  deriving (Show)

instance Onion SingleMeta where
  peel (SingleMeta s _ _) = s

data StateS1 s = StateS1
  { s1Metas  :: Map.Map T.Text (SingleMeta s)
  , s1Tokens :: [AsmToken 'S1 s]
  } deriving (Show)

type WeedS1 s = StateT (StateS1 s) (Weed (WeedError s))

addMeta :: s -> P1.Name s -> P1.JsonValue s -> WeedS1 s ()
addMeta s name@(P1.Name namePos nameText) value = do
  s1 <- get
  when (nameText `Map.member` s1Metas s1) $
    lift $ harmless $ errorWith namePos "duplicate .meta names"
  let meta = SingleMeta s name value
  put s1{s1Metas = Map.insert nameText meta $ s1Metas s1}

takeMetas :: WeedS1 s [SingleMeta s]
takeMetas = do
  s <- get
  put s{s1Metas = Map.empty}
  pure $ Map.elems $ s1Metas s

withMetas :: WeedS1 s () -> WeedS1 s ()
withMetas f = do
  metas <- takeMetas
  for_ (reverse metas) $ \(SingleMeta s name value) ->
    addToken $ TokenMeta s () $
    MetaStart s (p1ToP2Name name) (p1ToP2JsonValue value)
  f
  for_ metas $ \(SingleMeta s name _) ->
    addToken $ TokenMeta s () $ MetaStop s (p1ToP2Name name)

addToken :: AsmToken 'S1 s -> WeedS1 s ()
addToken t = modify $ \s -> s{s1Tokens = t : s1Tokens s}

addP1Token :: P1.AsmToken s -> WeedS1 s ()
addP1Token (P1.TokenLabel name) =
  addToken $ TokenLabel (peel name) () $ p1ToP2Name name
addP1Token (P1.TokenInstruction instr) = do
  i <- p1ToP2Instruction instr
  withMetas $ addToken $ TokenInstr (peel instr) () i
addP1Token (P1.TokenDirective (P1.Reg s _ regDir)) = do
  r <- p1ToP2RegDir regDir
  addToken $ TokenReg s () r
addP1Token (P1.TokenDirective (P1.Org s _ addr)) = do
  withMetas $ pure ()
  a <- p1ToP2Address addr
  addToken $ TokenOrg s a
addP1Token (P1.TokenDirective (P1.Lit s _ word)) = do
  w <- p1ToP2Word word
  withMetas $ addToken $ TokenLit s () w
addP1Token (P1.TokenDirective (P1.Arr s _ ws)) =
  withMetas $ for_ ws $ \word -> do
    w <- p1ToP2Word word
    addToken $ TokenLit s () w
addP1Token (P1.TokenDirective (P1.Meta s _ name value)) =
  addMeta s name value
addP1Token (P1.TokenDirective (P1.MetaStart s _ name value)) =
  addToken $ TokenMeta s () $
  MetaStart s (p1ToP2Name name) (p1ToP2JsonValue value)
addP1Token (P1.TokenDirective (P1.MetaStop s _ name)) =
  addToken $ TokenMeta s () $ MetaStop s (p1ToP2Name name)
addP1Token P1.TokenComment{} = pure ()

subphase1 :: P1.Phase1 s -> Weed (WeedError s) (Phase2 'S1 s)
subphase1 ts = do
  let initialS = StateS1 Map.empty []
  s <- flip execStateT initialS $ do
    traverse_ addP1Token ts
    withMetas $ pure ()
  pure $ reverse $ s1Tokens s
