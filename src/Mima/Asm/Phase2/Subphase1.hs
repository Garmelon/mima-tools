{-# LANGUAGE DataKinds #-}

module Mima.Asm.Phase2.Subphase1
  ( subphase1
  ) where

import           Data.Traversable

import qualified Mima.Asm.Phase1       as P1
import           Mima.Asm.Phase2.Types
import           Mima.Asm.Phase2.Util
import           Mima.Asm.Types
import           Mima.Asm.Weed

type WeedS1 s = Weed (WeedError s)

p1ToP2Name :: P1.Name s -> Name s
p1ToP2Name (P1.Name s text) = Name s text

p1ToP2JsonValue :: P1.JsonValue s -> JsonValue s
p1ToP2JsonValue (P1.JsonValue s value) = JsonValue s value

p1ToP2Address :: P1.Address s -> WeedS1 s (OrgAddress s)
p1ToP2Address (P1.AddressAbsolute s addr) =
  OrgAddrAbsolute s <$> intToBounded s addr
p1ToP2Address (P1.AddressRelative s offset) = pure $ OrgAddrRelative s offset

p1ToP2Location :: P1.Location s -> WeedS1 s (Location s)
p1ToP2Location (P1.LocationAddress (P1.AddressAbsolute s addr)) =
  LocAbsolute s <$> intToBounded s addr
p1ToP2Location (P1.LocationAddress (P1.AddressRelative s offset)) =
  pure $ LocRelative s offset
p1ToP2Location (P1.LocationLabel name) = pure $ LocLabel $ p1ToP2Name name
p1ToP2Location (P1.LocationLabelRel s name s' offset) =
  pure $ LocLabelRel s (p1ToP2Name name) s' offset

p1ToP2Instruction :: P1.Instruction s -> WeedS1 s (Instruction 'S1 s)
p1ToP2Instruction (P1.SmallInstruction _ (P1.SmallOpcode _ so) loc) =
  SmallInstruction so <$> p1ToP2Location loc
p1ToP2Instruction (P1.LargeInstruction _ (P1.LargeOpcode _ lo) maybeSv) = do
  val <- for maybeSv $ \(P1.SmallValue s v) -> intToBounded s v
  pure $ LargeInstruction lo val

p1ToP2Word :: P1.MimaWord s -> WeedS1 s (MimaWord 'S1 s)
p1ToP2Word (P1.WordRaw s w)      = WordRaw <$> intToBounded s w
p1ToP2Word (P1.WordLocation loc) = WordLocation <$> p1ToP2Location loc

p1ToP2RegDir :: P1.RegisterDirective s -> WeedS1 s (RegisterDirective 'S1 s)
p1ToP2RegDir (P1.RegIar s _ loc)  = RegIar s <$> p1ToP2Location loc
p1ToP2RegDir (P1.RegAcc s _ word) = RegAcc s <$> p1ToP2Word word
p1ToP2RegDir (P1.RegRa  s _ loc)  = RegRa  s <$> p1ToP2Location loc
p1ToP2RegDir (P1.RegSp  s _ loc)  = RegSp  s <$> p1ToP2Location loc
p1ToP2RegDir (P1.RegFp  s _ loc)  = RegFp  s <$> p1ToP2Location loc

p1ToP2Directive :: P1.Directive s -> WeedS1 s [AsmToken 'S1 s]
p1ToP2Directive (P1.Reg s _ regDir) = do
  r <- p1ToP2RegDir regDir
  pure [TokenReg s () r]
p1ToP2Directive (P1.Org s _ addr) = do
  a <- p1ToP2Address addr
  pure [TokenOrg s a]
p1ToP2Directive (P1.Lit s _ word) = do
  w <- p1ToP2Word word
  pure [TokenLit s () w]
p1ToP2Directive (P1.Arr s _ ws) = for ws $ \word -> do
  w <- p1ToP2Word word
  pure $ TokenLit s () w
p1ToP2Directive (P1.MetaGlobal s _ name value) =
  pure [TokenMeta s () $ MetaGlobal s (p1ToP2Name name) (p1ToP2JsonValue value)]
p1ToP2Directive (P1.MetaStart s _ name value) =
  pure [TokenMeta s () $ MetaStart s (p1ToP2Name name) (p1ToP2JsonValue value)]
p1ToP2Directive (P1.MetaStop s _ name) =
  pure [TokenMeta s () $ MetaStop s (p1ToP2Name name)]
p1ToP2Directive (P1.Meta s _ name value) =
  pure [TokenMeta s () $ Meta s (p1ToP2Name name) (p1ToP2JsonValue value)]

p1ToP2Token :: P1.AsmToken s -> WeedS1 s [AsmToken 'S1 s]
p1ToP2Token (P1.TokenLabel name) =
  pure [TokenLabel (peel name) () $ p1ToP2Name name]
p1ToP2Token (P1.TokenInstruction instr) = do
  i <- p1ToP2Instruction instr
  pure [TokenInstr (peel instr) () i]
p1ToP2Token (P1.TokenDirective dir) = p1ToP2Directive dir
p1ToP2Token P1.TokenComment{} = pure []

subphase1 :: P1.Phase1 s -> Weed (WeedError s) (Phase2 'S1 s)
subphase1 ts = concat <$> traverse p1ToP2Token ts
