{-# LANGUAGE OverloadedStrings #-}

module Mima.Asm.Phase1.Format
  ( formatPhase1
  ) where

import qualified Data.Aeson.Text       as A
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

import           Mima.Asm.Phase1.Types
import           Mima.Format

formatName :: Name a -> T.Text
formatName (Name _ text) = text

formatAddress :: Address a -> T.Text
formatAddress (AddressAbsolute _ addr) = toDec addr
formatAddress (AddressRelative _ rel)
  | rel >= 0 = T.pack $ "+" ++ show rel
  | otherwise = T.pack $ show rel

formatLocation :: Location a -> T.Text
formatLocation (LocationAddress addr) = formatAddress addr
formatLocation (LocationLabel l)      = formatName l
formatLocation (LocationLabelRel _ l _ offset)
  = formatName l <> "[" <> toDec offset <> "]"

formatSmallOpcode :: SmallOpcode a -> T.Text
formatSmallOpcode (SmallOpcode _ opcode) = T.pack $ show opcode

formatLargeOpcode :: LargeOpcode a -> T.Text
formatLargeOpcode (LargeOpcode _ opcode) = T.pack $ show opcode

formatMimaWord :: MimaWord a -> T.Text
formatMimaWord (WordRaw _ word)   = toDec word
formatMimaWord (WordLocation loc) = formatLocation loc

formatSmallValue :: SmallValue a -> T.Text
formatSmallValue (SmallValue _ val) = toDec val

formatInstruction :: Instruction a -> T.Text
formatInstruction (SmallInstruction _ opcode loc) =
  formatSmallOpcode opcode <> " " <> formatLocation loc
formatInstruction (LargeInstruction _ opcode Nothing) =
  formatLargeOpcode opcode
formatInstruction (LargeInstruction _ opcode (Just val)) =
  formatLargeOpcode opcode <> " " <> formatSmallValue val

formatRegisterDirective :: RegisterDirective a -> T.Text
formatRegisterDirective (RegIar _ _ loc)  = "IAR " <> formatLocation loc
formatRegisterDirective (RegAcc _ _ word) = "ACC " <> formatMimaWord word
formatRegisterDirective (RegRa  _ _ loc)  = "RA "  <> formatLocation loc
formatRegisterDirective (RegSp  _ _ loc)  = "SP "  <> formatLocation loc
formatRegisterDirective (RegFp  _ _ loc)  = "FP "  <> formatLocation loc

formatJsonValue :: JsonValue a -> T.Text
formatJsonValue (JsonValue _ val) = TL.toStrict $ A.encodeToLazyText val

formatDirective :: Directive a -> T.Text
formatDirective (Reg _ _ regDir) = ".reg " <> formatRegisterDirective regDir
formatDirective (Org _ _ addr)   = ".org " <> formatAddress addr
formatDirective (Lit _ _ val)    = ".lit " <> formatMimaWord val
formatDirective (Arr _ _ vals) =
  ".arr [" <> T.intercalate ", " (map formatMimaWord vals) <> "]"
formatDirective (MetaGlobal _ _ n val) =
  ".meta-global " <> formatName n <> " " <> formatJsonValue val
formatDirective (MetaStart _ _ n val) =
  ".meta-start " <> formatName n <> " " <> formatJsonValue val
formatDirective (MetaStop _ _ n) = ".meta-stop " <> formatName n
formatDirective (Meta _ _ n val) =
  ".meta " <> formatName n <> " " <> formatJsonValue val

formatToken :: AsmToken a -> T.Text
formatToken (TokenLabel n)          = formatName n <> ":"
formatToken (TokenInstruction ins)  = "  " <> formatInstruction ins
formatToken (TokenDirective dir)    = formatDirective dir
formatToken (TokenComment _ text _) = ";" <> text

formatPhase1 :: Phase1 a -> T.Text
formatPhase1 (x:y@(TokenComment _ _ True):xs) = formatToken x <> " " <> formatPhase1 (y:xs)
formatPhase1 (x:xs) = formatToken x <> "\n" <> formatPhase1 xs
formatPhase1 [] = ""
