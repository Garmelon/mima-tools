{-# LANGUAGE OverloadedStrings #-}

module Mima.Assembler.Parser.RawInstruction
  ( RawInstruction
  , rawInstruction
  ) where

import           Data.Bits
import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C

import           Mima.Assembler.Parser.Basic
import           Mima.Assembler.Parser.Label
import           Mima.Word

data RawInstruction addr
  = RawLIT  MimaWord
  | RawLDC  addr
  | RawLDV  addr
  | RawSTV  addr
  | RawADD  addr
  | RawAND  addr
  | RawOR   addr
  | RawXOR  addr
  | RawEQL  addr
  | RawJMP  addr
  | RawJMN  addr
  | RawLDIV addr
  | RawSTIV addr
  | RawCALL addr
  | RawLDVR addr
  | RawSTVR addr
  | RawHALT SmallValue
  | RawNOT  SmallValue
  | RawRAR  SmallValue
  | RawRET  SmallValue
  | RawLDRA SmallValue
  | RawSTRA SmallValue
  | RawLDSP SmallValue
  | RawSTSP SmallValue
  | RawLDFP SmallValue
  | RawSTFP SmallValue
  | RawADC  SmallValue
  deriving (Show)

rawInstruction :: Parser (RawInstruction Address)
rawInstruction
  = label "instruction"
  $   RawLIT  <$> instr  "LIT"  mimaWord
  <|> RawLDC  <$> instr  "LDC"  address
  <|> RawLDV  <$> instr  "LDV"  address   
  <|> RawSTV  <$> instr  "STV"  address   
  <|> RawADD  <$> instr  "ADD"  address   
  <|> RawAND  <$> instr  "AND"  address   
  <|> RawOR   <$> instr  "OR"   address    
  <|> RawXOR  <$> instr  "XOR"  address   
  <|> RawEQL  <$> instr  "EQL"  address   
  <|> RawJMP  <$> instr  "JMP"  address   
  <|> RawJMN  <$> instr  "JMN"  address   
  <|> RawLDIV <$> instr  "LDIV" address  
  <|> RawSTIV <$> instr  "STIV" address  
  <|> RawCALL <$> instr  "CALL" address  
  <|> RawLDVR <$> instr  "LDVR" address  
  <|> RawSTVR <$> instr  "STVR" address  
  <|> RawHALT <$> instr' "HALT" smallValue 
  <|> RawNOT  <$> instr' "NOT"  smallValue 
  <|> RawRAR  <$> instr' "RAR"  smallValue 
  <|> RawRET  <$> instr' "RET"  smallValue 
  <|> RawLDRA <$> instr' "LDRA" smallValue 
  <|> RawSTRA <$> instr' "STRA" smallValue 
  <|> RawLDSP <$> instr' "LDSP" smallValue 
  <|> RawSTSP <$> instr' "STSP" smallValue 
  <|> RawLDFP <$> instr' "LDFP" smallValue 
  <|> RawSTFP <$> instr' "STFP" smallValue 
  <|> RawADC  <$> instr  "ADC"  smallValue 
  where
    instr  name value = lexeme (C.string' name >> whitespace) >> space *> value
    instr' name value = lexeme (C.string' name >> whitespace) *> (value <|> pure zeroBits)

{-
data InstrState = InstrState
  { isCurPos     :: MimaAddress
  , isPrevLabels :: Set.Set Label
  , isNewPos     :: Maybe MimaAddress
  , isLabels     :: Set.Set Label
  } deriving (Show)

instrState :: MimaAddress -> Set.Set Label -> InstrState
instrState curPos prevLabels = InstrState curPos prevLabels Nothing Set.empty

parseLabel :: StatefulParser InstrState ()
parseLabel = do
  name <- lift labelName
  is <- get
  let prevLabels = isPrevLabels is
      labels     = isLabels is
  if name `Set.member` prevLabels || name `Set.member` labels
    then fail "label can't be specified more than once"
    else do
      void $ lift $ lexeme $ C.string ":"
      put is{isLabels = Set.insert name labels}
    
parseLocationLabel :: StatefulParser InstrState ()
parseLocationLabel = do
  newPos <- lift largeValue'
  is <- get
  case isNewPos is of
    Just _ -> fail "cannot specify two positions for one instruction"
    Nothing -> if newPos < isCurPos is
      then fail "cannot set a position to an earlier position"
      else do
        void $ lift $ lexeme $ C.string ":"
        put is{isNewPos = Just newPos}

parseInstruction :: StatefulParser InstrState RawInstruction
parseInstruction = try parseLabel <|> parseLocationLabel
-}
