{-# LANGUAGE OverloadedStrings #-}

module Mima.Assembler.Parser.Register
  ( Registers(..)
  , parseRegisters
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C

import           Mima.Assembler.Parser.Basic
import           Mima.Assembler.Parser.Label
import           Mima.Word

data Registers addr = Registers
  { regIAR :: Maybe addr
  , regACC :: Maybe MimaWord
  , regRA  :: Maybe addr
  , regSP  :: Maybe addr
  , regFP  :: Maybe addr
  } deriving (Show)

emptyRegisters :: Registers a
emptyRegisters = Registers Nothing Nothing Nothing Nothing Nothing

parseRegisters :: Parser (Registers Address)
parseRegisters = snd <$> runStatefulParser parseRegisters' emptyRegisters

parseRegisters' :: StatefulParser (Registers Address) ()
parseRegisters' = (parseARegister >> lift newlines >> parseRegisters') <|> pure ()

parseARegister :: StatefulParser (Registers Address) ()
parseARegister
  =   parseRegister "IAR" address  regIAR (\v reg -> reg{regIAR = Just v})
  <|> parseRegister "ACC" mimaWord regACC (\v reg -> reg{regACC = Just v})
  <|> parseRegister "RA"  address  regRA  (\v reg -> reg{regRA  = Just v})
  <|> parseRegister "SP"  address  regSP  (\v reg -> reg{regSP  = Just v})
  <|> parseRegister "FP"  address  regFP  (\v reg -> reg{regFP  = Just v})
  <?> "register initialisation"

parseRegister :: T.Text
              -> Parser x
              -> (Registers addr -> Maybe x)
              -> (x -> Registers addr -> Registers addr)
              -> StatefulParser (Registers addr) ()
parseRegister name parser readReg writeReg = do
  void $ lift $ lexeme $ C.string' name
  void $ lift $ lexeme $ C.string' "="
  reg <- get
  case readReg reg of
    Just _  -> fail $ "can't specify register " ++ T.unpack name ++ " twice"
    Nothing -> do
      x <- lift parser
      modify (writeReg x)
