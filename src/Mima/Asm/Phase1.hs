{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Monad
import           Control.Monad.Trans.Writer
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BS
import           Data.Char
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)

import qualified Mima.Vm.Instruction        as Vm
import qualified Mima.Vm.Word               as Vm

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
  | AddressRelative a Integer
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

-- | The first @a@ parameter represents the span of the whole thing. The second
-- @a@ parameter represents the span of the directive literal (e. g. @.org@).
data RegisterDirective a
  = RegIar a a (Location a)
  | RegAcc a a (MimaWord a)
  | RegRa  a a (Location a)
  | RegSp  a a (Location a)
  | RegFp  a a (Location a)
  deriving (Show)

instance Onion RegisterDirective where
  peel (RegIar a _ _) = a
  peel (RegAcc a _ _) = a
  peel (RegRa  a _ _) = a
  peel (RegSp  a _ _) = a
  peel (RegFp  a _ _) = a

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

type Phase1 = [AsmToken Span]
type Parser = WriterT (Endo Phase1) (Parsec Void T.Text)

data Span = Span SourcePos SourcePos

instance Show Span where
  show (Span start stop)
    = "[" ++ formatSourcePos start ++ " - " ++ formatSourcePos stop ++ "]"
    where
      formatSourcePos sp
        = formatPos (sourceLine sp) ++ ":" ++ formatPos (sourceColumn sp)
      formatPos = show . unPos

addTokens :: [AsmToken Span] -> Parser ()
addTokens = tell . Endo . (++)

addToken :: AsmToken Span -> Parser ()
addToken t = addTokens [t]

withSpan :: Parser a -> Parser (Span, a)
withSpan f = do
  start <- getSourcePos
  result <- f
  stop <- getSourcePos
  pure (Span start stop, result)

name :: Parser (Name Span)
name = fmap (uncurry Name) $ withSpan $ do
  firstChar <- satisfy isLower <?> "lowercase character"
  otherChars <- takeWhileP (Just "alphanumeric character") isAlphaNum
  pure $ T.pack [firstChar] <> otherChars

number :: (Num a) => Parser a
number =
  (chunk "0b" *> binary) <|>
  (chunk "0o" *> octal) <|>
  (chunk "0x" *> hexadecimal) <|>
  decimal

optionallySignedNumber :: (Num a) => Parser a
optionallySignedNumber = signed (pure ()) number -- do not allow any space

signedNumber :: (Num a) => Parser a
signedNumber = lookAhead (char '+' <|> char '-') *> optionallySignedNumber

boundedNumber :: (Bounded n, Num n) => Parser n
boundedNumber = do
  n <- optionallySignedNumber :: Parser Integer
  when (n < minVal || n > maxVal) $ fail $
    "invalid range: " ++
    show n ++ " is not between " ++
    show minVal ++ " and " ++ show maxVal
  pure $ fromInteger n
  where
    maxVal = toInteger (maxBound :: Vm.MimaWord)
    minVal = -(maxVal + 1)

address :: Parser (Address Span)
address =
  fmap (uncurry AddressRelative) (withSpan signedNumber) <|>
  fmap (uncurry AddressAbsolute) (withSpan boundedNumber)

location :: Parser (Location Span)
location = (LocationAddress <$> address) <|> (LocationLabel <$> name)

smallOpcode :: Parser (SmallOpcode Span)
smallOpcode = asum $ map parseOpcode [minBound..maxBound]
  where
    parseOpcode o = do
      (s, _) <- withSpan $ chunk $ T.pack $ show o
      pure $ SmallOpcode s o

largeOpcode :: Parser (LargeOpcode Span)
largeOpcode = asum $ map parseOpcode [minBound..maxBound]
  where
    parseOpcode o = do
      (s, _) <- withSpan $ chunk $ T.pack $ show o
      pure $ LargeOpcode s o

mimaWord :: Parser (MimaWord Span)
mimaWord =
  (uncurry WordRaw <$> withSpan boundedNumber) <|> (WordLocation <$> location)

smallValue :: Parser (SmallValue Span)
smallValue = uncurry SmallValue <$> withSpan boundedNumber

instruction :: Parser (Instruction Span)
-- TODO: Instructions without arguments! (e.g. HALT)
instruction = small <|> large
  where
    small = do
      start <- getSourcePos
      so <- smallOpcode
      space1
      loc <- location
      stop <- getSourcePos
      pure $ SmallInstruction (Span start stop) so loc
    large = do
      start <- getSourcePos
      lo <- largeOpcode
      space1
      sv <- smallValue
      stop <- getSourcePos
      pure $ LargeInstruction (Span start stop) lo sv

singleDirective
  :: (Span -> Span -> a -> b Span)
  -> T.Text
  -> Parser a
  -> Parser (b Span)
singleDirective f t p = do
  (outerSpan, (nameSpan, a)) <- withSpan $ do
    (nameSpan, _) <- withSpan $ chunk t
    space1
    a <- p
    pure (nameSpan, a)
  pure $ f outerSpan nameSpan a

registerDirective :: Parser (RegisterDirective Span)
registerDirective =
  singleDirective RegIar "IAR" location <|>
  singleDirective RegAcc "ACC" mimaWord <|>
  singleDirective RegRa  "RA"  location <|>
  singleDirective RegSp  "SP"  location <|>
  singleDirective RegFp  "FP"  location

directive :: Parser (Directive Span)
directive =
  singleDirective Reg ".reg" registerDirective <|>
  singleDirective Org ".org" address           <|>
  singleDirective Lit ".lit" mimaWord          <|>
  arr                                          <|>
  metaStart MetaStart ".meta-start"            <|>
  singleDirective MetaStop ".meta-stop" name   <|>
  metaStart Meta ".meta"
  where
    arr = do
      (outerSpan, (regSpan, words)) <- withSpan $ do
        (dirSpan, _) <- withSpan $ chunk ".arr"
        space1
        words <- between (char '[') (char ']') $
          sepBy1 mimaWord (char ',' *> space)
        pure (dirSpan, words)

      pure $ Arr outerSpan regSpan words
    metaStart f keyword = do
      (outerSpan, (regSpan, metaName, jsonValue)) <- withSpan $ do
        (dirSpan, _) <- withSpan $ chunk keyword
        space1
        metaName <- name
        space1

        (valueSpan, rawJsonValue) <- withSpan $ do
          metaValueBS <- BS.fromStrict . T.encodeUtf8
            <$> takeWhile1P (Just "json value") (/= '\n')
          case A.eitherDecode metaValueBS of
            Left msg    -> fail msg
            Right value -> pure value

        pure (dirSpan, metaName, JsonValue valueSpan rawJsonValue)

      pure $ f outerSpan regSpan metaName jsonValue

comment :: Parser T.Text
comment = char ';' *> takeWhileP (Just "comment") (/= '\n')

asmToken :: Parser (AsmToken Span)
asmToken
  = (TokenLabel <$> name)                          <|>
    (TokenInstruction <$> instruction)             <|>
    (TokenDirective <$> directive)                 <|>
    fmap (uncurry TokenComment) (withSpan comment)

parsePhase1 :: Parser Phase1
parsePhase1 = many (space *> asmToken)

-- | A small helper for visualizing the parse.
--
-- > doParse address "+200"
-- TODO: Delete this helper
doParse :: (Show a) => Parser a -> String -> IO ()
doParse p input = case parse parsecParser "" (T.pack input) of
  Left msg                 -> putStrLn $ errorBundlePretty msg
  Right (res, tokenStream) -> putStrLn $ "Success:\n  " ++ show res ++ "\n  " ++ show (appEndo tokenStream [])
  where parsecParser = runWriterT p

parseAssembly :: T.Text -> Either T.Text Phase1
parseAssembly input = case parse (runWriterT parsePhase1) "" input of
  Left msg          -> Left $ T.pack $ errorBundlePretty msg
  Right (result, _) -> Right result

displayParseResult :: Either T.Text Phase1 -> IO ()
displayParseResult (Left msg)  = putStrLn $ T.unpack msg
displayParseResult (Right val) = traverse_ print val
