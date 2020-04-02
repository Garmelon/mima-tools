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
  , Span(..)
  -- * Phase1
  , Phase1
  , parsePhase1
  , formatPhase1
  ) where

import           Control.Monad
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Text            as A
import qualified Data.ByteString.Lazy       as BS
import           Data.Char
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)

import           Mima.Format
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
  | LargeInstruction a (LargeOpcode a) (Maybe (SmallValue a))
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
  | TokenComment a T.Text Bool
  -- ^ @'TokenComment' a text inline@ represents a comment.
  -- @inline@ is true if the comment is on the same line as an instruction or a label.
  deriving (Show)

instance Onion AsmToken where
  peel (TokenLabel a)       = peel a
  peel (TokenInstruction a) = peel a
  peel (TokenDirective a)   = peel a
  peel (TokenComment a _ _) = a

data Span = Span SourcePos SourcePos

instance Show Span where
  show (Span start end) = "<" ++ showPos start ++ "-" ++ showPos end ++ ">"
    where
      showPos pos =
        show (unPos $ sourceLine pos) ++ ":" ++ show (unPos $ sourceColumn pos)

type Phase1 = [AsmToken Span]

{- Parsing -}

type Parser = Parsec Void T.Text

inlineSpace :: Parser ()
inlineSpace = void $ takeWhileP (Just "space (no newline)") (\x -> isSpace x && x /= '\n')

inlineSpace1 :: Parser ()
inlineSpace1 = void $ takeWhile1P (Just "space (no newline)") (\x -> isSpace x && x /= '\n')

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
instruction = small <|> large
  where
    small = do
      start <- getSourcePos
      so <- smallOpcode
      inlineSpace1
      loc <- location
      stop <- getSourcePos
      pure $ SmallInstruction (Span start stop) so loc
    large = do
      start <- getSourcePos
      lo <- largeOpcode
      sv <- optionalAwareArgument lo
      stop <- getSourcePos
      pure $ LargeInstruction (Span start stop) lo sv
    optionalAwareArgument (LargeOpcode _ code)
      | Vm.argumentIsOptional code = optional (inlineSpace1 *> smallValue <?> "argument")
      | otherwise                  = Just <$> (inlineSpace1 *> smallValue <?> "argument")

singleDirective
  :: (Span -> Span -> a -> b Span)
  -> T.Text
  -> Parser a
  -> Parser (b Span)
singleDirective f t p = do
  (outerSpan, (nameSpan, a)) <- withSpan $ do
    (nameSpan, _) <- withSpan $ chunk t
    inlineSpace1
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
      (outerSpan, (regSpan, mimaWords)) <- withSpan $ do
        (dirSpan, _) <- withSpan $ chunk ".arr"
        inlineSpace1
        mimaWords <- between (char '[') (char ']') $
          sepBy1 mimaWord (char ',' *> inlineSpace)
        pure (dirSpan, mimaWords)
      pure $ Arr outerSpan regSpan mimaWords

    metaStart f keyword = do
      (outerSpan, (regSpan, metaName, jsonValue)) <- withSpan $ do
        (dirSpan, _) <- withSpan $ chunk keyword
        inlineSpace1
        metaName <- name
        inlineSpace1

        (valueSpan, rawJsonValue) <- withSpan $ do
          metaValueBS <- BS.fromStrict . T.encodeUtf8
            <$> takeWhile1P (Just "json value") (/= '\n')
          case A.eitherDecode metaValueBS of
            Left msg    -> fail msg
            Right value -> pure value

        pure (dirSpan, metaName, JsonValue valueSpan rawJsonValue)
      pure $ f outerSpan regSpan metaName jsonValue

comment :: Bool -> Parser (AsmToken Span)
comment inline = fmap (\(s, text) -> TokenComment s text inline) $ withSpan $
  char ';' *> takeWhileP (Just "comment") (/= '\n')

asmToken :: Parser (AsmToken Span)
asmToken = (TokenInstruction <$> instruction) <|> (TokenDirective <$> directive)

labels :: Parser [AsmToken Span]
labels = do
  start <- optional label_
  case start of
    Nothing -> pure []
    Just l  -> do
      ls <- many $ try $ inlineSpace1 *> label_
      pure $ l : ls
  where
    label_ = (TokenLabel <$> name) <* (inlineSpace <* char ':')

-- | Parses a single line consisting of zero or more tokens:
-- inlineSpace, zero or more labels, zero or more instructions/directives,
-- zero or more comments and a final newline or EOF.
lineParser :: Parser [AsmToken Span]
lineParser = do
  inlineSpace
  ls <- labels
  t <- optional $ try $ do
    unless (null ls) inlineSpace1
    asmToken
  c <- optional $ try $ do
    let aloneOnLine = null ls && isNothing t
    unless aloneOnLine inlineSpace1
    comment $ not aloneOnLine
  pure $ ls ++ toList t ++ toList c

parsePhase1 :: Parser Phase1
parsePhase1 = mconcat <$> sepBy lineParser newline <* eof

{- Formatting -}

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
formatDirective (Meta _ _ n val) =
  ".meta " <> formatName n <> " " <> formatJsonValue val
formatDirective (MetaStart _ _ n val) =
  ".meta-start " <> formatName n <> " " <> formatJsonValue val
formatDirective (MetaStop _ _ n) = ".meta-stop " <> formatName n

formatToken :: AsmToken a -> T.Text
formatToken (TokenLabel n)          = formatName n <> ":"
formatToken (TokenInstruction ins)  = "  " <> formatInstruction ins
formatToken (TokenDirective dir)    = formatDirective dir
formatToken (TokenComment _ text _) = ";" <> text

formatPhase1 :: Phase1 -> T.Text
formatPhase1 (x:y@(TokenComment _ _ True):xs) = formatToken x <> " " <> formatPhase1 (y:xs)
formatPhase1 (x:xs) = formatToken x <> "\n" <> formatPhase1 xs
formatPhase1 [] = ""
