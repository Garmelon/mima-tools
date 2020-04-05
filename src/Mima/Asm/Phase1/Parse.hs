{-# LANGUAGE OverloadedStrings #-}

module Mima.Asm.Phase1.Parse
  ( Span(..)
  , Parser
  , parsePhase1
  ) where

import           Control.Monad
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BS
import           Data.Char
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)

import           Mima.Asm.Phase1.Types
import qualified Mima.Vm.Instruction        as Vm

data Span = Span Int Int

instance Show Span where
  show (Span start end) = "<" ++ show start ++ "-" ++ show end ++ ">"

type Parser = Parsec Void T.Text

inlineSpace :: Parser ()
inlineSpace = void $ takeWhileP (Just "space (no newline)") (\x -> isSpace x && x /= '\n')

inlineSpace1 :: Parser ()
inlineSpace1 = void $ takeWhile1P (Just "space (no newline)") (\x -> isSpace x && x /= '\n')

withSpan :: Parser a -> Parser (Span, a)
withSpan f = do
  start <- getOffset
  result <- f
  stop <- getOffset
  pure (Span start stop, result)

name :: Parser (Name Span)
name = fmap (uncurry Name) $ withSpan $ do
  firstChar <- satisfy isLower <?> "lowercase character"
  otherChars <- takeWhileP (Just "alphanumeric character or '_'") (\c -> isAlphaNum c || c == '_')
  pure $ T.pack [firstChar] <> otherChars

number :: (Num a) => Parser a
number =
  (chunk "0b" *> binary) <|>
  (chunk "0o" *> octal) <|>
  (chunk "0x" *> hexadecimal) <|>
  decimal

signedNumber :: (Num a) => Parser a
signedNumber = signed (pure ()) number -- do not allow any space

address :: Parser (Address Span)
address =
  fmap (uncurry AddressRelative) (withSpan $ between (char '[') (char ']') signedNumber) <|>
  fmap (uncurry AddressAbsolute) (withSpan signedNumber)

labelWithOffset :: Parser (Location Span)
labelWithOffset = do
  (completeSpan, (n, offsetSpan, offset)) <- withSpan $ do
    n <- name
    (offsetSpan, offset) <- withSpan $ between (char '[') (char ']') signedNumber
    pure (n, offsetSpan, offset)

  pure $ LocationLabelRel completeSpan n offsetSpan offset

location :: Parser (Location Span)
location =
  (LocationAddress <$> address) <|> try labelWithOffset <|> (LocationLabel <$> name)

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
  (uncurry WordRaw <$> withSpan signedNumber) <|> (WordLocation <$> location)

smallValue :: Parser (SmallValue Span)
smallValue = uncurry SmallValue <$> withSpan signedNumber

instruction :: Parser (Instruction Span)
instruction = small <|> large
  where
    small = do
      start <- getOffset
      so <- smallOpcode
      inlineSpace1
      loc <- location
      stop <- getOffset
      pure $ SmallInstruction (Span start stop) so loc
    large = do
      start <- getOffset
      lo <- largeOpcode
      sv <- optionalAwareArgument lo
      stop <- getOffset
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
    label_ = (TokenLabel <$> name) <* char ':'

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

parsePhase1 :: Parser (Phase1 Span)
parsePhase1 = mconcat <$> sepBy lineParser newline <* eof
