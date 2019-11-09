module Mima.Assembler.Parser.Instruction
  ( parseInstruction
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Maybe
import qualified Data.Set as Set
import           Text.Megaparsec

import           Mima.Assembler.Parser.Basic
import           Mima.Assembler.Parser.Label
import           Mima.Assembler.Parser.RawInstruction
import           Mima.Word

data MyState = MyState
  { sCurrentPos  :: MimaAddress
  , sKnownLabels :: Set.Set MimaLabel
  , sActualPos   :: Maybe MimaAddress
  , sLabels      :: Set.Set MimaLabel
  } deriving (Show)

initialState :: MimaAddress -> Set.Set MimaLabel -> MyState
initialState currentPos knownLabels = MyState
  { sCurrentPos = currentPos
  , sKnownLabels = knownLabels
  , sActualPos = Nothing
  , sLabels = Set.empty
  }

getActualPos :: MyState -> MimaAddress
getActualPos s = fromMaybe (sCurrentPos s) (sActualPos s)

alreadySeen :: MimaLabel -> MyState -> Bool
alreadySeen l s = l `Set.member` sKnownLabels s || l `Set.member` sLabels s

addLabel :: MimaLabel -> MyState -> MyState
addLabel l s = s{sLabels = Set.insert l $ sLabels s}

{- And now, the parsing -}

type SParser a = StatefulParser MyState a

parseLabel :: SParser ()
parseLabel = do
  s <- get
  l <- lift $ try $ mimaLabel' <* colon
  void $ lift $ many newline
  if alreadySeen l s
    then lift $ failAtLabel l "label already defined earlier"
    else modify (addLabel l)

parseAddressLabel :: SParser ()
parseAddressLabel = do
  s <- get
  (addr, offset) <- lift $ try $ withOffset largeValue' <* colon
  void $ lift $ many newline
  when (addr < sCurrentPos s) $ do
    let errorMsg = "address can't be earlier than " ++ show (sCurrentPos s)
    lift $ failAt offset errorMsg
  case sActualPos s of
    Just _  -> lift $ failAt offset "can't set an instruction's address twice"
    Nothing -> put s{sActualPos = Just addr}

parseInstruction' :: SParser (RawInstruction Address)
parseInstruction' = do
  void $ many (parseLabel <|> parseAddressLabel)
  lift $ rawInstruction <* newlines

-- | @'parseInstruction' currentPos knownLabels@ parses an instruction and
-- its preceding label markings.
--
-- * @currentPos@ is the position at which, if no other marking is
--   specified, this instruction is located.
--
-- * @knownLabels@ are the labels which have already been set
--   elsewhere and thus cannot be set again on this instruction.
--
-- Returns @(actualPos, instruction, labels)@.
--
-- * @actualPos@ is the position at which the instruction is actually
--   located in memory. This can differ from @currentPos@ if a
--   location label is attached to this instruction. The following
--   must always hold: @actualPos >= currentPos@.
--
-- * @instruction@ is the 'RawInstruction' that was parsed.
--
-- * @labels@ are the labels attached to the parsed instruction.
parseInstruction :: MimaAddress
                 -> Set.Set MimaLabel
                 -> Parser (MimaAddress, RawInstruction Address, Set.Set MimaLabel)
parseInstruction currentPos knownLabels = do
  let s = initialState currentPos knownLabels
  (instruction, s') <- runStatefulParser parseInstruction' s
  let actualPos = getActualPos s'
      labels    = sLabels s'
  pure (actualPos, instruction, labels)
