{-# LANGUAGE DataKinds #-}

module Mima.Asm.Phase2.Subphase3
  ( subphase3
  , ResultS3
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Aeson                as A
import           Data.Foldable
import           Data.List
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.Void

import           Mima.Asm.Phase2.Types
import           Mima.Asm.Weed
import qualified Mima.Vm.Metadata          as Vm
import qualified Mima.Vm.Word              as Vm

data SortedByIndex a = SortedByIndex Int a
  deriving (Show)

instance Eq (SortedByIndex a) where
  (SortedByIndex a _) == (SortedByIndex b _) = a == b

instance Ord (SortedByIndex a) where
  compare (SortedByIndex a _) (SortedByIndex b _) = compare a b

getValue :: SortedByIndex a -> a
getValue (SortedByIndex _ a) = a

data StateS3 s = StateS3
  { s3Labels         :: Map.Map T.Text Vm.MimaAddress
  , s3GlobalMeta     :: Vm.MetaInfo
  , s3LocalMeta      :: [SortedByIndex Vm.Range]
  , s3OpenMetaRanges :: Map.Map T.Text [SortedByIndex (s, A.Value, Vm.MimaAddress)]
  , s3Index          :: Int
  } deriving (Show)

type WeedS3 s = StateT (StateS3 s) (Weed (WeedError s))

nextIndex :: WeedS3 s Int
nextIndex = do
  s3 <- get
  let i = s3Index s3
  put s3{s3Index = i + 1}
  pure i

addLabel :: Vm.MimaAddress -> Name s -> WeedS3 s ()
addLabel addr (Name s nameText) = do
  s3 <- get
  let labels = s3Labels s3
  if nameText `Map.member` labels
    then lift $ harmless $ errorWith s "label already set previously"
    else put s3{s3Labels = Map.insert nameText addr labels}

addGlobalMeta :: Name s -> A.Value -> WeedS3 s ()
addGlobalMeta (Name s nameText) val = do
  s3 <- get
  let globalMeta = s3GlobalMeta s3
  if nameText `Map.member` globalMeta
    then lift $ harmless $ errorWith s "global meta with this name already set"
    else put s3 {s3GlobalMeta = Map.insert nameText val globalMeta}

addRange :: Int -> Vm.Range -> WeedS3 s ()
addRange i r =
  modify $ \s3 -> s3 {s3LocalMeta = SortedByIndex i r : s3LocalMeta s3}

addLocalMeta :: Vm.MimaAddress -> Name s -> A.Value -> WeedS3 s ()
addLocalMeta addr (Name _ nameText) val = do
  i <- nextIndex
  addRange i $ Vm.RangeAt (Map.singleton nameText val) addr

startLocalMeta :: Vm.MimaAddress -> s -> Name s -> A.Value -> WeedS3 s ()
startLocalMeta addr s (Name _ nameText) val = do
  i <- nextIndex
  s3 <- get
  let ranges = s3OpenMetaRanges s3
      tuple = SortedByIndex i (s, val, addr)
      ranges' = Map.alter (Just . (tuple :) . fromMaybe []) nameText ranges
  put s3{s3OpenMetaRanges = ranges'}

stopLocalMeta :: Vm.MimaAddress -> Name s -> WeedS3 s ()
stopLocalMeta stopAddr (Name s nameText) = do
  s3 <- get
  let ranges = s3OpenMetaRanges s3
  case fromMaybe [] $ ranges Map.!? nameText of
    [] -> lift $ harmless $ errorWith s "closing unopened meta"
    (SortedByIndex i (_, val, startAddr):xs) -> do
      let range = Vm.RangeFromTo (Map.singleton nameText val) startAddr stopAddr
          ranges' = Map.insert nameText xs ranges
      put s3{s3OpenMetaRanges = ranges'}
      addRange i range

handleMeta :: Vm.MimaAddress -> Meta s -> WeedS3 s ()
handleMeta _    (MetaGlobal _ name (JsonValue _ val)) = addGlobalMeta name val
handleMeta addr (MetaStart  s name (JsonValue _ val)) = startLocalMeta addr s name val
handleMeta addr (MetaStop   _ name)                   = stopLocalMeta addr name
handleMeta addr (Meta       _ name (JsonValue _ val)) = addLocalMeta addr name val

updateToken :: AsmToken 'S2 s -> WeedS3 s [AsmToken 'S3 s]
updateToken (TokenOrg   _ x)         = absurd x
updateToken (TokenLabel _ addr name) = [] <$ addLabel addr name
updateToken (TokenMeta  _ addr meta) = [] <$ handleMeta addr meta
updateToken (TokenLit   s addr word) = pure [TokenLit   s addr $ idWord word]
updateToken (TokenInstr s addr i)    = pure [TokenInstr s addr $ idInstruction i]
updateToken (TokenReg   s addr reg)  = pure [TokenReg   s addr $ idRegDir reg]

type ResultS3 s = (Phase2 'S3 s, Map.Map T.Text Vm.MimaAddress, Vm.Metadata)

subphase3 :: Phase2 'S2 s -> Weed (WeedError s) (ResultS3 s)
subphase3 tokens = do
  let initialS = StateS3 Map.empty Map.empty [] Map.empty 0
  (newTokens, finalS) <- runStateT (traverse updateToken tokens) initialS
  let labels = s3Labels finalS
      global = s3GlobalMeta finalS
      local = map getValue $ sort $ s3LocalMeta finalS
      metadata = Vm.Metadata global local
      openRanges = map getValue $ concat $ Map.elems $ s3OpenMetaRanges finalS
  for_ openRanges $ \(s, _, _) ->
    harmless $ errorWith s "meta range was not closed"
  pure (concat newTokens, labels, metadata)
