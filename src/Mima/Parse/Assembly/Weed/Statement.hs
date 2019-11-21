{-# LANGUAGE RecordWildCards #-}

module Mima.Parse.Assembly.Weed.Statement
  ( weedStatements
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set

import           Mima.Flag
import           Mima.Label
import           Mima.Parse.Assembly.Common
import           Mima.Parse.Assembly.Directive
import           Mima.Parse.Assembly.RawInstruction
import           Mima.Parse.Assembly.Statement
import           Mima.Parse.Assembly.Weed.Common
import           Mima.Parse.Weed
import           Mima.Word

data WeedState = WeedState
  { wsAt        :: MimaAddress
  , wsOccupied  :: Bool
  , wsOpenFlags :: Map.Map Char MimaAddress
  , wsResult    :: WeedResult Address
  } deriving (Show)

initialState :: WeedState
initialState = WeedState
  { wsAt        = minBound
  , wsOccupied  = False
  , wsOpenFlags = Map.empty
  , wsResult    = emptyResult
  }

-- Sweet!
type SWeed a = StateT WeedState (Weed WeedError) a

{- State manipulation -}

-- Yes, I know that lenses would probably make the below code much nicer. I may
-- get around to that eventually.

modifyResult :: (WeedResult Address -> WeedResult Address) -> SWeed ()
modifyResult f = modify (\s -> s{wsResult = f (wsResult s)})

{- Let's start weeding -}

-- | Advance to the next unoccupied address and return that. This function
-- either returns the current wsAt (if not wsOccupied) or advances wsAt and
-- returns that.
--
-- This function takes an object with an offset, which it uses to produce an
-- error if it could not advance to an unoccupied address.
toNextFree :: WithOffset a -> SWeed MimaAddress
toNextFree thing = do
  s@WeedState{..} <- get
  if wsOccupied
    then if wsAt >= maxBound 
         then lift $ critical $ errorAt thing "No more space in memory, already at max address"
         else let next = wsAt + 1 in next <$ put s{wsAt = next, wsOccupied = False}
    else pure wsAt

helpSetRegister :: WithOffset a
                -> (Registers Address -> Maybe c)
                -> (Registers Address -> Registers Address)
                -> SWeed ()
helpSetRegister thing readF writeF = do
  WeedState{..} <- get
  case readF (wrRegisters wsResult) of
    Nothing -> modifyResult (\r -> r{wrRegisters = writeF (wrRegisters r)})
    Just _  -> lift $ harmless $ errorAt thing "Register was already set earlier"

setRegister :: WithOffset a -> SetRegister Address -> SWeed ()
setRegister thing (SetIAR a) = helpSetRegister thing rIAR (\r -> r{rIAR = Just a})
setRegister thing (SetACC a) = helpSetRegister thing rACC (\r -> r{rACC = Just a})
setRegister thing (SetRA  a) = helpSetRegister thing rRA  (\r -> r{rRA  = Just a})
setRegister thing (SetSP  a) = helpSetRegister thing rSP  (\r -> r{rSP  = Just a})
setRegister thing (SetFP  a) = helpSetRegister thing rFP  (\r -> r{rFP  = Just a})

setAddressTo :: WithOffset a -> MimaAddress -> SWeed ()
setAddressTo thing addr = do
  s@WeedState{..} <- get
  if (addr > wsAt) || (not wsOccupied && addr == wsAt)
    then put s{wsAt = addr, wsOccupied = False}
    else lift $ harmless $ errorAt thing "Can only increase address"

addAlmostWord :: WithOffset a -> AlmostWord Address -> SWeed ()
addAlmostWord thing aw = do
  addr <- toNextFree thing
  modifyResult (\r -> r{wrMemory = Map.insert addr aw (wrMemory r)})

addLabel :: WithOffset a -> LabelName -> SWeed ()
addLabel thing l = do
  addr <- toNextFree thing
  WeedState{..} <- get
  case wrLabels wsResult Map.!? l of
    Nothing -> modifyResult (\r -> r{wrLabels = Map.insert l addr (wrLabels r)})
    Just _  -> lift $ harmless $ errorAt thing "Label was already defined earlier"

addFlagRange :: Char -> AddressRange -> SWeed()
addFlagRange c r =
  modifyResult (\res -> res{wrFlags = Map.alter (Just . (r:) . fromMaybe []) c (wrFlags res)})

setFlag :: WithOffset a -> Char -> SWeed ()
setFlag thing c = do
  addr <- toNextFree thing
  addFlagRange c $ range addr addr

turnFlagOn :: WithOffset a -> Char -> SWeed ()
turnFlagOn thing c = do
  s@WeedState{..} <- get
  case wsOpenFlags Map.!? c of
    Just _  -> lift $ harmless $ errorAt thing "Flag is already active at this address"
    Nothing -> put s{wsOpenFlags = Map.insert c wsAt wsOpenFlags}

turnFlagOff :: WithOffset a -> Char -> SWeed ()
turnFlagOff thing c = do
  s@WeedState{..} <- get
  case wsOpenFlags Map.!? c of
    Nothing    -> lift $ harmless $ errorAt thing "Flag is not active at this address"
    Just start -> do
      put s {wsOpenFlags = Map.delete c wsOpenFlags}
      addFlagRange c $ range start wsAt

{- Weeding at a larger scale -}

weedDirective :: WithOffset a -> Directive Address -> SWeed ()
weedDirective thing d = do
  case d of
    DReg sr        -> setRegister thing sr
    DOrg addr      -> setAddressTo thing addr
    DLit w         -> addAlmostWord thing (ALiteral w)
    DArr ws        -> mapM_ (addAlmostWord thing . ALiteral) ws
    DFlag chars    -> mapM_ (setFlag thing)     (Set.toList chars)
    DFlagOn chars  -> mapM_ (turnFlagOn thing)  (Set.toList chars)
    DFlagOff chars -> mapM_ (turnFlagOff thing) (Set.toList chars)

weedInstruction :: WithOffset a -> RawInstruction Address -> SWeed ()
weedInstruction thing i = addAlmostWord thing $ AInstruction i

weedStep :: WithOffset (Statement Address) -> SWeed ()
weedStep thing =
  case woValue thing of
    SDirective d      -> weedDirective thing d
    SRawInstruction i -> weedInstruction thing i
    SLabel l          -> addLabel thing l

weedStatements :: [WithOffset (Statement Address)] -> Weed WeedError (WeedResult Address)
weedStatements statements = wsResult <$> execStateT (mapM_ weedStep statements) initialState
