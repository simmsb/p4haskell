{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | P4 'compresses' it's json exported AST, this module decompresses it
-- if you're interested, it does some lazy magic to fetch the end state before the end
module P4Haskell.Types.AST.DecompressJSON
    ( tryParseVal
    , runDecompressor
    , type DecompressC ) where

import qualified Data.HashMap.Lazy as H

import           Polysemy.EndState
import           Polysemy.Fixpoint
import           Polysemy.State
import           Polysemy.Trace

import qualified Waargonaut.Decode as D


type DecompressState = HashMap Int D.JCurs

type DecompressC r =
  Members '[Fixpoint, State DecompressState,
  EndState DecompressState, Trace, Final Identity] r

addNode :: Member (State DecompressState) r => Int -> D.JCurs -> Sem r ()
addNode k v = modify (H.insert k v)

getNode :: Members '[State DecompressState, EndState DecompressState] r => Int -> Sem r D.JCurs
getNode k = do
  s <- get
  case H.lookup k s of
    Just x  -> pure x
    Nothing -> do
      es <- getEndState
      let Just x = H.lookup k es in pure x

isReferenceNode :: Monad m => D.JCurs -> D.DecodeResult m Bool
isReferenceNode curs = do
  o <- D.down curs
  ty <- D.fromKeyOptional "Node_Type" D.text o
  pure $ isNothing ty

tryParseVal
  :: forall r b. DecompressC r
  => (D.JCurs -> D.DecodeResult (Sem r) b)
  -> D.JCurs
  -> D.DecodeResult (Sem r) b
tryParseVal f curs = do
  ref <- isReferenceNode curs
  o <- D.down curs
  id' <- D.fromKey "Node_ID" D.int o
  curs' <- if ref
    then lift $ getNode id'
    else lift $ do
      addNode id' curs
      pure curs
  f curs'
  -- parsed <- f curs
  -- if ref
  --   then lift $ do
  --     n <- getNode id'
  --     let Just n' = fromDynamic @b n in pure n'
  --   else do
  --     b <- f curs
  --     lift $ addNode id' (toDyn b)
  --     pure b

runDecompressor
  :: forall a.
  Sem '[EndState DecompressState, State DecompressState,
  Fixpoint, Trace, Final Identity] a
  -> ([String], a)
runDecompressor =
    runIdentity
  . runFinal
  . runTraceList
  . fixpointToFinal @Identity
  . evalLazyState @DecompressState H.empty
  . runEndState @DecompressState
