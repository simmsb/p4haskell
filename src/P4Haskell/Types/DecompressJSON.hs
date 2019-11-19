{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | P4 'compresses' it's json exported AST, this module decompresses it
-- if you're interested, it does some lazy magic to fetch the end state before the end
module P4Haskell.Types.DecompressJSON
    ( tryParseVal
    , tryParseVal'
    , runDecompressor
    , type DecompressC ) where

import           Data.Generics.Sum.Typed
import qualified Data.HashMap.Lazy       as H

import           Polysemy.EndState
import           Polysemy.Fixpoint
import           Polysemy.State

import qualified Waargonaut.Decode       as D

type DecompressState s = HashMap Int s

type DecompressC s r =
  Members '[Fixpoint, State (DecompressState s),
  EndState (DecompressState s), Final Identity] r

addNode :: Member (State (DecompressState s)) r => Int -> s -> Sem r ()
addNode k v = modify (H.insert k v)

getNode :: Member (EndState (DecompressState s)) r => Int -> Sem r s
getNode k = do
  es <- getEndState
  let ~(Just x) = H.lookup k es in pure x

isReferenceNode :: Monad m => D.JCurs -> D.DecodeResult m Bool
isReferenceNode curs = do
  o <- D.down curs
  ty <- D.fromKeyOptional "Node_Type" D.text o
  pure $ isNothing ty

tryParseVal
  :: DecompressC s r
  => Prism' s b
  -> (D.JCurs -> D.DecodeResult (Sem r) b)
  -> D.JCurs
  -> D.DecodeResult (Sem r) b
tryParseVal t f curs = do
  ref <- isReferenceNode curs
  o <- D.down curs
  id' <- D.fromKey "Node_ID" D.int o
  if ref
    then lift $ do
      n <- getNode id'
      let ~(Just n') = n ^? t in pure n'
    else do
      b <- f curs
      lift $ addNode id' (b ^. re t)
      pure b

tryParseVal'
  :: forall s r b.
  (DecompressC s r, AsType b s)
  => (D.JCurs -> D.DecodeResult (Sem r) b)
  -> D.JCurs
  -> D.DecodeResult (Sem r) b
tryParseVal' = tryParseVal (_Typed @b @s)

runDecompressor
  :: forall s a.
  Sem '[EndState (DecompressState s), State (DecompressState s),
  Fixpoint, Final Identity] a
  -> a
runDecompressor = runIdentity
  . runFinal
  . fixpointToFinal @Identity
  . evalState @(DecompressState s) H.empty
  . runEndState @(DecompressState s)
