{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | P4 'compresses' it's json exported AST, this module decompresses it
-- if you're interested, it does some lazy magic to fetch the end state before the end
module P4Haskell.Types.AST.DecompressJSON
    ( tryParseVal
    , runDecompressor
    , type DecompressC ) where

import qualified Data.HashMap.Lazy       as H

import           Polysemy.EndState
import           Polysemy.EndState
import           Polysemy.Fixpoint
import           Polysemy.State
import           Data.Dynamic

import qualified Waargonaut.Decode       as D

type DecompressState = HashMap Int Dynamic

type DecompressC r =
  Members '[Fixpoint, State DecompressState,
  EndState DecompressState, Final Identity] r

addNode :: Member (State DecompressState) r => Int -> Dynamic -> Sem r ()
addNode k v = modify (H.insert k v)

getNode :: Member (EndState DecompressState) r => Int -> Sem r Dynamic
getNode k = do
  es <- getEndState
  let ~(Just x) = H.lookup k es in pure x

isReferenceNode :: Monad m => D.JCurs -> D.DecodeResult m Bool
isReferenceNode curs = do
  o <- D.down curs
  ty <- D.fromKeyOptional "Node_Type" D.text o
  pure $ isNothing ty

tryParseVal
  :: forall r b. (Typeable b, DecompressC r)
  => (D.JCurs -> D.DecodeResult (Sem r) b)
  -> D.JCurs
  -> D.DecodeResult (Sem r) b
tryParseVal f curs = do
  ref <- isReferenceNode curs
  o <- D.down curs
  id' <- D.fromKey "Node_ID" D.int o
  if ref
    then lift $ do
      n <- getNode id'
      let ~(Just n') = fromDynamic @b n in pure n'
    else do
      b <- f curs
      lift $ addNode id' (toDyn b)
      pure b

runDecompressor
  :: forall a.
  Sem '[EndState DecompressState, State DecompressState,
  Fixpoint, Final Identity] a
  -> a
runDecompressor = runIdentity
  . runFinal
  . fixpointToFinal @Identity
  . evalState @DecompressState H.empty
  . runEndState @DecompressState
