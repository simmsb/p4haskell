{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | P4 'compresses' it's json exported AST, this module decompresses it
-- if you're interested, it does some lazy magic to fetch the end state before the end
module P4Haskell.Types.AST.DecompressJSON
    ( tryParseVal
    , tryDecoder
    , runDecompressor
    , currentNodeType
    , type DecompressC ) where

import           Data.Dynamic
import qualified Data.HashMap.Lazy as H

import           Polysemy.EndState
import           Polysemy.Fixpoint
import           Polysemy.State
import           Polysemy.Trace

import qualified Waargonaut.Decode as D

import qualified Debug.Trace as T

import Type.Reflection
import qualified Relude

type StoredData = (Dynamic, Text)

type DecompressState = HashMap Int StoredData

type DecompressC r =
  (Members '[Fixpoint, State DecompressState,
  EndState DecompressState, Trace, Final Identity] r, HasCallStack)

addNode :: Member (State DecompressState) r => Int -> StoredData -> Sem r ()
addNode k v = modify (H.insert k v)

getNode :: Members '[EndState DecompressState] r => Int -> Sem r StoredData
getNode k = do
  es <- getEndState
  let Just x = H.lookup k es
    in pure x

isReferenceNode :: Monad m => D.JCurs -> D.DecodeResult m Bool
isReferenceNode curs = do
  ty <- D.fromKeyOptional "Node_Type" D.text curs
  pure $ isNothing ty

currentNodeType :: DecompressC r => D.JCurs -> D.DecodeResult (Sem r) Text
currentNodeType o = do
  ref <- isReferenceNode o
  id' <- D.fromKey "Node_ID" D.int o
  obj <- D.focus D.json o
  if ref
    then lift $ do
      T.traceM $ show obj
      T.traceM $ "getting type of unvisited node: " <> show id' <> ", cs: " <> prettyCallStack callStack
      ~(_, ty) <- getNode id'
      pure ty
    else D.fromKey "Node_Type" D.text o

tryParseVal
  :: forall r b. (HasCallStack, Typeable b, DecompressC r)
  => (D.JCurs -> D.DecodeResult (Sem r) b)
  -> D.JCurs
  -> D.DecodeResult (Sem r) b
tryParseVal f o = do
  ref <- isReferenceNode o
  id' <- D.fromKey "Node_ID" D.int o
  T.traceM $ "working with node: " <> show id' <> ", dest type: " <> show (someTypeRep $ Proxy @b) <> ", cs: " <> prettyCallStack callStack
  if ref
    then lift $ do
      Relude.error "fuck"
      -- ~(n, _) <- getNode id'
      -- trace $ "my type: " <> show (dynTypeRep n) <> ", wanted type: " <> show (someTypeRep $ Proxy @b) <> ", same? " <> show (dynTypeRep n == someTypeRep (Proxy @b))
      -- let Just n' = fromDynamic @b n in pure n'
    else do
      b <- f o
      ty <- D.fromKey "Node_Type" D.text o
      lift $ addNode id' (toDyn b, ty)
      pure b

tryDecoder
  :: forall r b. (Typeable b, DecompressC r)
  => D.Decoder (Sem r) b
  -> D.JCurs
  -> D.DecodeResult (Sem r) b
tryDecoder = tryParseVal . D.focus

runDecompressor
  :: forall a.
  Sem '[Trace, EndState DecompressState, State DecompressState,
  Fixpoint, Final Identity] a
  -> ([String], a)
runDecompressor =
    runIdentity
  . runFinal
  . fixpointToFinal @Identity
  . evalState @DecompressState H.empty
  . runEndState @DecompressState
  . runTraceList
