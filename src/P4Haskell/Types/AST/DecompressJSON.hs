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
import           Data.Typeable
import qualified Data.HashMap.Lazy as H

import           Polysemy
import           Polysemy.EndState
import           Polysemy.Fixpoint
import           Polysemy.State

-- import Text.Pretty.Simple (pShow)

import qualified Waargonaut.Decode as D

import Relude ( error )

-- import qualified Debug.Trace as T
-- import qualified Data.Text.Lazy as T

type DecompressState = HashMap Int (Dynamic, Text)

type DecompressC r =
  (Members '[Fixpoint, State DecompressState,
  EndState DecompressState, Final Identity] r) -- , HasCallStack)

addNode :: Member (State DecompressState) r => Int -> (Dynamic, Text) -> Sem r ()
addNode k v = modify $ H.insert k v

getNode :: Members '[State DecompressState, EndState DecompressState] r => Int -> Sem r (Dynamic, Text)
getNode k = do
  s <- get
  case H.lookup k s of
    Just x -> pure x
    _      -> do
      es <- getEndState
      let Just x = H.lookup k es
        in pure x

isReferenceNode :: Monad m => D.JCurs -> D.DecodeResult m Bool
isReferenceNode curs = do
  ty <- D.fromKeyOptional "Node_Type" D.text curs
  pure $ isNothing ty

currentNodeType :: DecompressC r => D.JCurs -> D.DecodeResult (Sem r) Text
currentNodeType curs = do
  o   <- D.down curs
  ref <- isReferenceNode o
  id' <- D.fromKey "Node_ID" D.int o
  -- s <- lift $ get
  -- T.traceM $ "state: " <> T.unpack (pShow s)
  -- T.traceM $ "getting node type of: " <> show id' <> ", cs: " <> prettyCallStack callStack
  if ref
    then lift $ do
      ~(_, ty) <- getNode id'
      pure ty
    else D.fromKey "Node_Type" D.text o

fromJustMsg :: Text -> Maybe a -> a
fromJustMsg _ (Just a) = a
fromJustMsg msg _ = error msg

tryParseVal
  :: forall r b. (Typeable b, DecompressC r)
  => (D.JCurs -> D.DecodeResult (Sem r) b)
  -> D.JCurs
  -> D.DecodeResult (Sem r) b
tryParseVal f curs = do
  o   <- D.down curs
  ref <- isReferenceNode o
  id' <- D.fromKey "Node_ID" D.int o
  -- traceM $ "parsing node: " <> show id' <> ", cs: " <> prettyCallStack callStack
  if ref
    then lift $ do
      ~(n, _) <- getNode id'
      pure $ fromJustMsg ("node: " <> show id'
                          <> ", wanted type: " <> show (typeRep $ Proxy @b)
                          <> ", was type: " <> show (dynTypeRep n)) $ fromDynamic @b n
    else do
      b <- f curs
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
  Sem '[EndState DecompressState, State DecompressState,
  Fixpoint, Final Identity] a
  -> a
runDecompressor =
    runIdentity
  . runFinal
  . fixpointToFinal @Identity
  . evalLazyState @DecompressState H.empty
  . runEndState @DecompressState
