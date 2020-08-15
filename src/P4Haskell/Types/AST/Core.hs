-- | Core stuff for parsing the P4 AST
module P4Haskell.Types.AST.Core
    ( parseVectorPure
    , parseVector
    , parseIndexedVector
    , parseNestedObject ) where

import           P4Haskell.Types.AST.DecompressJSON

import           Prelude

import           Polysemy

import qualified Waargonaut.Decode              as D

-- ^ Like 'parseVector' but doesn't perform lookups on the decompression state.
parseVectorPure :: Monad m => D.Decoder m a -> D.Decoder m [a]
parseVectorPure inner = D.withCursor $ \c -> do
  o <- D.down c
  D.fromKey "vec" (D.list inner) o

parseVector :: (Typeable a, DecompressC r)
            => D.Decoder (Sem r) a
            -> D.Decoder (Sem r) [a]
parseVector inner = D.withCursor . tryParseVal $ \c -> do
    o <- D.down c
    D.fromKey "vec" (D.list inner) o

parseIndexedVector :: (Typeable a, DecompressC r)
            => D.Decoder (Sem r) a
            -> D.Decoder (Sem r) (HashMap Text a)
parseIndexedVector inner = D.withCursor . tryParseVal $ \c -> do
    o <- D.down c
    -- even though we don't use 'vec', sometimes the nodes only exist in the vec
    -- and not the declarations
    _ <- D.fromKey "vec" (D.list inner) o
    fromList <$> D.fromKey "declarations" (D.objectAsKeyValues D.text inner) o

parseNestedObject
  :: (Typeable a, DecompressC r)
  => Text
  -> D.Decoder (Sem r) a
  -> D.Decoder (Sem r) a
parseNestedObject key inner = D.withCursor . tryParseVal $ \c -> do
    o <- D.down c
    D.fromKey key inner o
