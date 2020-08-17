module P4Haskell.Types.AST.Method
    ( Method(..)
    , parseMethod ) where

import           P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Types

import           Polysemy

import qualified Waargonaut.Decode                  as D

data Method = Method
  { annotations :: [Annotation]
  , name        :: Text
  , type_       :: TypeMethod
  }
  deriving ( Show, Generic, Eq, Hashable )

parseMethod :: DecompressC r => D.Decoder (Sem r) Method
parseMethod = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  name        <- D.fromKey "name" D.text o
  type_       <- D.fromKey "type" parseTypeMethod o
  pure $ Method annotations name type_
