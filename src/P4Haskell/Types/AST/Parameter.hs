-- | P4 Parameters
module P4Haskell.Types.AST.Parameter
    ( Parameter(..)
    , parseParameter ) where

import           P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.Types
import           P4Haskell.Types.AST.DecompressJSON

import           Prelude

import           Polysemy

import qualified Waargonaut.Decode                  as D

data Parameter = Parameter
  { annotations :: [Annotation]
  , direction   :: Text
  , name        :: Text
  , type_       :: P4Type
  }
  deriving ( Show, Generic )

parseParameter :: DecompressC r => D.Decoder (Sem r) Parameter
parseParameter = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  direction   <- D.fromKey "direction" D.text o
  name        <- D.fromKey "name" D.text o
  type_       <- D.fromKey "type" p4TypeDecoder o
  pure $ Parameter annotations direction name type_
