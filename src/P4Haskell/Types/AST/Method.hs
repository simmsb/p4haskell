module P4Haskell.Types.AST.Method
    ( Method(..)
    , parseMethod ) where

import           P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Types

import qualified Waargonaut.Decode                  as D

data Method = Method
  { annotations :: [Annotation]
  , name        :: Text
  , type_       :: TypeMethod
  }
  deriving ( Show, Generic )

parseMethod :: DecompressC r => D.Decoder (Sem r) Method
parseMethod = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  let annotations = []
  name        <- D.fromKey "name" D.text o
  type_       <- D.fromKey "type" parseTypeMethod o
  pure $ Method annotations name type_
