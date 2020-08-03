module P4Haskell.Types.AST.ActionList
    ( ActionList(..)
    , ActionListElement(..)
    , parseActionList
    , parseActionListElement ) where

import           P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Expression

import           Prelude

import           Polysemy

import qualified Waargonaut.Decode                  as D

newtype ActionList = ActionList
  { actions :: [ActionListElement]
  }
  deriving ( Show, Generic )

parseActionList :: DecompressC r => D.Decoder (Sem r) ActionList
parseActionList = D.withCursor . tryParseVal $ \c -> do
  o       <- D.down c
  elems   <- D.fromKey "actionList" (parseVector parseActionListElement) o
  pure $ ActionList elems

data ActionListElement = ActionListElement
  { annotations :: [Annotation]
  , expression  :: Expression
  }
  deriving ( Show, Generic )

parseActionListElement :: DecompressC r => D.Decoder (Sem r) ActionListElement
parseActionListElement = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  expression  <- D.fromKey "expression" expressionDecoder o
  pure $ ActionListElement annotations expression
