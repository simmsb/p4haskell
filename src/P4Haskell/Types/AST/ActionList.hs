module P4Haskell.Types.AST.ActionList
  ( ActionList (..),
    ActionListElement (..),
    parseActionList,
    parseActionListElement,
  )
where

import Control.Lens
import Data.Generics.Sum.Typed
import P4Haskell.Types.AST.Annotation
import P4Haskell.Types.AST.Core
import P4Haskell.Types.AST.DecompressJSON
import P4Haskell.Types.AST.Expression
import P4Haskell.Types.AST.MapVec
import Polysemy
import qualified Waargonaut.Decode as D
import Prelude

newtype ActionList = ActionList
  { actions :: MapVec Text ActionListElement
  }
  deriving (Show, Generic, Eq, Hashable)

parseActionList :: DecompressC r => D.Decoder (Sem r) ActionList
parseActionList = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  elems <- D.fromKey "actionList" (parseIndexedVector parseActionListElement) o
  pure $ ActionList elems

data ActionListElement = ActionListElement
  { annotations :: [Annotation],
    expression :: MethodCallExpression
  }
  deriving (Show, Generic, Eq, Hashable)

parseActionListElement :: DecompressC r => D.Decoder (Sem r) ActionListElement
parseActionListElement = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  expression <- D.fromKey "expression" expressionDecoder o
  let expression' = expression ^?! _Typed @MethodCallExpression
  pure $ ActionListElement annotations expression'
