-- |
module P4Haskell.Types.AST.Table
  ( P4Table (..),
    parseP4Table,
    PropertyValue (..),
    propertyValueDecoder,
    Property (..),
    parseProperty,
    Key (..),
    parseKey,
    KeyElement (..),
    parseKeyElement,
    ExpressionValue (..),
    parseExpressionValue,
  )
where

import Control.Monad.Error.Class (throwError)
import Data.Generics.Sum.Typed
import P4Haskell.Types.AST.ActionList
import P4Haskell.Types.AST.Annotation
import P4Haskell.Types.AST.Core
import P4Haskell.Types.AST.DecompressJSON
import P4Haskell.Types.AST.Expression
import P4Haskell.Types.AST.MapVec
import Polysemy hiding (Member)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D

data P4Table = P4Table
  { name        :: Text
  , annotations :: [Annotation]
  , properties  :: MapVec Text Property
  }
  deriving ( Show, Generic, Eq, Hashable )

parseP4Table :: DecompressC r => D.Decoder (Sem r) P4Table
parseP4Table = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  properties  <- D.fromKey "properties"
    (parseNestedObject "properties"
     (parseIndexedVector parseProperty)) o
  pure $ P4Table name annotations properties

data PropertyValue
  = PropertyValue'Key Key
  | PropertyValue'ActionList ActionList
  | PropertyValue'ExpressionValue ExpressionValue
  deriving ( Show, Generic, Eq, Hashable )

propertyValueDecoder :: DecompressC r => D.Decoder (Sem r) PropertyValue
propertyValueDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "Key"             -> (_Typed @Key #)             <$> tryDecoder parseKey c
    "ActionList"      -> (_Typed @ActionList #)      <$> tryDecoder parseActionList c
    "ExpressionValue" -> (_Typed @ExpressionValue #) <$> tryDecoder parseExpressionValue c
    _ -> throwError . D.ParseFailed $ "invalid node type for PropertyValue: " <> nodeType

data Property = Property
  { name        :: Text
  , annotations :: [Annotation]
  , value       :: PropertyValue
  , isConstant  :: Bool
  }
  deriving ( Show, Generic, Eq, Hashable )

parseProperty :: DecompressC r => D.Decoder (Sem r) Property
parseProperty = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  value       <- D.fromKey "value" propertyValueDecoder o
  isConstant  <- D.fromKey "isConstant" D.bool o
  pure $ Property name annotations value isConstant

newtype Key = Key
  { keyElements :: [KeyElement]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseKey :: DecompressC r => D.Decoder (Sem r) Key
parseKey = D.withCursor . tryParseVal $ \c -> do
  o        <- D.down c
  elems   <- D.fromKey "keyElements" (parseVector parseKeyElement) o
  pure $ Key elems

data KeyElement = KeyElement
  { annotations :: [Annotation]
  , expression  :: Expression
  , matchType   :: Expression
  }
  deriving ( Show, Generic, Eq, Hashable )

parseKeyElement :: DecompressC r => D.Decoder (Sem r) KeyElement
parseKeyElement = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  expression  <- D.fromKey "expression" expressionDecoder o
  matchType   <- D.fromKey "matchType" expressionDecoder o
  pure $ KeyElement annotations expression matchType

newtype ExpressionValue = ExpressionValue
  { value :: Expression
  }
  deriving ( Show, Generic, Eq, Hashable )

parseExpressionValue :: DecompressC r => D.Decoder (Sem r) ExpressionValue
parseExpressionValue = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  value <- D.fromKey "expression" expressionDecoder o
  pure $ ExpressionValue value
