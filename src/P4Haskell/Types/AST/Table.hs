-- |
module P4Haskell.Types.AST.Table (
  P4Table (..),
  parseP4Table,
  Key (..),
  parseKey,
  KeyElement (..),
  parseKeyElement,
  ExpressionValue (..),
  parseExpressionValue,
  TableEntry (..),
  parseTableEntry,
) where

import Control.Lens
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Generics.Product.Fields ()
import Data.Generics.Sum.Typed
import P4Haskell.Types.AST.ActionList
import P4Haskell.Types.AST.Annotation
import P4Haskell.Types.AST.Core
import P4Haskell.Types.AST.DecompressJSON
import P4Haskell.Types.AST.Expression
import P4Haskell.Types.AST.Path
import P4Haskell.Types.AST.Types
import Polysemy hiding (Member)
import Relude
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D

data P4Table = P4Table
  { name :: Text
  , annotations :: [Annotation]
  , keys :: [KeyElement]
  , actions :: ActionList
  , defaultAction :: Maybe MethodCallExpression
  , entries :: Maybe [TableEntry]
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

ensure :: MonadError D.DecodeError m => Text -> Maybe a -> m a
ensure msg = maybe (throwError $ D.ParseFailed msg) pure

parseP4Table :: DecompressC r => D.Decoder (Sem r) P4Table
parseP4Table = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  name <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  properties <-
    D.fromKey
      "properties"
      ( parseNestedObject
          "properties"
          (parseIndexedVector parseProperty)
      )
      o
  keys <-
    ensure "table does not have a 'key' value" $
      properties ^? #map . ix "key" . #value . _Typed @Key . #keyElements
  actions <-
    ensure "table does not have a 'actions' value" $
      properties ^? #map . ix "actions" . #value . _Typed @ActionList
  let defaultAction =
        properties
          ^? #map . ix "default_action"
            . #value
            . _Typed @ExpressionValue
            . #value
            . _Typed @MethodCallExpression
  let entries = properties ^? #map . ix "entries" . #value . _Typed @EntriesList . #entries
  pure $ P4Table name annotations keys actions defaultAction entries

data PropertyValue
  = PropertyValue'Key Key
  | PropertyValue'ActionList ActionList
  | PropertyValue'EntriesList EntriesList
  | PropertyValue'ExpressionValue ExpressionValue
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

propertyValueDecoder :: DecompressC r => D.Decoder (Sem r) PropertyValue
propertyValueDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "Key" -> (_Typed @Key #) <$> tryDecoder parseKey c
    "ActionList" -> (_Typed @ActionList #) <$> tryDecoder parseActionList c
    "EntriesList" -> (_Typed @EntriesList #) <$> tryDecoder parseEntriesList c
    "ExpressionValue" -> (_Typed @ExpressionValue #) <$> tryDecoder parseExpressionValue c
    _ -> throwError . D.ParseFailed $ "invalid node type for PropertyValue: " <> nodeType

data Property = Property
  { name :: Text
  , annotations :: [Annotation]
  , value :: PropertyValue
  , isConstant :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

parseProperty :: DecompressC r => D.Decoder (Sem r) Property
parseProperty = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  name <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  value <- D.fromKey "value" propertyValueDecoder o
  isConstant <- D.fromKey "isConstant" D.bool o
  pure $ Property name annotations value isConstant

newtype Key = Key
  { keyElements :: [KeyElement]
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

parseKey :: DecompressC r => D.Decoder (Sem r) Key
parseKey = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  elems <- D.fromKey "keyElements" (parseVector parseKeyElement) o
  pure $ Key elems

data KeyElement = KeyElement
  { annotations :: [Annotation]
  , expression :: Expression
  , matchType :: Path
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

parseKeyElement :: DecompressC r => D.Decoder (Sem r) KeyElement
parseKeyElement = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  expression <- D.fromKey "expression" expressionDecoder o
  matchType <- D.fromKey "matchType" parsePathExpression o
  pure $ KeyElement annotations expression (matchType ^. #path)

newtype ExpressionValue = ExpressionValue
  { value :: Expression
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

parseExpressionValue :: DecompressC r => D.Decoder (Sem r) ExpressionValue
parseExpressionValue = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  value <- D.fromKey "expression" expressionDecoder o
  pure $ ExpressionValue value

data EntriesList = EntriesList
  { entries :: [TableEntry]
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

parseEntriesList :: DecompressC r => D.Decoder (Sem r) EntriesList
parseEntriesList = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  entries <- D.fromKey "entries" (parseVector parseTableEntry) o
  pure $ EntriesList entries

data TableEntry = TableEntry
  { keys :: [SelectKey]
  , action :: MethodCallExpression
  , type_ :: P4Type
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable)

parseTableEntry :: DecompressC r => D.Decoder (Sem r) TableEntry
parseTableEntry = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  type_ <- D.fromKey "keys" (parseNestedObject "type" p4TypeDecoder) o
  keys <- D.fromKey "keys" (parseNestedObject "components" (parseVector selectKeyDecoder)) o
  action <- D.fromKey "action" expressionDecoder o
  let action' = action ^?! _Typed @MethodCallExpression
  pure $ TableEntry keys action' type_
