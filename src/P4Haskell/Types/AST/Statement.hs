module P4Haskell.Types.AST.Statement where

import           Control.Monad.Error.Class          ( throwError )

import           Data.Generics.Sum.Typed

import           P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Expression
import           P4Haskell.Types.AST.Types

import           Prelude                            hiding ( Member )

import qualified Waargonaut.Decode                  as D
import qualified Waargonaut.Decode.Error            as D

data Statement
  = MethodCallStatement'Statement MethodCallStatement
  | AssignmentStatement'Statement AssignmentStatement
  | DeclarationVariable'Statement DeclarationVariable
  | IfStatement'Statement IfStatement
  | BlockStatement'Statement BlockStatement
  deriving ( Show, Generic )

statementDecoderInner :: DecompressC r => D.JCurs -> D.DecodeResult (Sem r) (Maybe Statement)
statementDecoderInner c = do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "MethodCallStatement" -> Just . (_Typed @MethodCallStatement #) <$> D.focus parseMethodCallStatement c
    "AssignmentStatement" -> Just . (_Typed @AssignmentStatement #) <$> D.focus parseAssignmentStatement c
    "Declaration_Variable" -> Just . (_Typed @DeclarationVariable #) <$> D.focus parseDeclarationVariable c
    "IfStatement"         -> Just . (_Typed @IfStatement #) <$> D.focus parseIfStatement c
    "BlockStatement"      -> Just . (_Typed @BlockStatement #) <$> D.focus parseBlockStatement c
    _ -> pure Nothing

statementDecoder :: DecompressC r => D.Decoder (Sem r) Statement
statementDecoder = D.withCursor $ \c -> do
  res <- statementDecoderInner c
  case res of
    Just x  -> pure x
    Nothing -> do
        o <- D.down c
        nodeType <- D.fromKey "Node_Type" D.text o
        throwError . D.ParseFailed $ "invalid node type for Statement: " <> nodeType

newtype MethodCallStatement = MethodCallStatement
  { methodCall :: MethodCallExpression
  }
  deriving ( Show, Generic )

parseMethodCallStatement :: DecompressC r => D.Decoder (Sem r) MethodCallStatement
parseMethodCallStatement = D.withCursor . tryParseVal $ \c -> do
  o          <- D.down c
  methodCall <- D.fromKey "methodCall" parseMethodCallExpression o
  pure $ MethodCallStatement methodCall

data AssignmentStatement = AssignmentStatement
  { left  :: Expression
  , right :: Expression
  }
  deriving ( Show, Generic )

parseAssignmentStatement :: DecompressC r => D.Decoder (Sem r) AssignmentStatement
parseAssignmentStatement = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  left  <- D.fromKey "left" expressionDecoder o
  right <- D.fromKey "right" expressionDecoder o
  pure $ AssignmentStatement left right

data DeclarationVariable = DeclarationVariable
  { name :: Text
  , annotations :: [Annotation]
  , type_ :: P4Type
  , initializer :: Expression
  }
  deriving ( Show, Generic )

parseDeclarationVariable :: DecompressC r => D.Decoder (Sem r) DeclarationVariable
parseDeclarationVariable = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  type_       <- D.fromKey "type" parseP4Type o
  initializer <- D.fromKey "initializer" expressionDecoder o
  pure $ DeclarationVariable name annotations type_ initializer

data IfStatement = IfStatement
  { condition :: Expression
  , ifTrue    :: Statement
  , ifFalse   :: Maybe Statement
  }
  deriving ( Show, Generic )

parseIfStatement :: DecompressC r => D.Decoder (Sem r) IfStatement
parseIfStatement = D.withCursor . tryParseVal $ \c -> do
  o         <- D.down c
  condition <- D.fromKey "condition" expressionDecoder o
  ifTrue    <- D.fromKey "ifTrue" statementDecoder o
  ifFalse   <- D.fromKeyOptional "ifFalse" statementDecoder o
  pure $ IfStatement condition ifTrue ifFalse

data BlockStatement = BlockStatement
  { annotations :: [Annotation]
  , components  :: [StatOrDecl]
  }
  deriving ( Show, Generic )

parseBlockStatement :: DecompressC r => D.Decoder (Sem r) BlockStatement
parseBlockStatement = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  components  <- D.fromKey "components" (parseVector statOrDeclDecoder) o
  pure $ BlockStatement annotations components

data StatOrDecl
  = DeclarationVariable'StatOrDecl DeclarationVariable
  | Statement'StatOrDecl Statement
  deriving ( Show, Generic )

statOrDeclDecoder :: DecompressC r => D.Decoder (Sem r) StatOrDecl
statOrDeclDecoder = D.withCursor $ \c -> do
  res <- statementDecoderInner c
  case res of
    Just x  -> pure $ (_Typed @Statement #) x
    Nothing -> do
      o <- D.down c
      nodeType <- D.fromKey "Node_Type" D.text o

      case nodeType of
        "DeclarationVariable" -> (_Typed @DeclarationVariable #) <$> D.focus parseDeclarationVariable c
        _ -> throwError . D.ParseFailed $ "invalid node type for StatOrDecl: " <> nodeType