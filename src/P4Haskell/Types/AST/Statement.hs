module P4Haskell.Types.AST.Statement where

import Control.Lens
import Control.Monad.Error.Class (throwError)
import Data.Generics.Sum.Typed
import P4Haskell.Types.AST.Annotation
import P4Haskell.Types.AST.Core
import P4Haskell.Types.AST.DecompressJSON
import P4Haskell.Types.AST.Expression
import P4Haskell.Types.AST.Types
import Polysemy
import Relude
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D

data Statement
  = MethodCallStatement'Statement MethodCallStatement
  | AssignmentStatement'Statement AssignmentStatement
  | DeclarationVariable'Statement DeclarationVariable
  | IfStatement'Statement IfStatement
  deriving stock ( Show, Generic, Eq)
  deriving anyclass ( Hashable )

statementDecoderInner :: DecompressC r => D.JCurs -> D.DecodeResult (Sem r) (Maybe Statement)
statementDecoderInner c = do
  nodeType <- currentNodeType c

  case nodeType of
    "MethodCallStatement"  -> Just . (_Typed @MethodCallStatement #) <$> tryDecoder parseMethodCallStatement c
    "AssignmentStatement"  -> Just . (_Typed @AssignmentStatement #) <$> tryDecoder parseAssignmentStatement c
    "Declaration_Variable" -> Just . (_Typed @DeclarationVariable #) <$> tryDecoder parseDeclarationVariable c
    "IfStatement"          -> Just . (_Typed @IfStatement #)         <$> tryDecoder parseIfStatement c
    _ -> pure Nothing

statementDecoder :: DecompressC r => D.Decoder (Sem r) Statement
statementDecoder = D.withCursor $ \c -> do
  res <- statementDecoderInner c
  case res of
    Just x  -> pure x
    Nothing -> do
        nodeType <- currentNodeType c
        throwError . D.ParseFailed $ "invalid node type for Statement: " <> nodeType

newtype MethodCallStatement = MethodCallStatement
  { methodCall :: MethodCallExpression
  }
  deriving stock ( Show, Generic, Eq)
  deriving anyclass ( Hashable )

parseMethodCallStatement :: DecompressC r => D.Decoder (Sem r) MethodCallStatement
parseMethodCallStatement = D.withCursor . tryParseVal $ \c -> do
  o          <- D.down c
  methodCall <- D.fromKey "methodCall" parseMethodCallExpression o
  pure $ MethodCallStatement methodCall

data AssignmentStatement = AssignmentStatement
  { left  :: Expression
  , right :: Expression
  }
  deriving stock ( Show, Generic, Eq)
  deriving anyclass ( Hashable )

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
  , initializer :: Maybe Expression
  }
  deriving stock ( Show, Generic, Eq)
  deriving anyclass ( Hashable )

parseDeclarationVariable :: DecompressC r => D.Decoder (Sem r) DeclarationVariable
parseDeclarationVariable = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  type_       <- D.fromKey "type" p4TypeDecoder o
  initializer <- D.fromKeyOptional"initializer" expressionDecoder o
  pure $ DeclarationVariable name annotations type_ initializer

data IfStatement = IfStatement
  { condition :: Expression
  , ifTrue    :: Statement
  , ifFalse   :: Maybe Statement
  }
  deriving stock ( Show, Generic, Eq)
  deriving anyclass ( Hashable )

parseIfStatement :: DecompressC r => D.Decoder (Sem r) IfStatement
parseIfStatement = D.withCursor . tryParseVal $ \c -> do
  o         <- D.down c
  condition <- D.fromKey "condition" expressionDecoder o
  ifTrue    <- D.fromKey "ifTrue" statementDecoder o
  ifFalse   <- D.fromKeyOptional "ifFalse" statementDecoder o
  pure $ IfStatement condition ifTrue ifFalse

data BlockStatement = BlockStatement
  { annotations :: [Annotation]
  , components  :: [Statement]
  }
  deriving stock ( Show, Generic, Eq)
  deriving anyclass ( Hashable )

parseBlockStatement :: DecompressC r => D.Decoder (Sem r) BlockStatement
parseBlockStatement = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  components  <- D.fromKey "components" (parseVector statementDecoder) o
  pure $ BlockStatement annotations components

-- data StatOrDecl
--   = DeclarationVariable'StatOrDecl DeclarationVariable
--   | Statement'StatOrDecl Statement
--   deriving stock ( Show, Generic, Eq)
--   deriving anyclass ( Hashable )

-- statOrDeclDecoder :: DecompressC r => D.Decoder (Sem r) StatOrDecl
-- statOrDeclDecoder = D.withCursor $ \c -> do
--   res <- statementDecoderInner c
--   case res of
--     Just x  -> pure $ (_Typed @Statement #) x
--     Nothing -> do
--       nodeType <- currentNodeType c

--       case nodeType of
--         "DeclarationVariable" -> (_Typed @DeclarationVariable #) <$> tryDecoder parseDeclarationVariable c
--         _ -> throwError . D.ParseFailed $ "invalid node type for StatOrDecl: " <> nodeType
