-- | Represents the P4 AST in haskellmodule P4Haskell.Types.AST
module P4Haskell.Types.AST.AST
    ( astDecoder ) where

import           Control.Monad.Error.Class          ( throwError )

import           Data.Generics.Sum.Typed

import           P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DeclarationID
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Expression
import           P4Haskell.Types.AST.Statement
import           P4Haskell.Types.AST.Method
import           P4Haskell.Types.AST.Parameter
import           P4Haskell.Types.AST.Types

import           Prelude                            hiding ( Member )

import           Waargonaut
import qualified Waargonaut.Decode                  as D
import qualified Waargonaut.Decode.Error            as D


astDecoder :: DecompressC r => D.Decoder (Sem r) P4Program
astDecoder = D.withCursor $ \c -> do
  o       <- D.down c
  objects <- D.fromKey "objects" (parseVectorPure nodeDecoder) o
  pure $ P4Program objects

newtype P4Program = P4Program
  { objects :: [Node]
  }
  deriving ( Show, Generic )

data Node
  = TypeError'Node TypeError
  | TypeExtern'Node TypeExtern
  | TypeParser'Node TypeParser
  | TypeControl'Node TypeControl
  | TypePackage'Node TypePackage
  | TypeTypedef'Node TypeTypedef
  | TypeHeader'Node TypeHeader
  | TypeStruct'Node TypeStruct
  | Method'Node Method
  | Member'Node Member
  | DeclarationMatchKind'Node DeclarationMatchKind
  | P4Parser'Node P4Parser
  | PathExpression'Node PathExpression
  | P4Control'Node P4Control
  | BoolLiteral'Node BoolLiteral
  | Key'Node Key
  | ActionList'Node ActionList
  | MethodCallExpression'Node MethodCallExpression
  | ExpressionValue'Node ExpressionValue
  | ConstructorCallExpression'Node ConstructorCallExpression
  | Constant'Node Constant
  deriving ( Show, Generic )

nodeDecoder :: DecompressC r => D.Decoder (Sem r) Node
nodeDecoder = D.withCursor $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "Type_Error"                -> (_Typed @TypeError #) <$> D.focus parseTypeError c
    "Type_Extern"               -> (_Typed @TypeExtern #) <$> D.focus parseTypeExtern c
    "Type_Parser"               -> (_Typed @TypeParser #) <$> D.focus parseTypeParser c
    "Type_Control"              -> (_Typed @TypeControl #) <$> D.focus parseTypeControl c
    "Type_Package"              -> (_Typed @TypePackage #) <$> D.focus parseTypePackage c
    "Type_Typedef"              -> (_Typed @TypeTypedef #) <$> D.focus parseTypeTypedef c
    "Type_Header"               -> (_Typed @TypeHeader #) <$> D.focus parseTypeHeader c
    "Type_Struct"               -> (_Typed @TypeStruct #) <$> D.focus parseTypeStruct c
    "Method"                    -> (_Typed @Method #) <$> D.focus parseMethod c
    "Member"                    -> (_Typed @Member #) <$> D.focus parseMember c
    "Declaration_MatchKind"     -> (_Typed @DeclarationMatchKind #) <$> D.focus parseDeclarationMatchKind c
    "P4Parser"                  -> (_Typed @P4Parser #) <$> D.focus parseP4Parser c
    "PathExpression"            -> (_Typed @PathExpression #) <$> D.focus parsePathExpression c
    "P4Control"                 -> (_Typed @P4Control #) <$> D.focus parseP4Control c
    "BoolLiteral"               -> (_Typed @BoolLiteral #) <$> D.focus parseBoolLiteral c
    "Key"                       -> (_Typed @Key #) <$> D.focus parseKey c
    "ActionList"                -> (_Typed @ActionList #) <$> D.focus parseActionList c
    "MethodCallExpression"      -> (_Typed @MethodCallExpression #) <$> D.focus parseMethodCallExpression c
    "ExpressionValue"           -> (_Typed @ExpressionValue #) <$> D.focus parseExpressionValue c
    "ConstructorCallExpression" -> (_Typed @ConstructorCallExpression #) <$> D.focus parseConstructorCallExpression c
    "Constant"                  -> (_Typed @Constant #) <$> D.focus parseConstant c
    _ -> throwError . D.ParseFailed $ "invalid node type for Node: " <> nodeType

data P4Action = P4Action
  { name        :: Text
  , annotations :: [Annotation]
  , parameters  :: [Parameter]
  , body        :: BlockStatement
  }
  deriving ( Show, Generic )

parseP4Action :: DecompressC r => D.Decoder (Sem r) P4Action
parseP4Action = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  parameters  <- D.fromKey "parameters"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  body        <- D.fromKey "body" parseBlockStatement o
  pure $ P4Action name annotations parameters body

data Declaration
  = P4Action'Declaration P4Action
  | P4Table'Declaration P4Table
  deriving ( Show, Generic )

declarationDecoder :: DecompressC r => D.Decoder (Sem r) Declaration
declarationDecoder = D.withCursor $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "P4Action" -> (_Typed @P4Action #) <$> D.focus parseP4Action c
    "P4Table"  -> (_Typed @P4Table #) <$> D.focus parseP4Table c
    _ -> throwError . D.ParseFailed $ "invalid node type for Declaration: " <> nodeType

data ParserState = ParserState
  { annotations :: [Annotation]
  , components  :: [StatOrDecl]
  }
  deriving ( Show, Generic )

parseParserState :: DecompressC r => D.Decoder (Sem r) ParserState
parseParserState = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  components  <- D.fromKey "components" (parseVector statOrDeclDecoder) o
  pure $ ParserState annotations components

data P4Parser = P4Parser
  { name              :: Text
  , type_             :: P4Type
  , constructorParams :: [Parameter]
  , parserLocals      :: [Declaration]
  , states            :: [ParserState]
  }
  deriving ( Show, Generic )

parseP4Parser :: DecompressC r => D.Decoder (Sem r) P4Parser
parseP4Parser = D.withCursor . tryParseVal $ \c -> do
  o                 <- D.down c
  name              <- D.fromKey "name" D.text o
  type_             <- D.fromKey "type" parseP4Type o
  constructorParams <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  parserLocals      <- D.fromKey "parserLocals" (parseVector declarationDecoder) o
  states            <- D.fromKey "states" (parseVector parseParserState) o
  pure $ P4Parser name type_ constructorParams parserLocals states

newtype Attribute = Attribute Json
  deriving ( Show, Generic )

newtype DeclarationMatchKind = DeclarationMatchKind
  { members :: [DeclarationID]
  }
  deriving ( Show, Generic )

parseDeclarationMatchKind :: DecompressC r => D.Decoder (Sem r) DeclarationMatchKind
parseDeclarationMatchKind = D.withCursor . tryParseVal $ \c -> do
  o       <- D.down c
  members <- D.fromKey "members" (parseVector parseDeclarationID) o
  pure $ DeclarationMatchKind members

data P4Control = P4Control
  { name              :: Text
  , type_             :: P4Type
  , constructorParams :: [Parameter]
  , controlLocals     :: [Declaration]
  , body              :: BlockStatement
  }
  deriving ( Show, Generic )

parseP4Control :: DecompressC r => D.Decoder (Sem r) P4Control
parseP4Control = D.withCursor . tryParseVal $ \c -> do
  o                 <- D.down c
  name              <- D.fromKey "name" D.text o
  type_             <- D.fromKey "type" parseP4Type o
  constructorParams <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  controlLocals      <- D.fromKey "controlLocals" (parseVector declarationDecoder) o
  body              <- D.fromKey "body" parseBlockStatement o
  pure $ P4Control name type_ constructorParams controlLocals body

data P4Table = P4Table
  { name        :: Text
  , annotations :: [Annotation]
  , properties  :: [Property]
  }
  deriving ( Show, Generic )

parseP4Table :: DecompressC r => D.Decoder (Sem r) P4Table
parseP4Table = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  properties  <- D.fromKey "properties"
    (parseNestedObject "properties"
     (parseVector parseProperty)) o
  pure $ P4Table name annotations properties

data Property = Property
  { name        :: Text
  , annotations :: [Annotation]
  , value       :: Node
  , isConstant  :: Bool
  }
  deriving ( Show, Generic )

parseProperty :: DecompressC r => D.Decoder (Sem r) Property
parseProperty = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  value       <- D.fromKey "value" nodeDecoder o
  isConstant  <- D.fromKey "isConstant" D.bool o
  pure $ Property name annotations value isConstant

newtype Key = Key
  { keyElements :: [KeyElement]
  }
  deriving ( Show, Generic )

parseKey :: DecompressC r => D.Decoder (Sem r) Key
parseKey = D.withCursor . tryParseVal $ \c -> do
  o        <- D.down c
  elems   <- D.fromKey "keyElements" (parseVector parseKeyElement) o
  pure $ Key elems

data KeyElement = KeyElement
  { annotations :: [Annotation]
  , expression  :: Node
  , matchType   :: Node
  }
  deriving ( Show, Generic )

parseKeyElement :: DecompressC r => D.Decoder (Sem r) KeyElement
parseKeyElement = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  expression  <- D.fromKey "expression" nodeDecoder o -- TODO: constrain
  matchType   <- D.fromKey "expression" nodeDecoder o -- TODO: constrain
  pure $ KeyElement annotations expression matchType

newtype ActionList = ActionList
  { actions :: [ActionListElement]
  }
  deriving ( Show, Generic )

parseActionList :: DecompressC r => D.Decoder (Sem r) ActionList
parseActionList = D.withCursor . tryParseVal $ \c -> do
  o        <- D.down c
  elems   <- D.fromKey "actionList" (parseVector parseActionListElement) o
  pure $ ActionList elems

data ActionListElement = ActionListElement
  { annotations :: [Annotation]
  , expression  :: Node
  }
  deriving ( Show, Generic )

parseActionListElement :: DecompressC r => D.Decoder (Sem r) ActionListElement
parseActionListElement = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  expression  <- D.fromKey "expression" nodeDecoder o -- TODO: constrain
  pure $ ActionListElement annotations expression

newtype ExpressionValue = ExpressionValue
  { value :: Node
  }
  deriving ( Show, Generic )

parseExpressionValue :: DecompressC r => D.Decoder (Sem r) ExpressionValue
parseExpressionValue = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  value <- D.fromKey "expression" nodeDecoder o
  pure $ ExpressionValue value


