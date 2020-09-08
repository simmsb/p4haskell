-- | Represents the P4 AST in haskellmodule P4Haskell.Types.AST
module P4Haskell.Types.AST.AST where

import           Control.Monad.Error.Class          ( throwError )

import qualified Generics.SOP as GS

import           Data.Generics.Sum.Typed

import           P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DeclarationID
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Expression
import           P4Haskell.Types.AST.Statement
import           P4Haskell.Types.AST.Method
import           P4Haskell.Types.AST.MapVec
import           P4Haskell.Types.AST.SelectKey
import           P4Haskell.Types.AST.Parameter
import           P4Haskell.Types.AST.Types
import           P4Haskell.Types.AST.Table

import           Prelude

import           Polysemy                           hiding ( Member )

import qualified Waargonaut.Decode                  as D
import qualified Waargonaut.Decode.Error            as D


astDecoder :: DecompressC r => D.Decoder (Sem r) P4Program
astDecoder = D.withCursor $ \c -> do
  o       <- D.down c
  objects <- D.fromKey "node" (parseNestedObject "objects"
                               (parseVectorPure topLevelDecoder)) o
  pure $ P4Program objects

newtype P4Program = P4Program
  { objects :: [TopLevel]
  }
  deriving ( Show, Generic, Eq, Hashable )

data TopLevel
  = TypeDecl'TopLevelTypeDecl TopLevelTypeDecl
  | Method'TopLevel Method
  | DeclarationMatchKind'TopLevel DeclarationMatchKind
  | TypeParser'TopLevel TypeParser
  | TypeControl'TopLevel TypeControl
  | TypePackage'TopLevel TypePackage
  | P4Parser'TopLevel P4Parser
  | P4Control'TopLevel P4Control
  | DeclarationInstance'TopLevel DeclarationInstance
  deriving ( Show, Generic, GS.Generic, Eq, Hashable )

topLevelDecoder :: DecompressC r => D.Decoder (Sem r) TopLevel
topLevelDecoder = D.withCursor $ \c -> do
  res <- topLevelTypeDeclDecoderInner c
  case res of
    Just x  -> pure $ (_Typed @TopLevelTypeDecl #) x
    Nothing -> do
      nodeType <- currentNodeType c

      case nodeType of
        "Method"                    -> (_Typed @Method #)               <$> tryDecoder parseMethod c
        "Declaration_MatchKind"     -> (_Typed @DeclarationMatchKind #) <$> tryDecoder parseDeclarationMatchKind c
        "P4Parser"                  -> (_Typed @P4Parser #)             <$> tryDecoder parseP4Parser c
        "P4Control"                 -> (_Typed @P4Control #)            <$> tryDecoder parseP4Control c
        "Declaration_Instance"      -> (_Typed @DeclarationInstance #)  <$> tryDecoder parseDeclarationInstance c
        "Type_Parser"               -> (_Typed @TypeParser #)           <$> tryDecoder parseTypeParser c
        "Type_Control"              -> (_Typed @TypeControl #)          <$> tryDecoder parseTypeControl c
        "Type_Package"              -> (_Typed @TypePackage #)          <$> tryDecoder parseTypePackage c
        _ -> throwError . D.ParseFailed $ "invalid node type for TopLevel: " <> nodeType

data TopLevelTypeDecl
  = TypeError'TopLevelTypeDecl TypeError
  | TypeTypedef'TopLevelTypeDecl TypeTypedef
  | TypeHeader'TopLevelTypeDecl TypeHeader
  | TypeStruct'TopLevelTypeDecl TypeStruct
  | TypeEnum'TopLevelTypeDecl TypeEnum
  | TypeExtern'TopLevelTypeDecl TypeExtern
  deriving ( Show, Generic, GS.Generic, Eq, Hashable )

topLevelTypeDeclDecoderInner :: DecompressC r => D.JCurs -> D.DecodeResult (Sem r) (Maybe TopLevelTypeDecl)
topLevelTypeDeclDecoderInner c = do
  nodeType <- currentNodeType c

  case nodeType of
    "Type_Error"                -> Just . (_Typed @TypeError #)   <$> tryDecoder parseTypeError c
    "Type_Typedef"              -> Just . (_Typed @TypeTypedef #) <$> tryDecoder parseTypeTypedef c
    "Type_Header"               -> Just . (_Typed @TypeHeader #)  <$> tryDecoder parseTypeHeader c
    "Type_Struct"               -> Just . (_Typed @TypeStruct #)  <$> tryDecoder parseTypeStruct c
    "Type_Enum"                 -> Just . (_Typed @TypeEnum #)    <$> tryDecoder parseTypeEnum c
    "Type_Extern"               -> Just . (_Typed @TypeExtern #)  <$> tryDecoder parseTypeExtern c
    _ -> pure Nothing

topLevelTypeDeclDecoder :: DecompressC r => D.Decoder (Sem r) TopLevelTypeDecl
topLevelTypeDeclDecoder = D.withCursor $ \c -> do
  res <- topLevelTypeDeclDecoderInner c
  case res of
    Just x  -> pure x
    Nothing -> do
        nodeType <- currentNodeType c
        throwError . D.ParseFailed $ "invalid node type for TopLevelTypeDecl: " <> nodeType

data P4Action = P4Action
  { name        :: Text
  , annotations :: [Annotation]
  , parameters  :: MapVec Text Parameter
  , body        :: BlockStatement
  }
  deriving ( Show, Generic, Eq, Hashable )

parseP4Action :: DecompressC r => D.Decoder (Sem r) P4Action
parseP4Action = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  parameters  <- D.fromKey "parameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o
  body        <- D.fromKey "body" parseBlockStatement o
  pure $ P4Action name annotations parameters body

data Declaration
  = P4Action'Declaration P4Action
  | P4Table'Declaration P4Table
  | DeclarationInstance'Declaration DeclarationInstance
  | DeclarationVariable'Declaration DeclarationVariable
  deriving ( Show, Generic, Eq, Hashable )

declarationDecoder :: DecompressC r => D.Decoder (Sem r) Declaration
declarationDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "P4Action"             -> (_Typed @P4Action #)            <$> tryDecoder parseP4Action c
    "P4Table"              -> (_Typed @P4Table #)             <$> tryDecoder parseP4Table c
    "Declaration_Instance" -> (_Typed @DeclarationInstance #) <$> tryDecoder parseDeclarationInstance c
    "Declaration_Variable" -> (_Typed @DeclarationVariable #) <$> tryDecoder parseDeclarationVariable c
    _ -> throwError . D.ParseFailed $ "invalid node type for Declaration: " <> nodeType

data ParserState = ParserState
  { annotations      :: [Annotation]
  , components       :: [Statement]
  , selectExpression :: Maybe ParserSelect
  }
  deriving ( Show, Generic, Eq, Hashable )

parseParserState :: DecompressC r => D.Decoder (Sem r) ParserState
parseParserState = D.withCursor . tryParseVal $ \c -> do
  o                <- D.down c
  annotations      <- D.fromKey "annotations" parseAnnotations o
  components       <- D.fromKey "components" (parseVector statementDecoder) o
  selectExpression <- D.fromKeyOptional "selectExpression" parserSelectDecoder o
  pure $ ParserState annotations components selectExpression

data ParserSelect
  = SelectExpression'ParserSelect SelectExpression
  | PathExpression'ParserSelect PathExpression
  deriving ( Show, Generic, Eq, Hashable )

parserSelectDecoder :: DecompressC r => D.Decoder (Sem r) ParserSelect
parserSelectDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "SelectExpression" -> (_Typed @SelectExpression #) <$> tryDecoder parseSelectExpression c
    "PathExpression"   -> (_Typed @PathExpression #)   <$> tryDecoder parsePathExpression c
    _ -> throwError . D.ParseFailed $ "invalid node type for ParserSelect: " <> nodeType

data SelectExpression = SelectExpression
  { type_            :: P4Type
  , selectType       :: P4Type
  , selectComponents :: [Expression]
  , cases            :: [SelectCase]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseSelectExpression :: DecompressC r => D.Decoder (Sem r) SelectExpression
parseSelectExpression = D.withCursor . tryParseVal $ \c -> do
  o                <- D.down c
  type_            <- D.fromKey "type" p4TypeDecoder o
  selectType       <- D.fromKey "select" (parseNestedObject "type" p4TypeDecoder) o
  selectComponents <- D.fromKey "select"
    (parseNestedObject "components"
      (parseVector expressionDecoder)) o
  cases            <- D.fromKey "selectCases" (parseVector parseSelectCase) o
  pure $ SelectExpression type_ selectType selectComponents cases

data SelectCase = SelectCase
  { keyset :: SelectKey
  , state  :: PathExpression
  }
  deriving ( Show, Generic, Eq, Hashable )

parseSelectCase :: DecompressC r => D.Decoder (Sem r) SelectCase
parseSelectCase = D.withCursor . tryParseVal $ \c -> do
  o      <- D.down c
  keyset <- D.fromKey "keyset" selectKeyDecoder o
  state  <- D.fromKey "state" parsePathExpression o
  pure $ SelectCase keyset state

data P4Parser = P4Parser
  { name              :: Text
  , type_             :: TypeParser
  , constructorParams :: MapVec Text Parameter
  , parserLocals      :: MapVec Text Declaration
  , states            :: MapVec Text ParserState
  }
  deriving ( Show, Generic, Eq, Hashable )

parseP4Parser :: DecompressC r => D.Decoder (Sem r) P4Parser
parseP4Parser = D.withCursor . tryParseVal $ \c -> do
  o                 <- D.down c
  name              <- D.fromKey "name" D.text o
  type_             <- D.fromKey "type" parseTypeParser o
  constructorParams <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o
  parserLocals      <- D.fromKey "parserLocals" (parseIndexedVector declarationDecoder) o
  states            <- D.fromKey "states" (parseIndexedVector parseParserState) o
  pure $ P4Parser name type_ constructorParams parserLocals states

-- newtype Attribute = Attribute Json
--   deriving ( Show, Generic, Eq, Hashable )

newtype DeclarationMatchKind = DeclarationMatchKind
  { members :: MapVec Text DeclarationID
  }
  deriving ( Show, Generic, Eq, Hashable )

parseDeclarationMatchKind :: DecompressC r => D.Decoder (Sem r) DeclarationMatchKind
parseDeclarationMatchKind = D.withCursor . tryParseVal $ \c -> do
  o       <- D.down c
  members <- D.fromKey "members" (parseIndexedVector parseDeclarationID) o
  pure $ DeclarationMatchKind members

data P4Control = P4Control
  { name              :: Text
  , type_             :: TypeControl
  , constructorParams :: MapVec Text Parameter
  , controlLocals     :: MapVec Text Declaration
  , body              :: BlockStatement
  }
  deriving ( Show, Generic, Eq, Hashable )

parseP4Control :: DecompressC r => D.Decoder (Sem r) P4Control
parseP4Control = D.withCursor . tryParseVal $ \c -> do
  o                 <- D.down c
  name              <- D.fromKey "name" D.text o
  type_             <- D.fromKey "type" parseTypeControl o
  constructorParams <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o
  controlLocals     <- D.fromKey "controlLocals" (parseIndexedVector declarationDecoder) o
  body              <- D.fromKey "body" parseBlockStatement o
  pure $ P4Control name type_ constructorParams controlLocals body

data DeclarationInstance = DeclarationInstance
  { name        :: Text
  , annotations :: [Annotation]
  , type_       :: P4Type
  , arguments   :: [Argument ConstructorCallExpression]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseDeclarationInstance :: DecompressC r => D.Decoder (Sem r) DeclarationInstance
parseDeclarationInstance = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  type_       <- D.fromKey "type" p4TypeDecoder o
  arguments   <- D.fromKey "arguments" (parseVector $ parseArgument parseConstructorCallExpression) o
  pure $ DeclarationInstance name annotations type_ arguments
