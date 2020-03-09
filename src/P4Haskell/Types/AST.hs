-- | Represents the P4 AST in haskellmodule P4Haskell.Types.AST
module P4Haskell.Types.AST
    ( astDecoder ) where

import           Control.Monad.Error.Class      ( throwError )

import           Data.Generics.Sum.Typed

import           P4Haskell.Types.DecompressJSON

import           Prelude                        hiding ( Member )

import           Waargonaut
import qualified Waargonaut.Decode              as D
import qualified Waargonaut.Decode.Error        as D

type DecompressC' r = DecompressC Node r

astDecoder :: DecompressC' r => D.Decoder (Sem r) P4Program
astDecoder = D.withCursor $ \c -> do
  o       <- D.down c
  objects <- D.fromKey "objects" (parseVectorPure nodeDecoder) o
  pure $ P4Program objects

-- ^ Like 'parseVector' but doesn't perform lookups on the decompression state.
parseVectorPure :: Monad m => D.Decoder m a -> D.Decoder m [a]
parseVectorPure inner = D.withCursor $ \c -> do
  o <- D.down c
  D.fromKey "vec" (D.list inner) o

parseVector :: (DecompressC' r, AsType [a] Node)
            => D.Decoder (Sem r) a
            -> D.Decoder (Sem r) [a]
parseVector inner = D.withCursor . tryParseVal' $ \c -> do
    o <- D.down c
    D.fromKey "vec" (D.list inner) o

-- parseMap :: DecompressC' => D.decoder

parseNestedObject
  :: (DecompressC' r, AsType a Node)
  => Text
  -> D.Decoder (Sem r) a
  -> D.Decoder (Sem r) a
parseNestedObject key inner = D.withCursor . tryParseVal' $ \c -> do
    o <- D.down c
    D.fromKey key inner o

newtype P4Program = P4Program
  { objects :: [Node]
  }
  deriving ( Show, Generic )

data Node
  = TypeError'Node TypeError
  | TypeExtern'Node TypeExtern
  | Annotations'Node [Annotation]
  | Annotation'Node Annotation
  | TypeVar'Node TypeVar
  | TypeVars'Node [TypeVar]
  | DeclarationID'Node DeclarationID
  | DeclarationIDs'Node [DeclarationID]
  | Method'Node Method
  | Methods'Node [Method]
  | Parameter'Node Parameter
  | Parameters'Node [Parameter]
  | TypeMethod'Node TypeMethod
  | TypeBits'Node TypeBits
  | TypeParser'Node TypeParser
  | P4Type'Node P4Type
  | P4Types'Node [P4Type]
  | TypeControl'Node TypeControl
  | TypePackage'Node TypePackage
  | TypeSpecialized'Node TypeSpecialized
  | TypeTypedef'Node TypeTypedef
  | TypeHeader'Node TypeHeader
  | StructField'Node StructField
  | StructFields'Node [StructField]
  | TypeStruct'Node TypeStruct
  | Jsons'Node [Json]
  | TypeName'Node TypeName
  | DeclarationMatchKind'Node DeclarationMatchKind
  | Path'Node Path
  | PathExpression'Node PathExpression
  | AssignmentStatement'Node AssignmentStatement
  | P4Action'Node P4Action
  | BlockStatement'Node BlockStatement
  | StatOrDecl'Node StatOrDecl
  | StatOrDecls'Node [StatOrDecl]
  | P4Parser'Node P4Parser
  | Declaration'Node Declaration
  | Declarations'Node [Declaration]
  | ParserState'Node ParserState
  | ParserStates'Node [ParserState]
  | Argument'Node Argument
  | Arguments'Node [Argument]
  | MethodCallExpression'Node MethodCallExpression
  | MethodCallStatement'Node MethodCallStatement
  | Member'Node Member
  | P4Control'Node P4Control
  | BoolLiteral'Node BoolLiteral
  | Property'Node Property
  | Properties'Node [Property]
  | P4Table'Node P4Table
  | Nope
  deriving ( Show, Generic )

nodeDecoder :: DecompressC' r => D.Decoder (Sem r) Node
nodeDecoder = D.withCursor $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "Type_Error"            -> (_Typed @TypeError #) <$> D.focus parseTypeError c
    "Type_Extern"           -> (_Typed @TypeExtern #) <$> D.focus parseTypeExtern c
    "Type_Parser"           -> (_Typed @TypeParser #) <$> D.focus parseTypeParser c
    "Type_Control"          -> (_Typed @TypeControl #) <$> D.focus parseTypeControl c
    "Type_Package"          -> (_Typed @TypePackage #) <$> D.focus parseTypePackage c
    "Type_Typedef"          -> (_Typed @TypeTypedef #) <$> D.focus parseTypeTypedef c
    "Type_Header"           -> (_Typed @TypeHeader #) <$> D.focus parseTypeHeader c
    "Type_Struct"           -> (_Typed @TypeStruct #) <$> D.focus parseTypeStruct c
    "Method"                -> (_Typed @Method #) <$> D.focus parseMethod c
    "Member"                -> (_Typed @Member #) <$> D.focus parseMember c
    "Declaration_MatchKind" -> (_Typed @DeclarationMatchKind #) <$> D.focus parseDeclarationMatchKind c
    "P4Parser"              -> (_Typed @P4Parser #) <$> D.focus parseP4Parser c
    "PathExpression"        -> (_Typed @PathExpression #) <$> D.focus parsePathExpression c
    "P4Control"             -> (_Typed @P4Control #) <$> D.focus parseP4Control c
    "BoolLiteral"           -> (_Typed @BoolLiteral #) <$> D.focus parseBoolLiteral c
    _ -> throwError . D.ParseFailed $ "invalid node type for Node: " <> nodeType

data Member = Member
  { type_ :: P4Type
  , expr  :: Node
  , member :: Text
  }
  deriving ( Show, Generic )

parseMember :: DecompressC' r => D.Decoder (Sem r) Member
parseMember = D.withCursor . tryParseVal' $ \c -> do
  o      <- D.down c
  type_  <- D.fromKey "type" parseP4Type o
  expr   <- D.fromKey "expr" nodeDecoder o
  member <- D.fromKey "member" D.text o
  pure $ Member type_ expr member

data Argument = Argument
  { name       :: Maybe Text
  , expression :: Node
  }
  deriving ( Show, Generic )

parseArgument :: DecompressC' r => D.Decoder (Sem r) Argument
parseArgument = D.withCursor . tryParseVal' $ \c -> do
  o          <- D.down c
  name       <- D.fromKey "name" (D.maybeOrNull D.text) o
  expression <- D.fromKey "expression" nodeDecoder o
  pure $ Argument name expression

data MethodCallExpression = MethodCallExpression
  { type_         :: P4Type
  , method        :: Node -- TODO: is this correct
  , typeArguments :: [P4Type]
  , arguments     :: [Argument]
  }
  deriving ( Show, Generic )

parseMethodCallExpression :: DecompressC' r => D.Decoder (Sem r) MethodCallExpression
parseMethodCallExpression = D.withCursor . tryParseVal' $ \c -> do
  o             <- D.down c
  type_         <- D.fromKey "type" parseP4Type o
  method        <- D.fromKey "method" nodeDecoder o
  typeArguments <- D.fromKey "typeArguments" (parseVector parseP4Type) o
  arguments     <- D.fromKey "arguments" (parseVector parseArgument) o
  pure $ MethodCallExpression type_ method typeArguments arguments

data MethodCallStatement = MethodCallStatement
  { methodCall :: MethodCallExpression
  }
  deriving ( Show, Generic )

parseMethodCallStatement :: DecompressC' r => D.Decoder (Sem r) MethodCallStatement
parseMethodCallStatement = D.withCursor . tryParseVal' $ \c -> do
  o          <- D.down c
  methodCall <- D.fromKey "methodCall" parseMethodCallExpression o
  pure $ MethodCallStatement methodCall

data AssignmentStatement = AssignmentStatement
  { left  :: Node
  , right :: Node
  }
  deriving ( Show, Generic )

parseAssignmentStatement :: DecompressC' r => D.Decoder (Sem r) AssignmentStatement
parseAssignmentStatement = D.withCursor . tryParseVal' $ \c -> do
  o     <- D.down c
  left  <- D.fromKey "left" nodeDecoder o
  right <- D.fromKey "right" nodeDecoder o
  pure $ AssignmentStatement left right

data StatOrDecl
  = AssignmentStatement'StatOrDecl AssignmentStatement
  | MethodCallStatement'StatOrDecl MethodCallStatement
  deriving ( Show, Generic )

statOrDeclDecoder :: DecompressC' r => D.Decoder (Sem r) StatOrDecl
statOrDeclDecoder = D.withCursor $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "AssignmentStatement" -> (_Typed @AssignmentStatement #) <$> D.focus parseAssignmentStatement c
    "MethodCallStatement" -> (_Typed @MethodCallStatement #) <$> D.focus parseMethodCallStatement c
    _ -> throwError . D.ParseFailed $ "invalid node type for StatOrDecl: " <> nodeType

data BlockStatement = BlockStatement
  { annotations :: [Annotation]
  , components  :: [StatOrDecl]
  }
  deriving ( Show, Generic )

parseBlockStatement :: DecompressC' r => D.Decoder (Sem r) BlockStatement
parseBlockStatement = D.withCursor . tryParseVal' $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  components  <- D.fromKey "components" (parseVector statOrDeclDecoder) o
  pure $ BlockStatement annotations components

data P4Action = P4Action
  { name        :: Text
  , annotations :: [Annotation]
  , parameters  :: [Parameter]
  , body        :: BlockStatement
  }
  deriving ( Show, Generic )

parseP4Action :: DecompressC' r => D.Decoder (Sem r) P4Action
parseP4Action = D.withCursor . tryParseVal' $ \c -> do
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

declarationDecoder :: DecompressC' r => D.Decoder (Sem r) Declaration
declarationDecoder = D.withCursor $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "P4Action" -> (_Typed @P4Action #) <$> D.focus parseP4Action c
    "P4Table" -> (_Typed @P4Table #) <$> D.focus parseP4Table c
    _ -> throwError . D.ParseFailed $ "invalid node type for Declaration: " <> nodeType

data ParserState = ParserState
  { annotations :: [Annotation]
  , components  :: [StatOrDecl]
  }
  deriving ( Show, Generic )

parseParserState :: DecompressC' r => D.Decoder (Sem r) ParserState
parseParserState = D.withCursor . tryParseVal' $ \c -> do
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

parseP4Parser :: DecompressC' r => D.Decoder (Sem r) P4Parser
parseP4Parser = D.withCursor . tryParseVal' $ \c -> do
  o                 <- D.down c
  name              <- D.fromKey "name" D.text o
  type_             <- D.fromKey "type" parseP4Type o
  constructorParams <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  parserLocals      <- D.fromKey "parserLocals" (parseVector declarationDecoder) o
  states            <- D.fromKey "states" (parseVector parseParserState) o
  pure $ P4Parser name type_ constructorParams parserLocals states

data Annotation = Annotation
  { name         :: Text
  , body         :: [AnnotatedToken]
  , needsParsing :: Bool
  , expr         :: [Expression]
  , kv           :: [NamedExpression]
  }
  deriving ( Show, Generic )

parseAnnotations :: DecompressC' r => D.Decoder (Sem r) [Annotation]
parseAnnotations = D.withCursor . tryParseVal' $ \c -> do
  o <- D.down c
  D.fromKey "annotations" (parseVector parseAnnotation) o

parseAnnotation :: DecompressC' r => D.Decoder (Sem r) Annotation
parseAnnotation = D.withCursor . tryParseVal' $ \c -> do
  o            <- D.down c
  name         <- D.fromKey "name" D.text o
  body         <- D.fromKey "body" (parseVector parseAnnotatedToken) o
  needsParsing <- D.fromKey "needsParsing" D.bool o
  expr         <- D.fromKey "expr" (parseVector parseExpression) o
  kv           <- D.fromKey "kv" (parseVector parseNamedExpression) o
  pure $ Annotation name body needsParsing expr kv

type AnnotatedToken = Json

parseAnnotatedToken :: Monad m => D.Decoder m Json
parseAnnotatedToken = D.json

type Expression = Json

parseExpression :: Monad m => D.Decoder m Json
parseExpression = D.json

type NamedExpression = Json

parseNamedExpression :: Monad m => D.Decoder m Json
parseNamedExpression = D.json

data P4Type
  = TypeVar'P4Type TypeVar
  | TypeVoid'P4Type TypeVoid
  | TypeUnknown'P4Type TypeUnknown
  | TypeBits'P4Type TypeBits
  | TypeName'P4Type TypeName
  | TypeBoolean'P4Type TypeBoolean
  | TypeParser'P4Type TypeParser
  | TypeControl'P4Type TypeControl
  | TypePackage'P4Type TypePackage
  | TypeSpecialized'P4Type TypeSpecialized
  | TypeTypedef'P4Type TypeTypedef
  | TypeHeader'P4Type TypeHeader
  | TypeMethod'P4Type TypeMethod
  | TypeExtern'P4Type TypeExtern
  | TypeStruct'P4Type TypeStruct
  deriving ( Show, Generic )

parseP4Type :: DecompressC' r => D.Decoder (Sem r) P4Type
parseP4Type = D.withCursor . tryParseVal' $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "Type_Var"         -> (_Typed @TypeVar #) <$> D.focus parseTypeVar c
    "Type_Void"        -> (_Typed @TypeVoid #) <$> D.focus parseTypeVoid c
    "Type_Unknown"     -> (_Typed @TypeUnknown #) <$> D.focus parseTypeUnknown c
    "Type_Bits"        -> (_Typed @TypeBits #) <$> D.focus parseTypeBits c
    "Type_Name"        -> (_Typed @TypeName #) <$> D.focus parseTypeName c
    "Type_Boolean"     -> (_Typed @TypeBoolean #) <$> D.focus parseTypeBoolean c
    "Type_Parser"      -> (_Typed @TypeParser #) <$> D.focus parseTypeParser c
    "Type_Control"     -> (_Typed @TypeControl #) <$> D.focus parseTypeControl c
    "Type_Package"     -> (_Typed @TypePackage #) <$> D.focus parseTypePackage c
    "Type_Specialized" -> (_Typed @TypeSpecialized #) <$> D.focus parseTypeSpecialized c
    "Type_Typedef"     -> (_Typed @TypeTypedef #) <$> D.focus parseTypeTypedef c
    "Type_Header"      -> (_Typed @TypeHeader #) <$> D.focus parseTypeHeader c
    "Type_Method"      -> (_Typed @TypeMethod #) <$> D.focus parseTypeMethod c
    "Type_Extern"      -> (_Typed @TypeExtern #) <$> D.focus parseTypeExtern c
    "Type_Struct"      -> (_Typed @TypeStruct #) <$> D.focus parseTypeStruct c
    _ -> throwError . D.ParseFailed $ "invalid node type for P4Type: " <> nodeType

data TypeStruct = TypeStruct
  { name        :: Text
  , annotations :: [Annotation]
  , fields      :: [StructField]
  }
  deriving ( Show, Generic )

parseTypeStruct :: DecompressC' r => D.Decoder (Sem r) TypeStruct
parseTypeStruct = D.withCursor . tryParseVal' $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  fields      <- D.fromKey "fields" (parseVector parseStructField) o
  pure $ TypeStruct name annotations fields

data StructField = StructField
  { name :: Text
  , annotations :: [Annotation]
  , type_ :: P4Type
  }
  deriving ( Show, Generic )

parseStructField :: DecompressC' r => D.Decoder (Sem r) StructField
parseStructField = D.withCursor . tryParseVal' $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  type_       <- D.fromKey "type" parseP4Type o
  pure $ StructField name annotations type_

data TypeHeader = TypeHeader
  { name        :: Text
  , annotations :: [Annotation]
  , fields      :: [StructField]
  }
  deriving ( Show, Generic )

parseTypeHeader :: DecompressC' r => D.Decoder (Sem r) TypeHeader
parseTypeHeader = D.withCursor . tryParseVal' $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  fields      <- D.fromKey "fields" (parseVector parseStructField) o
  pure $ TypeHeader name annotations fields

data TypeTypedef = TypeTypedef
  { name        :: Text
  , annotations :: [Annotation]
  , type_       :: P4Type
  }
  deriving ( Show, Generic )

parseTypeTypedef :: DecompressC' r => D.Decoder (Sem r) TypeTypedef
parseTypeTypedef = D.withCursor . tryParseVal' $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  type_       <- D.fromKey "type" parseP4Type o
  pure $ TypeTypedef name annotations type_

data TypeSpecialized = TypeSpecialized
  { baseType  :: P4Type
  , arguments :: [P4Type]
  }
  deriving ( Show, Generic )

parseTypeSpecialized :: DecompressC' r => D.Decoder (Sem r) TypeSpecialized
parseTypeSpecialized = D.withCursor . tryParseVal' $ \c -> do
  o              <- D.down c
  baseType       <- D.fromKey "baseType" parseP4Type o
  arguments      <- D.fromKey "arguments" (parseVector parseP4Type) o
  pure $ TypeSpecialized baseType arguments

data TypePackage = TypePackage
  { name              :: Text
  , annotations       :: [Annotation]
  , typeParameters    :: [TypeVar]
  , constructorParams :: [Parameter]
  }
  deriving ( Show, Generic )

parseTypePackage :: DecompressC' r => D.Decoder (Sem r) TypePackage
parseTypePackage = D.withCursor . tryParseVal' $ \c -> do
  o              <- D.down c
  name           <- D.fromKey "name" D.text o
  annotations    <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseVector parseTypeVar)) o
  constructorParams    <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  pure $ TypePackage name annotations typeParameters constructorParams

data TypeControl = TypeControl
  { name           :: Text
  , annotations    :: [Annotation]
  , typeParameters :: [TypeVar]
  , applyParams    :: [Parameter]
  }
  deriving ( Show, Generic )

parseTypeControl :: DecompressC' r => D.Decoder (Sem r) TypeControl
parseTypeControl = D.withCursor . tryParseVal' $ \c -> do
  o              <- D.down c
  name           <- D.fromKey "name" D.text o
  annotations    <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseVector parseTypeVar)) o
  applyParams    <- D.fromKey "applyParams"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  pure $ TypeControl name annotations typeParameters applyParams

data TypeBoolean = TypeBoolean
  deriving ( Show, Generic )

parseTypeBoolean :: Monad m => D.Decoder m TypeBoolean
parseTypeBoolean = pure TypeBoolean

data TypeUnknown = TypeUnknown
  deriving ( Show, Generic )

parseTypeUnknown :: Monad m => D.Decoder m TypeUnknown
parseTypeUnknown = pure TypeUnknown

data TypeVoid = TypeVoid
  deriving ( Show, Generic )

parseTypeVoid :: Monad m => D.Decoder m TypeVoid
parseTypeVoid = pure TypeVoid

data TypeBits = TypeBits
  { size     :: Int
  , isSigned :: Bool
  }
  deriving ( Show, Generic )

parseTypeBits :: DecompressC' r => D.Decoder (Sem r) TypeBits
parseTypeBits = D.withCursor . tryParseVal' $ \c -> do
  o        <- D.down c
  size     <- D.fromKey "size" D.int o
  isSigned <- D.fromKey "isSigned" D.bool o
  pure $ TypeBits size isSigned

data TypeVar = TypeVar
  { name   :: Text
  , declID :: Int
  }
  deriving ( Show, Generic )

parseTypeVar :: DecompressC' r => D.Decoder (Sem r) TypeVar
parseTypeVar = D.withCursor . tryParseVal' $ \c -> do
  o      <- D.down c
  name   <- D.fromKey "name" D.text o
  declID <- D.fromKey "declid" D.int o
  pure $ TypeVar name declID

data PathExpression = PathExpression
  { type_ :: P4Type
  , path  :: Path
  }
  deriving ( Show, Generic )

parsePathExpression :: DecompressC' r => D.Decoder (Sem r) PathExpression
parsePathExpression = D.withCursor . tryParseVal' $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" parseP4Type o
  path  <- D.fromKey "path" parsePath o
  pure $ PathExpression type_ path

data Path = Path
  { absolute :: Bool
  , name     :: Text
  }
  deriving ( Show, Generic )

parsePath :: DecompressC' r => D.Decoder (Sem r) Path
parsePath = D.withCursor . tryParseVal' $ \c -> do
  o        <- D.down c
  absolute <- D.fromKey "absolute" D.bool o
  name     <- D.fromKey "name" D.text o
  pure $ Path absolute name

newtype TypeName = TypeName
  { path :: Path
  }
  deriving ( Show, Generic )

parseTypeName :: DecompressC' r => D.Decoder (Sem r) TypeName
parseTypeName = D.withCursor . tryParseVal' $ \c -> do
  o <- D.down c
  TypeName <$> D.fromKey "path" parsePath o

data Parameter = Parameter
  { annotations :: [Annotation]
  , direction   :: Text
  , name        :: Text
  , type_       :: P4Type
  }
  deriving ( Show, Generic )

parseParameter :: DecompressC' r => D.Decoder (Sem r) Parameter
parseParameter = D.withCursor . tryParseVal' $ \c -> do
  o <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  direction   <- D.fromKey "direction" D.text o
  name        <- D.fromKey "name" D.text o
  type_       <- D.fromKey "type" parseP4Type o
  pure $ Parameter annotations direction name type_


data TypeMethod = TypeMethod
  { typeParameters :: [TypeVar]
  , parameters     :: [Parameter]
  , returnType     :: Maybe P4Type
  }
  deriving ( Show, Generic )

parseTypeMethod :: DecompressC' r => D.Decoder (Sem r) TypeMethod
parseTypeMethod = D.withCursor . tryParseVal' $ \c -> do
  o <- D.down c
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseVector parseTypeVar)) o

  parameters <- D.fromKey "parameters"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o

  returnType <- D.fromKeyOptional "returnType" parseP4Type o
  pure $ TypeMethod typeParameters parameters returnType

data Method = Method
  { annotations :: [Annotation]
  , name        :: Text
  , type_       :: TypeMethod
  }
  deriving ( Show, Generic )

parseMethod :: DecompressC' r => D.Decoder (Sem r) Method
parseMethod = D.withCursor . tryParseVal' $ \c -> do
  o           <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  name        <- D.fromKey "name" D.text o
  type_       <- D.fromKey "type" parseTypeMethod o
  pure $ Method annotations name type_

newtype Attribute = Attribute Json
  deriving ( Show, Generic )

data TypeExtern = TypeExtern
  { annotations    :: [Annotation]
  , typeParameters :: [TypeVar]
  , methods        :: [Method]
  , name           :: Text
  --, attributes     :: HashMap Text Attribute
  }
  deriving ( Show, Generic )

parseTypeExtern :: DecompressC' r => D.Decoder (Sem r) TypeExtern
parseTypeExtern = D.withCursor . tryParseVal' $ \c -> do
  o              <- D.down c
  annotations    <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseVector parseTypeVar)) o
  methods        <- D.fromKey "methods" (parseVector parseMethod) o
  name           <- D.fromKey "name" D.text o
  --attributes     <- D.fromKey "attributes" (parseMap parseAttribute) o
  pure $ TypeExtern annotations typeParameters methods name -- attributes

data TypeError = TypeError
  { members :: [DeclarationID]
  , name    :: Text
  , declID  :: Int
  }
  deriving ( Show, Generic )

parseTypeError :: DecompressC' r => D.Decoder (Sem r) TypeError
parseTypeError = D.withCursor . tryParseVal' $ \c -> do
  o       <- D.down c
  members <- D.fromKey "members" (parseVector parseDeclarationID) o
  name    <- D.fromKey "name" D.text o
  declID  <- D.fromKey "declid" D.int o
  pure $ TypeError members name declID

data TypeParser = TypeParser
  { name           :: Text
  , annotations    :: [Annotation]
  , typeParameters :: [TypeVar]
  , applyParams    :: [Parameter]
  }
  deriving ( Show, Generic )

parseTypeParser :: DecompressC' r => D.Decoder (Sem r) TypeParser
parseTypeParser = D.withCursor . tryParseVal' $ \c -> do
  o              <- D.down c
  name           <- D.fromKey "name" D.text o
  annotations    <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseVector parseTypeVar)) o
  applyParams    <- D.fromKey "applyParams"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  pure $ TypeParser name annotations typeParameters applyParams

data DeclarationID = DeclarationID
  { name   :: Text
  , declID :: Int
  }
  deriving ( Show, Generic )

parseDeclarationID :: DecompressC' r => D.Decoder (Sem r) DeclarationID
parseDeclarationID = D.withCursor . tryParseVal' $ \c -> do
  o      <- D.down c
  name   <- D.fromKey "name" D.text o
  declID <- D.fromKey "declid" D.int o
  pure $ DeclarationID name declID

newtype DeclarationMatchKind = DeclarationMatchKind
  { members :: [DeclarationID]
  }
  deriving ( Show, Generic )

parseDeclarationMatchKind :: DecompressC' r => D.Decoder (Sem r) DeclarationMatchKind
parseDeclarationMatchKind = D.withCursor . tryParseVal' $ \c -> do
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

parseP4Control :: DecompressC' r => D.Decoder (Sem r) P4Control
parseP4Control = D.withCursor . tryParseVal' $ \c -> do
  o                 <- D.down c
  name              <- D.fromKey "name" D.text o
  type_             <- D.fromKey "type" parseP4Type o
  constructorParams <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseVector parseParameter)) o
  controlLocals      <- D.fromKey "controlLocals" (parseVector declarationDecoder) o
  body              <- D.fromKey "body" parseBlockStatement o
  pure $ P4Control name type_ constructorParams controlLocals body

newtype BoolLiteral = BoolLiteral
  { value :: Bool
  }
  deriving ( Show, Generic )

parseBoolLiteral :: DecompressC' r => D.Decoder (Sem r) BoolLiteral
parseBoolLiteral = D.withCursor . tryParseVal' $ \c -> do
  o     <- D.down c
  value <- D.fromKey "value" D.bool o
  pure $ BoolLiteral value

data P4Table = P4Table
  { name        :: Text
  , annotations :: [Annotation]
  , properties  :: [Property]
  }
  deriving ( Show, Generic )

parseP4Table :: DecompressC' r => D.Decoder (Sem r) P4Table
parseP4Table = D.withCursor . tryParseVal' $ \c -> do
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

parseProperty :: DecompressC' r => D.Decoder (Sem r) Property
parseProperty = D.withCursor . tryParseVal' $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  value       <- D.fromKey "value" nodeDecoder o
  isConstant  <- D.fromKey "isConstant" D.bool o
  pure $ Property name annotations value isConstant
