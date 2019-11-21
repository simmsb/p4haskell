-- | Represents the P4 AST in haskellmodule P4Haskell.Types.AST
module P4Haskell.Types.AST
    ( astDecoder ) where

import           Control.Monad.Error.Class      ( throwError )

import           Data.Generics.Sum.Typed

import           P4Haskell.Types.DecompressJSON

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
  | P4Type'Node P4Type
  | Jsons'Node [Json]
  | TypeName'Node TypeName
  | Path'Node Path
  | Nope
  deriving ( Show, Generic )

nodeDecoder :: DecompressC' r => D.Decoder (Sem r) Node
nodeDecoder = D.withCursor $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "Type_Error"  -> (_Typed @TypeError #) <$> D.focus parseTypeError c
    "Type_Extern" -> (_Typed @TypeExtern #) <$> D.focus parseTypeExtern c
    "Method"      -> (_Typed @Method #) <$> D.focus parseMethod c
    _ -> throwError . D.ParseFailed $ "invalid node type for Node: " <> nodeType

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
  | TypeBits'P4Type TypeBits
  | TypeName'P4Type TypeName
  | TypeBoolean'P4Type TypeBoolean
  deriving ( Show, Generic )

parseP4Type :: DecompressC' r => D.Decoder (Sem r) P4Type
parseP4Type = D.withCursor . tryParseVal' $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "Type_Var"     -> (_Typed @TypeVar #) <$> D.focus parseTypeVar c
    "Type_Void"    -> (_Typed @TypeVoid #) <$> D.focus parseTypeVoid c
    "Type_Bits"    -> (_Typed @TypeBits #) <$> D.focus parseTypeBits c
    "Type_Name"    -> (_Typed @TypeName #) <$> D.focus parseTypeName c
    "Type_Boolean" -> (_Typed @TypeBoolean #) <$> D.focus parseTypeBoolean c
    _ -> throwError . D.ParseFailed $ "invalid node type for P4Type: " <> nodeType

data TypeBoolean = TypeBoolean
  deriving ( Show, Generic )

parseTypeBoolean :: Monad m => D.Decoder m TypeBoolean
parseTypeBoolean = pure TypeBoolean

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
  , returnType     :: P4Type
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

  returnType <- D.fromKey "returnType" parseP4Type o
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
