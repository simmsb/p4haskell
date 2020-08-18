-- | P4 Types
module P4Haskell.Types.AST.Types where

import           Control.Monad.Error.Class          ( throwError )

import           Data.Generics.Sum.Typed

import {-# SOURCE #-} P4Haskell.Types.AST.Annotation
import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DeclarationID
import           P4Haskell.Types.AST.DecompressJSON
import {-# SOURCE #-} P4Haskell.Types.AST.Method
import           P4Haskell.Types.AST.MapVec
import {-# SOURCE #-} P4Haskell.Types.AST.Parameter
import {-# SOURCE #-} P4Haskell.Types.AST.ActionList
import           P4Haskell.Types.AST.Path
import {-# SOURCE #-} P4Haskell.Types.AST.Table

import           Prelude

import           Polysemy

import qualified Waargonaut.Decode                  as D
import qualified Waargonaut.Decode.Error            as D

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
  | TypeEnum'P4Type TypeEnum
  | TypeAction'P4Type TypeAction
  | TypeError'P4Type TypeError
  | TypeActionEnum'P4Type TypeActionEnum
  | TypeString'P4Type TypeString
  | TypeMatchKind'P4Type TypeMatchKind
  | TypeTable'P4Type TypeTable
  deriving ( Show, Generic, Eq, Hashable )

p4TypeDecoder :: DecompressC r => D.Decoder (Sem r) P4Type
p4TypeDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "Type_Var"         -> (_Typed @TypeVar #)         <$> tryDecoder parseTypeVar c
    "Type_Void"        -> (_Typed @TypeVoid #)        <$> tryDecoder parseTypeVoid c
    "Type_Unknown"     -> (_Typed @TypeUnknown #)     <$> tryDecoder parseTypeUnknown c
    "Type_Bits"        -> (_Typed @TypeBits #)        <$> tryDecoder parseTypeBits c
    "Type_Name"        -> (_Typed @TypeName #)        <$> tryDecoder parseTypeName c
    "Type_Boolean"     -> (_Typed @TypeBoolean #)     <$> tryDecoder parseTypeBoolean c
    "Type_Parser"      -> (_Typed @TypeParser #)      <$> tryDecoder parseTypeParser c
    "Type_Control"     -> (_Typed @TypeControl #)     <$> tryDecoder parseTypeControl c
    "Type_Package"     -> (_Typed @TypePackage #)     <$> tryDecoder parseTypePackage c
    "Type_Specialized" -> (_Typed @TypeSpecialized #) <$> tryDecoder parseTypeSpecialized c
    "Type_Typedef"     -> (_Typed @TypeTypedef #)     <$> tryDecoder parseTypeTypedef c
    "Type_Header"      -> (_Typed @TypeHeader #)      <$> tryDecoder parseTypeHeader c
    "Type_Method"      -> (_Typed @TypeMethod #)      <$> tryDecoder parseTypeMethod c
    "Type_Extern"      -> (_Typed @TypeExtern #)      <$> tryDecoder parseTypeExtern c
    "Type_Struct"      -> (_Typed @TypeStruct #)      <$> tryDecoder parseTypeStruct c
    "Type_Enum"        -> (_Typed @TypeEnum #)        <$> tryDecoder parseTypeEnum c
    "Type_Action"      -> (_Typed @TypeAction #)      <$> tryDecoder parseTypeAction c
    "Type_Error"       -> (_Typed @TypeError #)       <$> tryDecoder parseTypeError c
    "Type_ActionEnum"  -> (_Typed @TypeActionEnum #)  <$> tryDecoder parseTypeActionEnum c
    "Type_String"      -> (_Typed @TypeString #)      <$> tryDecoder parseTypeString c
    "Type_MatchKind"   -> (_Typed @TypeMatchKind #)   <$> tryDecoder parseTypeMatchKind c
    "Type_Table"       -> (_Typed @TypeTable #)       <$> tryDecoder parseTypeTable c
    _ -> throwError . D.ParseFailed $ "invalid node type for P4Type: " <> nodeType


data TypeEnum = TypeEnum
  { name        :: Text
  , annotations :: [Annotation]
  , members     :: MapVec Text DeclarationID
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeEnum :: DecompressC r => D.Decoder (Sem r) TypeEnum
parseTypeEnum = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  members     <- D.fromKey "members" (parseIndexedVector parseDeclarationID) o
  pure $ TypeEnum name annotations members

data TypeStruct = TypeStruct
  { name        :: Text
  , annotations :: [Annotation]
  , fields      :: MapVec Text StructField
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeStruct :: DecompressC r => D.Decoder (Sem r) TypeStruct
parseTypeStruct = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  fields      <- D.fromKey "fields" (parseIndexedVector parseStructField) o
  pure $ TypeStruct name annotations fields

data StructField = StructField
  { name :: Text
  , annotations :: [Annotation]
  , type_ :: P4Type
  }
  deriving ( Show, Generic, Eq, Hashable )

parseStructField :: DecompressC r => D.Decoder (Sem r) StructField
parseStructField = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  type_       <- D.fromKey "type" p4TypeDecoder o
  pure $ StructField name annotations type_

data TypeHeader = TypeHeader
  { name        :: Text
  , annotations :: [Annotation]
  , fields      :: MapVec Text StructField
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeHeader :: DecompressC r => D.Decoder (Sem r) TypeHeader
parseTypeHeader = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  fields      <- D.fromKey "fields" (parseIndexedVector parseStructField) o
  pure $ TypeHeader name annotations fields

data TypeTypedef = TypeTypedef
  { name        :: Text
  , annotations :: [Annotation]
  , type_       :: P4Type
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeTypedef :: DecompressC r => D.Decoder (Sem r) TypeTypedef
parseTypeTypedef = D.withCursor . tryParseVal $ \c -> do
  o           <- D.down c
  name        <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  type_       <- D.fromKey "type" p4TypeDecoder o
  pure $ TypeTypedef name annotations type_

data TypeSpecialized = TypeSpecialized
  { baseType  :: P4Type
  , arguments :: [P4Type]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeSpecialized :: DecompressC r => D.Decoder (Sem r) TypeSpecialized
parseTypeSpecialized = D.withCursor . tryParseVal $ \c -> do
  o              <- D.down c
  baseType       <- D.fromKey "baseType" p4TypeDecoder o
  arguments      <- D.fromKey "arguments" (parseVector p4TypeDecoder) o
  pure $ TypeSpecialized baseType arguments

data TypePackage = TypePackage
  { name              :: Text
  , annotations       :: [Annotation]
  , typeParameters    :: MapVec Text TypeVar
  , constructorParams :: MapVec Text Parameter
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypePackage :: DecompressC r => D.Decoder (Sem r) TypePackage
parseTypePackage = D.withCursor . tryParseVal $ \c -> do
  o              <- D.down c
  name           <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseTypeVar)) o
  constructorParams    <- D.fromKey "constructorParams"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o
  pure $ TypePackage name annotations typeParameters constructorParams

data TypeControl = TypeControl
  { name           :: Text
  , annotations    :: [Annotation]
  , typeParameters :: MapVec Text TypeVar
  , applyParams    :: MapVec Text Parameter
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeControl :: DecompressC r => D.Decoder (Sem r) TypeControl
parseTypeControl = D.withCursor . tryParseVal $ \c -> do
  o              <- D.down c
  name           <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseTypeVar)) o
  applyParams    <- D.fromKey "applyParams"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o
  pure $ TypeControl name annotations typeParameters applyParams

-- NOTE: these can't just be `pure TypeX` because that won't insert it into the parse cache
--       which will mean when we have a reference node it will never resolve

data TypeMatchKind = TypeMatchKind
  deriving ( Show, Generic, Eq, Hashable )

parseTypeMatchKind :: DecompressC r => D.Decoder (Sem r) TypeMatchKind
parseTypeMatchKind = D.withCursor . tryParseVal $ \_c -> pure TypeMatchKind

data TypeString = TypeString
  deriving ( Show, Generic, Eq, Hashable )

parseTypeString :: DecompressC r => D.Decoder (Sem r) TypeString
parseTypeString = D.withCursor . tryParseVal $ \_c -> pure TypeString

data TypeBoolean = TypeBoolean
  deriving ( Show, Generic, Eq, Hashable )

parseTypeBoolean :: DecompressC r => D.Decoder (Sem r) TypeBoolean
parseTypeBoolean = D.withCursor . tryParseVal $ \_c -> pure TypeBoolean

data TypeUnknown = TypeUnknown
  deriving ( Show, Generic, Eq, Hashable )

parseTypeUnknown :: DecompressC r => D.Decoder (Sem r) TypeUnknown
parseTypeUnknown = D.withCursor . tryParseVal $ \_c -> pure TypeUnknown

data TypeVoid = TypeVoid
  deriving ( Show, Generic, Eq, Hashable )

parseTypeVoid :: DecompressC r => D.Decoder (Sem r) TypeVoid
parseTypeVoid = D.withCursor . tryParseVal $ \_c -> pure TypeVoid

data TypeBits = TypeBits
  { size     :: Int
  , isSigned :: Bool
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeBits :: DecompressC r => D.Decoder (Sem r) TypeBits
parseTypeBits = D.withCursor . tryParseVal $ \c -> do
  o        <- D.down c
  size     <- D.fromKey "size" D.int o
  isSigned <- D.fromKey "isSigned" D.bool o
  pure $ TypeBits size isSigned

data TypeVar = TypeVar
  { name   :: Text
  , declID :: Int
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeVar :: DecompressC r => D.Decoder (Sem r) TypeVar
parseTypeVar = D.withCursor . tryParseVal $ \c -> do
  o      <- D.down c
  name   <- D.fromKey "name" D.text o
  declID <- D.fromKey "declid" D.int o
  pure $ TypeVar name declID

data TypeAction = TypeAction
  { typeParameters :: MapVec Text TypeVar
  , parameters     :: MapVec Text Parameter
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeAction :: DecompressC r => D.Decoder (Sem r) TypeAction
parseTypeAction = D.withCursor . tryParseVal $ \c -> do
  o              <- D.down c
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseTypeVar)) o
  parameters     <- D.fromKey "parameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o
  pure $ TypeAction typeParameters parameters

newtype TypeName = TypeName
  { path :: Path
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeName :: DecompressC r => D.Decoder (Sem r) TypeName
parseTypeName = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  TypeName <$> D.fromKey "path" parsePath o

data TypeParser = TypeParser
  { name           :: Text
  , annotations    :: [Annotation]
  , typeParameters :: MapVec Text TypeVar
  , applyParams    :: MapVec Text Parameter
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeParser :: DecompressC r => D.Decoder (Sem r) TypeParser
parseTypeParser = D.withCursor . tryParseVal $ \c -> do
  o              <- D.down c
  name           <- D.fromKey "name" D.text o
  annotations <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseTypeVar)) o
  applyParams    <- D.fromKey "applyParams"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o
  pure $ TypeParser name annotations typeParameters applyParams

data TypeMethod = TypeMethod
  { typeParameters :: MapVec Text TypeVar
  , parameters     :: MapVec Text Parameter
  , returnType     :: Maybe P4Type
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeMethod :: DecompressC r => D.Decoder (Sem r) TypeMethod
parseTypeMethod = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseTypeVar)) o

  parameters <- D.fromKey "parameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseParameter)) o

  returnType <- D.fromKeyOptional "returnType" p4TypeDecoder o
  pure $ TypeMethod typeParameters parameters returnType

data TypeExtern = TypeExtern
  { annotations    :: [Annotation]
  , typeParameters :: MapVec Text TypeVar
  , methods        :: [Method]
  , name           :: Text
  --, attributes     :: MapVec Text Attribute
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeExtern :: DecompressC r => D.Decoder (Sem r) TypeExtern
parseTypeExtern = D.withCursor . tryParseVal $ \c -> do
  o              <- D.down c
  annotations <- D.fromKey "annotations" parseAnnotations o
  typeParameters <- D.fromKey "typeParameters"
    (parseNestedObject "parameters"
     (parseIndexedVector parseTypeVar)) o
  methods        <- D.fromKey "methods" (parseVector parseMethod) o
  name           <- D.fromKey "name" D.text o
  --attributes     <- D.fromKey "attributes" (parseMap parseAttribute) o
  pure $ TypeExtern annotations typeParameters methods name -- attributes

data TypeError = TypeError
  { members :: MapVec Text DeclarationID
  , name    :: Text
  , declID  :: Int
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeError :: DecompressC r => D.Decoder (Sem r) TypeError
parseTypeError = D.withCursor . tryParseVal $ \c -> do
  o       <- D.down c
  members <- D.fromKey "members" (parseIndexedVector parseDeclarationID) o
  name    <- D.fromKey "name" D.text o
  declID  <- D.fromKey "declid" D.int o
  pure $ TypeError members name declID

newtype TypeActionEnum = TypeActionEnum
  { actionList :: ActionList
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeActionEnum :: DecompressC r => D.Decoder (Sem r) TypeActionEnum
parseTypeActionEnum = D.withCursor . tryParseVal $ \c -> do
  o          <- D.down c
  actionList <- D.fromKey "actionList" parseActionList o
  pure $ TypeActionEnum actionList

newtype TypeTable = TypeTable
  { table :: P4Table
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeTable :: DecompressC r => D.Decoder (Sem r) TypeTable
parseTypeTable = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  table <- D.fromKey "table" parseP4Table o
  pure $ TypeTable table
