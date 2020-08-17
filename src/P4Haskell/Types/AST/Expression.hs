module P4Haskell.Types.AST.Expression where

import           Control.Monad.Error.Class          ( throwError )

import           Data.Generics.Sum.Typed

import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Path
import           P4Haskell.Types.AST.Types

import           Prelude

import           Polysemy                           hiding ( Member )

import qualified Waargonaut.Decode                  as D
import qualified Waargonaut.Decode.Error            as D


data Expression
  = MethodCallExpression'Expression MethodCallExpression
  | Member'Expression Member
  | Argument'Expression Argument
  | ConstructorCallExpression'Expression ConstructorCallExpression
  | Constant'Expression Constant
  | PathExpression'Expression PathExpression
  | BoolLiteral'Expression BoolLiteral
  | StringLiteral'Expression StringLiteral
  | TypeNameExpression'Expression TypeNameExpression
  | LNot'Expression LNot
  deriving ( Show, Generic, Eq, Hashable )

expressionDecoder :: DecompressC r => D.Decoder (Sem r) Expression
expressionDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "MethodCallExpression"      -> (_Typed @MethodCallExpression #)      <$> tryDecoder parseMethodCallExpression c
    "Member"                    -> (_Typed @Member #)                    <$> tryDecoder parseMember c
    "Argument"                  -> (_Typed @Argument #)                  <$> tryDecoder parseArgument c
    "ConstructorCallExpression" -> (_Typed @ConstructorCallExpression #) <$> tryDecoder parseConstructorCallExpression c
    "Constant"                  -> (_Typed @Constant #)                  <$> tryDecoder parseConstant c
    "PathExpression"            -> (_Typed @PathExpression #)            <$> tryDecoder parsePathExpression c
    "BoolLiteral"               -> (_Typed @BoolLiteral #)               <$> tryDecoder parseBoolLiteral c
    "StringLiteral"             -> (_Typed @StringLiteral #)             <$> tryDecoder parseStringLiteral c
    "TypeNameExpression"        -> (_Typed @TypeNameExpression #)        <$> tryDecoder parseTypeNameExpression c
    "LNot"                      -> (_Typed @LNot #)                      <$> tryDecoder parseLNot c
    _ -> throwError . D.ParseFailed $ "invalid node type for Expression: " <> nodeType

data TypeType = TypeType
  { type_ :: P4Type
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeType :: DecompressC r => D.Decoder (Sem r) TypeType
parseTypeType = D.withCursor . tryParseVal $ \c -> do
  o             <- D.down c
  type_         <- D.fromKey "type" p4TypeDecoder o
  pure $ TypeType type_

data TypeNameExpression = TypeNameExpression
  { type_    :: P4Type
  , typeName :: TypeName
  }
  deriving ( Show, Generic, Eq, Hashable )

parseTypeNameExpression :: DecompressC r => D.Decoder (Sem r) TypeNameExpression
parseTypeNameExpression = D.withCursor . tryParseVal $ \c -> do
  o              <- D.down c
  TypeType type_ <- D.fromKey "type" parseTypeType o
  typeName       <- D.fromKey "typeName" parseTypeName o
  pure $ TypeNameExpression type_ typeName

data MethodCallExpression = MethodCallExpression
  { type_         :: P4Type
  , method        :: Expression
  , typeArguments :: [P4Type]
  , arguments     :: [Argument]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseMethodCallExpression :: DecompressC r => D.Decoder (Sem r) MethodCallExpression
parseMethodCallExpression = D.withCursor . tryParseVal $ \c -> do
  o             <- D.down c
  type_         <- D.fromKey "type" p4TypeDecoder o
  method        <- D.fromKey "method" expressionDecoder o
  typeArguments <- D.fromKey "typeArguments" (parseVector p4TypeDecoder) o
  arguments     <- D.fromKey "arguments" (parseVector parseArgument) o
  pure $ MethodCallExpression type_ method typeArguments arguments

data Member = Member
  { type_ :: P4Type
  , expr  :: Expression
  , member :: Text
  }
  deriving ( Show, Generic, Eq, Hashable )

parseMember :: DecompressC r => D.Decoder (Sem r) Member
parseMember = D.withCursor . tryParseVal $ \c -> do
  o      <- D.down c
  type_  <- D.fromKey "type" p4TypeDecoder o
  expr   <- D.fromKey "expr" expressionDecoder o
  member <- D.fromKey "member" D.text o
  pure $ Member type_ expr member

data Argument = Argument
  { name       :: Maybe Text
  , expression :: Expression
  }
  deriving ( Show, Generic, Eq, Hashable )

parseArgument :: DecompressC r => D.Decoder (Sem r) Argument
parseArgument = D.withCursor . tryParseVal $ \c -> do
  o          <- D.down c
  name       <- D.fromKey "name" (D.maybeOrNull D.text) o
  expression <- D.fromKey "expression" expressionDecoder o
  pure $ Argument name expression

data ConstructorCallExpression = ConstructorCallExpression
  { type_           :: P4Type
  , constructedType :: P4Type
  , arguments       :: [Argument]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseConstructorCallExpression :: DecompressC r => D.Decoder (Sem r) ConstructorCallExpression
parseConstructorCallExpression = D.withCursor . tryParseVal $ \c -> do
  o               <- D.down c
  type_           <- D.fromKey "type" p4TypeDecoder o
  constructedType <- D.fromKey "constructedType" p4TypeDecoder o
  arguments       <- D.fromKey "arguments" (parseVector parseArgument) o
  pure $ ConstructorCallExpression type_ constructedType arguments

data Constant = Constant
  { type_ :: P4Type
  , value :: Int
  , base  :: Int
  }
  deriving ( Show, Generic, Eq, Hashable )

parseConstant :: DecompressC r => D.Decoder (Sem r) Constant
parseConstant = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  value <- D.fromKey "value" D.int o
  base  <- D.fromKey "base" D.int o
  pure $ Constant type_ value base

data PathExpression = PathExpression
  { type_ :: P4Type
  , path  :: Path
  }
  deriving ( Show, Generic, Eq, Hashable )

parsePathExpression :: DecompressC r => D.Decoder (Sem r) PathExpression
parsePathExpression = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  path  <- D.fromKey "path" parsePath o
  pure $ PathExpression type_ path

newtype BoolLiteral = BoolLiteral
  { value :: Bool
  }
  deriving ( Show, Generic, Eq, Hashable )

parseBoolLiteral :: DecompressC r => D.Decoder (Sem r) BoolLiteral
parseBoolLiteral = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  value <- D.fromKey "value" D.bool o
  pure $ BoolLiteral value

data LNot = LNot
  { type_ :: P4Type
  , expr  :: Expression
  }
  deriving ( Show, Generic, Eq, Hashable )

parseLNot :: DecompressC r => D.Decoder (Sem r) LNot
parseLNot = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  expr  <- D.fromKey "expr" expressionDecoder o
  pure $ LNot type_ expr

data StringLiteral = StringLiteral
  { type_ :: P4Type
  , value :: Text
  }
  deriving ( Show, Generic, Eq, Hashable )

parseStringLiteral :: DecompressC r => D.Decoder (Sem r) StringLiteral
parseStringLiteral = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  value <- D.fromKey "value" D.text o
  pure $ StringLiteral type_ value
