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
import qualified Generics.SOP as GS
import Relude.Extra.Map ((!?))


data Expression
  = MethodCallExpression'Expression MethodCallExpression
  | Member'Expression Member
  | Constant'Expression Constant
  | PathExpression'Expression PathExpression
  | BoolLiteral'Expression BoolLiteral
  | StringLiteral'Expression StringLiteral
  | TypeNameExpression'Expression TypeNameExpression
  | UnaryOp'Expression UnaryOp
  | BinaryOp'Expression BinaryOp
  deriving ( Show, Generic, GS.Generic, Eq, Hashable )

expressionDecoder :: DecompressC r => D.Decoder (Sem r) Expression
expressionDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "MethodCallExpression"      -> (_Typed @MethodCallExpression #)      <$> tryDecoder parseMethodCallExpression c
    "Member"                    -> (_Typed @Member #)                    <$> tryDecoder parseMember c
    "Constant"                  -> (_Typed @Constant #)                  <$> tryDecoder parseConstant c
    "PathExpression"            -> (_Typed @PathExpression #)            <$> tryDecoder parsePathExpression c
    "BoolLiteral"               -> (_Typed @BoolLiteral #)               <$> tryDecoder parseBoolLiteral c
    "StringLiteral"             -> (_Typed @StringLiteral #)             <$> tryDecoder parseStringLiteral c
    "TypeNameExpression"        -> (_Typed @TypeNameExpression #)        <$> tryDecoder parseTypeNameExpression c
    IsUnaryOp t                 -> (_Typed @UnaryOp #)                   <$> tryDecoder (parseUnaryOp t) c
    IsBinaryOp t                -> (_Typed @BinaryOp #)                  <$> tryDecoder (parseBinaryOp t) c
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

data MethodExpression
  = Member'MethodExpression Member
  | PathExpression'MethodExpression PathExpression
  deriving ( Show, Generic, GS.Generic, Eq, Hashable )

methodExpressionDecoder :: DecompressC r => D.Decoder (Sem r) MethodExpression
methodExpressionDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "Member"                    -> (_Typed @Member #)                    <$> tryDecoder parseMember c
    "PathExpression"            -> (_Typed @PathExpression #)            <$> tryDecoder parsePathExpression c
    _ -> throwError . D.ParseFailed $ "invalid node type for MethodExpression: " <> nodeType

data MethodCallExpression = MethodCallExpression
  { type_         :: P4Type
  , method        :: MethodExpression
  , typeArguments :: [P4Type]
  , arguments     :: [Argument Expression]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseMethodCallExpression :: DecompressC r => D.Decoder (Sem r) MethodCallExpression
parseMethodCallExpression = D.withCursor . tryParseVal $ \c -> do
  o             <- D.down c
  type_         <- D.fromKey "type" p4TypeDecoder o
  method        <- D.fromKey "method" methodExpressionDecoder o
  typeArguments <- D.fromKey "typeArguments" (parseVector p4TypeDecoder) o
  arguments     <- D.fromKey "arguments" (parseVector $ parseArgument expressionDecoder) o
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

data Argument a = Argument
  { name       :: Maybe Text
  , expression :: a
  }
  deriving ( Show, Generic, Eq, Hashable )

parseArgument :: (DecompressC r, Typeable a) => D.Decoder (Sem r) a -> D.Decoder (Sem r) (Argument a)
parseArgument inner = D.withCursor . tryParseVal $ \c -> do
  o          <- D.down c
  name       <- D.fromKey "name" (D.maybeOrNull D.text) o
  expression <- D.fromKey "expression" inner o
  pure $ Argument name expression

data ControlOrParser
  = Parser'ControlOrParser TypeParser
  | Control'ControlOrParser TypeControl
  deriving ( Show, Generic, Eq, Hashable )

controlOrParserDecoder :: DecompressC r => D.Decoder (Sem r) ControlOrParser
controlOrParserDecoder = D.withCursor $ \c -> do
  nodeType <- currentNodeType c

  case nodeType of
    "Type_Parser"       -> (_Typed @TypeParser #)  <$> tryDecoder parseTypeParser c
    "Type_Control"      -> (_Typed @TypeControl #) <$> tryDecoder parseTypeControl c
    _ -> throwError . D.ParseFailed $ "invalid node type for ControlOrParser: " <> nodeType

data ConstructorCallExpression = ConstructorCallExpression
  { type_           :: ControlOrParser
  , constructedType :: P4Type
  , arguments       :: [Argument Expression]
  }
  deriving ( Show, Generic, Eq, Hashable )

parseConstructorCallExpression :: DecompressC r => D.Decoder (Sem r) ConstructorCallExpression
parseConstructorCallExpression = D.withCursor . tryParseVal $ \c -> do
  o               <- D.down c
  type_           <- D.fromKey "type" controlOrParserDecoder o
  constructedType <- D.fromKey "constructedType" p4TypeDecoder o
  arguments       <- D.fromKey "arguments" (parseVector $ parseArgument expressionDecoder) o
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

data BoolLiteral = BoolLiteral
  { type_ :: P4Type
  , value :: Bool
  }
  deriving ( Show, Generic, Eq, Hashable )

parseBoolLiteral :: DecompressC r => D.Decoder (Sem r) BoolLiteral
parseBoolLiteral = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  value <- D.fromKey "value" D.bool o
  pure $ BoolLiteral type_ value

data UnaryOpType = UnaryOpLNot
  deriving ( Show, Generic, Eq, Enum, Hashable )

unaryOpTypes :: HashMap Text UnaryOpType
unaryOpTypes = fromList [("LNot", UnaryOpLNot)]

pattern IsUnaryOp :: UnaryOpType -> Text
pattern IsUnaryOp t <- ((unaryOpTypes !?) -> Just t)

data UnaryOp = UnaryOp
  { type_ :: P4Type
  , expr  :: Expression
  , op    :: UnaryOpType
  }
  deriving ( Show, Generic, Eq, Hashable )

parseUnaryOp :: UnaryOpType -> DecompressC r => D.Decoder (Sem r) UnaryOp
parseUnaryOp t = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  expr  <- D.fromKey "expr" expressionDecoder o
  pure $ UnaryOp type_ expr t

data BinaryOpType = BinaryOpAdd
  deriving ( Show, Generic, Eq, Enum, Hashable )

binaryOpTypes :: HashMap Text BinaryOpType
binaryOpTypes = fromList [("Add", BinaryOpAdd)]

pattern IsBinaryOp :: BinaryOpType -> Text
pattern IsBinaryOp t <- ((binaryOpTypes !?) -> Just t)

data BinaryOp = BinaryOp
  { type_ :: P4Type
  , left  :: Expression
  , right :: Expression
  , op    :: BinaryOpType
  }
  deriving ( Show, Generic, Eq, Hashable )

parseBinaryOp :: BinaryOpType -> DecompressC r => D.Decoder (Sem r) BinaryOp
parseBinaryOp t = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" p4TypeDecoder o
  left  <- D.fromKey "left" expressionDecoder o
  right  <- D.fromKey "right" expressionDecoder o
  pure $ BinaryOp type_ left right t

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
