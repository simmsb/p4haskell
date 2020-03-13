module P4Haskell.Types.AST.Expression where

import           Control.Monad.Error.Class          ( throwError )

import           Data.Generics.Sum.Typed

import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DecompressJSON
import           P4Haskell.Types.AST.Path
import           P4Haskell.Types.AST.Types

import           Prelude                            hiding ( Member )

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
  | LNot'Expression LNot
  deriving ( Show, Generic )

expressionDecoder :: DecompressC r => D.Decoder (Sem r) Expression
expressionDecoder = D.withCursor $ \c -> do
  o <- D.down c
  nodeType <- D.fromKey "Node_Type" D.text o

  case nodeType of
    "MethodCallExpression"      -> (_Typed @MethodCallExpression #) <$> D.focus parseMethodCallExpression c
    "Member"                    -> (_Typed @Member #) <$> D.focus parseMember c
    "Argument"                  -> (_Typed @Argument #) <$> D.focus parseArgument c
    "ConstructorCallExpression" -> (_Typed @ConstructorCallExpression #) <$> D.focus parseConstructorCallExpression c
    "Constant"                  -> (_Typed @Constant #) <$> D.focus parseConstant c
    "PathExpression"            -> (_Typed @PathExpression #) <$> D.focus parsePathExpression c
    "BoolLiteral"               -> (_Typed @BoolLiteral #) <$> D.focus parseBoolLiteral c
    "LNot"                      -> (_Typed @LNot #) <$> D.focus parseLNot c
    _ -> throwError . D.ParseFailed $ "invalid node type for Expression: " <> nodeType


data MethodCallExpression = MethodCallExpression
  { type_         :: P4Type
  , method        :: Expression
  , typeArguments :: [P4Type]
  , arguments     :: [Argument]
  }
  deriving ( Show, Generic )

parseMethodCallExpression :: DecompressC r => D.Decoder (Sem r) MethodCallExpression
parseMethodCallExpression = D.withCursor . tryParseVal $ \c -> do
  o             <- D.down c
  type_         <- D.fromKey "type" parseP4Type o
  method        <- D.fromKey "method" expressionDecoder o
  typeArguments <- D.fromKey "typeArguments" (parseVector parseP4Type) o
  arguments     <- D.fromKey "arguments" (parseVector parseArgument) o
  pure $ MethodCallExpression type_ method typeArguments arguments

data Member = Member
  { type_ :: P4Type
  , expr  :: Expression
  , member :: Text
  }
  deriving ( Show, Generic )

parseMember :: DecompressC r => D.Decoder (Sem r) Member
parseMember = D.withCursor . tryParseVal $ \c -> do
  o      <- D.down c
  type_  <- D.fromKey "type" parseP4Type o
  expr   <- D.fromKey "expr" expressionDecoder o
  member <- D.fromKey "member" D.text o
  pure $ Member type_ expr member

data Argument = Argument
  { name       :: Maybe Text
  , expression :: Expression
  }
  deriving ( Show, Generic )

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
  deriving ( Show, Generic )

parseConstructorCallExpression :: DecompressC r => D.Decoder (Sem r) ConstructorCallExpression
parseConstructorCallExpression = D.withCursor . tryParseVal $ \c -> do
  o               <- D.down c
  type_           <- D.fromKey "type" parseP4Type o
  constructedType <- D.fromKey "constructedType" parseP4Type o
  arguments       <- D.fromKey "arguments" (parseVector parseArgument) o
  pure $ ConstructorCallExpression type_ constructedType arguments

data Constant = Constant
  { type_ :: P4Type
  , value :: Int
  , base  :: Int
  }
  deriving ( Show, Generic )

parseConstant :: DecompressC r => D.Decoder (Sem r) Constant
parseConstant = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" parseP4Type o
  value <- D.fromKey "value" D.int o
  base  <- D.fromKey "base" D.int o
  pure $ Constant type_ value base

data PathExpression = PathExpression
  { type_ :: P4Type
  , path  :: Path
  }
  deriving ( Show, Generic )

parsePathExpression :: DecompressC r => D.Decoder (Sem r) PathExpression
parsePathExpression = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" parseP4Type o
  path  <- D.fromKey "path" parsePath o
  pure $ PathExpression type_ path

newtype BoolLiteral = BoolLiteral
  { value :: Bool
  }
  deriving ( Show, Generic )

parseBoolLiteral :: DecompressC r => D.Decoder (Sem r) BoolLiteral
parseBoolLiteral = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  value <- D.fromKey "value" D.bool o
  pure $ BoolLiteral value

data LNot = LNot
  { type_ :: P4Type
  , expr  :: Expression
  }
  deriving ( Show, Generic )

parseLNot :: DecompressC r => D.Decoder (Sem r) LNot
parseLNot = D.withCursor . tryParseVal $ \c -> do
  o     <- D.down c
  type_ <- D.fromKey "type" parseP4Type o
  expr  <- D.fromKey "expr" expressionDecoder o
  pure $ LNot type_ expr
