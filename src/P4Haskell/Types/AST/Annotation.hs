-- | P4 Annotations
module P4Haskell.Types.AST.Annotation
    ( Annotation(..)
    , parseAnnotations
    , parseAnnotation
    , AnnotatedToken
    , AnnotationExpression
    , NamedExpression
    , parseAnnotatedToken
    , parseExpression
    , parseNamedExpression ) where

import           P4Haskell.Types.AST.Core
import           P4Haskell.Types.AST.DecompressJSON

import           Prelude                            hiding ( Member )

import           Waargonaut
import qualified Waargonaut.Decode                  as D

data Annotation = Annotation
  { name         :: Text
  , body         :: [AnnotatedToken]
  , needsParsing :: Bool
  , expr         :: [AnnotationExpression]
  , kv           :: [NamedExpression]
  }
  deriving ( Show, Generic )

parseAnnotations :: DecompressC r => D.Decoder (Sem r) [Annotation]
parseAnnotations = D.withCursor . tryParseVal $ \c -> do
  o <- D.down c
  D.fromKey "annotations" (parseVector parseAnnotation) o

parseAnnotation :: DecompressC r => D.Decoder (Sem r) Annotation
parseAnnotation = D.withCursor . tryParseVal $ \c -> do
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

type AnnotationExpression = Json

parseExpression :: Monad m => D.Decoder m Json
parseExpression = D.json

type NamedExpression = Json

parseNamedExpression :: Monad m => D.Decoder m Json
parseNamedExpression = D.json
